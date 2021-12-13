program CtrKeys;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, Classes, SyncObjs,
  ZBase, ZGuard, ZPort, ZGClasses, Utils;

const
  CvtPortType = ZP_PORT_COM;
  CvtPortName: WideString = 'com49';
  CtrAddr = 2;

var
  g_hCtr: THandle;  // Контроллер
  g_nCtrMaxBanks: Integer;
  g_fWiegand: Boolean; // True - Wiegand, иначе - Dallas
  g_nFoundKeyIdx: Integer;
  g_oEvent: TEvent = nil;
  g_fThreadActive: Boolean;
  g_hThread: THandle = 0;


procedure ShowKeys();
var
  i, j, nTop, nCount: Integer;
  aKeys: array[0..5] of TZG_CTR_KEY;
  pKey: PZG_CTR_KEY;
begin
  for i := 0 to g_nCtrMaxBanks - 1 do
  begin
    Writeln('------------');
    Writeln(format('Bank %d:', [i]));
    CheckZGError(ZG_Ctr_GetKeyTopIndex(g_hCtr, nTop, i));
    if nTop = 0 then
    begin
      Writeln('List is empty.');
      continue;
    end;
    for j := 0 to nTop - 1 do
    begin
      if (j mod Length(aKeys)) = 0 then
      begin
        nCount := (nTop - j);
        if nCount > Length(aKeys) then
          nCount := Length(aKeys);
        CheckZGError(ZG_Ctr_ReadKeys(g_hCtr, j, @aKeys, nCount, nil, nil, i));
      end;
      pKey := @aKeys[j mod Length(aKeys)];
      if pKey.fErased then
        Writeln(format('%.4d empty', [j]))
      else
      begin
        Writeln(format('%.4d %s, %s, access: %.2Xh', [
          j,
          ZKeyNumToStr(pKey.rNum, g_fWiegand),
          KeyTypeStrs[pKey.nType],
          pKey.nAccess]));
      end;
    end;
  end;
  Writeln('Done.');
end;

function TryHexToDec(AHex: Char; var VDec: Integer): Boolean;
begin
  case AHex of
    '0'..'9': VDec := Ord(AHex) - Ord('0');
    'a'..'f': VDec := Ord(AHex) - Ord('a') + 10;
    'A'..'F': VDec := Ord(AHex) - Ord('A') + 10;
    else
    begin
      Result := False;
      exit;
    end;
  end;
  Result := True;
end;

function ParseKeyNum(var VKeyNum: TZ_KEYNUM; Const AText: String): Boolean;
var
  nGroup, nNumber, n, n2, i, j: Integer;
begin
  FillChar(VKeyNum, SizeOf(VKeyNum), 0);
  n := Pos(',', AText);
  if n <> 0 then
  begin
    if not (TryStrToInt(Copy(AText, 1, n - 1), nGroup) and
        TryStrToInt(Copy(AText, n + 1, Length(AText) - n), nNumber)) then
    begin
      Result := False;
      Exit;
    end;
    PWord(@VKeyNum[1])^ := nNumber;
    VKeyNum[3] := nGroup;
    VKeyNum[0] := 3;
    n := Pos('[', AText);
    if n <> 0 then
    begin
      n2 := Pos(']', AText);
      if (n2 <> 0) and TryStrToInt('$' + Copy(AText, n + 1, n2 - n - 1), n) then
      begin
        PWord(@VKeyNum[4])^ := n and $FFFF;
        VKeyNum[0] := 5;
      end;
    end;
  end
  else
  begin
    i := Length(AText) - 1;
    j := 1;
    while i >= 1 do
    begin
      if not (TryHexToDec(AText[i], n) and TryHexToDec(AText[i + 1], n2)) then
      begin
        Result := False;
        Exit;
      end;
      VKeyNum[j] := (n2 and $F) or ((n and $F) shl 4);
      Inc(j);
      if j > 6 then
        break;
      Dec(i, 2);
    end;
    VKeyNum[0] := (j - 1);
  end;
  Result := True;
end;

function FindKeyEnum(AIdx: Integer; AKey: PZG_CTR_KEY; APos: Integer; AMax: Integer; AUserData: Pointer): LongBool; stdcall;
var
  pFindKey: PZ_KEYNUM;
  fFound: Boolean;
begin
  pFindKey := PZ_KEYNUM(AUserData);
  if (pFindKey^)[0] < 6 then
    fFound := CompareMem(@(pFindKey^)[1], @AKey.rNum[1], (pFindKey^)[0])
  else
    fFound := CompareMem(@(pFindKey^)[1], @AKey.rNum[1], 6);
  if fFound then
  begin
    g_nFoundKeyIdx := AIdx;
    Result := False;
    Exit;
  end;
  Result := True;
end;

procedure DoFindKeyByNumber();
var
  s: String;
  rFindNum: TZ_KEYNUM;
  oL : TStringList;
  nBankN: Integer;
begin
  Writeln('Enter bank#, key number (-1 last key):');
  Readln(s);
  oL := TStringList.Create();
  try
    oL.Delimiter := ',';
    oL.StrictDelimiter := True;
    oL.DelimitedText := s;
    if (oL.Count < 2) or
        (not TryStrToInt(oL[0], nBankN)) then
    begin
      Writeln('Incorrect entry');
      exit;
    end;
    if oL[1] = '-1' then
      CheckZGError(ZG_Ctr_ReadLastKeyNum(g_hCtr, rFindNum))
    else if not ParseKeyNum(rFindNum, oL[1]) then
    begin
      Writeln('Incorrect entry.');
      exit;
    end;
    g_nFoundKeyIdx := -1;
    CheckZGError(ZG_Ctr_EnumKeys(g_hCtr, 0, FindKeyEnum, @rFindNum, nBankN));
    if g_nFoundKeyIdx <> -1 then
      Writeln(format('Key %s found (index=%d).\n', [
          ZKeyNumToStr(rFindNum, g_fWiegand), g_nFoundKeyIdx]))
    else
      Writeln(format('"Key %s not found..\n', [ZKeyNumToStr(rFindNum, g_fWiegand)]));
  finally
    oL.Free();
  end;
end;

procedure DoSetKey();
var
  s: String;
  oL : TStringList;
  nBankN, nKeyIdx, nKeyType, nKeyAccess: Integer;
  rKey: TZG_CTR_KEY;
begin
  Writeln('Enter bank#, key index (-1 top), number (-1 last key), type ' +
      '(1-normal,2-blocking,3-Master), access (hex):');
  Readln(s);
  oL := TStringList.Create();
  try
    oL.Delimiter := ',';
    oL.StrictDelimiter := True;
    oL.DelimitedText := s;
    if (oL.Count < 5) or
        (not TryStrToInt(oL[0], nBankN)) or
        (not TryStrToInt(oL[1], nKeyIdx)) or
        (not TryStrToInt(oL[3], nKeyType)) or
        (not TryStrToInt(oL[4], nKeyAccess)) then
    begin
      Writeln('Incorrect entry');
      exit;
    end;
    FillChar(rKey, SizeOf(rKey), 0);
    if nKeyIdx = -1 then
      CheckZGError(ZG_Ctr_GetKeyTopIndex(g_hCtr, nKeyIdx, nBankN));
    rKey.nType := TZG_CTR_KEY_TYPE(nKeyType);
    rKey.nAccess := nKeyAccess;
    if oL[2] = '-1' then
      CheckZGError(ZG_Ctr_ReadLastKeyNum(g_hCtr, rKey.rNum))
    else if not ParseKeyNum(rKey.rNum, oL[2]) then
    begin
      Writeln('Incorrect entry.');
      exit;
    end;
  finally
    oL.Free();
  end;
  CheckZGError(ZG_Ctr_WriteKeys(g_hCtr, nKeyIdx, @rKey, 1, nil, nil, nBankN));
  Writeln('Done.');
end;

procedure DoClearKey();
var
  s: String;
  oL : TStringList;
  nBankN, nKeyIdx, nTop: Integer;
begin
  Writeln('Enter bank#, key index (-1 key in tail):');
  Readln(s);
  oL := TStringList.Create();
  try
    oL.Delimiter := ',';
    oL.StrictDelimiter := True;
    oL.DelimitedText := s;
    if (oL.Count < 2) or
        (not TryStrToInt(oL[0], nBankN)) or
        (not TryStrToInt(oL[1], nKeyIdx)) then
    begin
      Writeln('Incorrect entry');
      exit;
    end;
  finally
    oL.Free();
  end;
  if nKeyIdx = -1 then
  begin
    CheckZGError(ZG_Ctr_GetKeyTopIndex(g_hCtr, nTop, nBankN));
    if nTop = 0 then
    begin
      Writeln('Key list is empty.');
      Readln;
      exit;
    end;
    nKeyIdx := (nTop - 1);
  end;
  CheckZGError(ZG_Ctr_ClearKeys(g_hCtr, nKeyIdx, 1, nil, nil, nBankN));
  Writeln('Done.');
end;

procedure DoClearAllKeys();
var
  nBankN, nTop: Integer;
  s: String;
begin
  Writeln('Enter bank#:');
  Readln(s);
  if not TryStrToInt(s, nBankN) then
  begin
    Writeln('Incorrect entry.');
    exit;
  end;
  CheckZGError(ZG_Ctr_GetKeyTopIndex(g_hCtr, nTop, nBankN));
  if nTop = 0 then
  begin
    Writeln('Key list is empty.');
    Readln;
    exit;
  end;
  Writeln('Clearing...');
  CheckZGError(ZG_Ctr_ClearKeys(g_hCtr, 0, nTop, nil, nil, nBankN));
  Writeln('Done.');
end;

function CheckNotifyMsgs(): HResult;
var
  nMsg: Cardinal;
  nMsgParam: NativeInt;
  pInfo: PZg_N_Key_Top_Info;
begin
  repeat
    Result := ZG_Ctr_GetNextMessage(g_hCtr, nMsg, nMsgParam);
    if Result <> S_OK then
      break;
    case nMsg of
      ZG_N_CTR_KEY_TOP:
      begin
        pInfo := PZg_N_Key_Top_Info(nMsgParam);
        Writeln(format('==> Bank#%d: top index of key changed (%d -> %d).', [
            pInfo.nBankN, pInfo.nOldTopIdx, pInfo.nNewTopIdx]));
      end;
    end;
  until False;
  if Result = ZP_S_NOTFOUND then
    Result := S_OK;
end;

function NotifyThreadProc(AParameter: Pointer): Cardinal; stdcall;
begin
  while g_fThreadActive do
		if g_oEvent.WaitFor() = wrSignaled then
    begin
			g_oEvent.ResetEvent();
      if g_hCtr <> 0 then
        CheckNotifyMsgs();
    end;
  Result := 0;
end;

procedure StartNotifyThread();
var
  nThreadId: Cardinal;
begin
  if g_hThread <> 0 then
    Exit;
  g_fThreadActive := True;
  g_hThread := CreateThread(nil, 0, @NotifyThreadProc, nil, 0, nThreadId);
end;

procedure StopNotifyThread();
begin
  if g_hThread = 0 then
    Exit;
  g_fThreadActive := False;
  g_oEvent.SetEvent();
  WaitForSingleObject(g_hThread, INFINITE);
  CloseHandle(g_hThread);
end;

procedure DoTest();
var
  hCvt: THandle;  // Конвертер
  rOp: TZG_CVT_OPEN_PARAMS;
  rCtrInfo: TZG_CTR_INFO;
  rNS: TZG_Ctr_Notify_Settings;
  s: String;
begin
  SetConsoleOutputCP(1251); // Переключение консоли на кодовую страницу CP1251 (Windows-1251).

  if not CheckZGVersion() then
  begin
    Writeln('Wrong version SDK Guard.');
    Readln;
    exit;
  end;
  CheckZGError(ZG_Initialize(ZP_IF_NO_MSG_LOOP));
  hCvt := 0;
  g_hCtr := 0;
  try
    FillCHar(rOp, SizeOf(rOp), 0);
    rOp.nType := CvtPortType;
    rOp.pszName := PWideChar(CvtPortName);
    rOp.nSpeed := ZG_SPEED_19200;
    CheckZGError(ZG_Cvt_Open(hCvt, @rOp));
    FillChar(rCtrInfo, SizeOf(rCtrInfo), 0);
    CheckZGError(ZG_Ctr_Open(g_hCtr, hCvt, CtrAddr, 0, @rCtrInfo));
    if (rCtrInfo.nFlags and ZG_CTR_F_2BANKS) <> 0 then
      g_nCtrMaxBanks := 2
    else
      g_nCtrMaxBanks := 1;
    g_fWiegand := (rCtrInfo.nFlags and ZG_CTR_F_PROXIMITY) <> 0;
    Writeln(format('%s addr: %d, s/n: %d, v%d.%d, Max_Banks: %d, Key_Mode: %s', [
        CtrTypeStrs[rCtrInfo.nType],
        rCtrInfo.nAddr,
        rCtrInfo.nSn,
        LoByte(rCtrInfo.nVersion), HiByte(rCtrInfo.nVersion),
        g_nCtrMaxBanks,
        RdModeStrs[g_fWiegand]]));

    g_oEvent := TEvent.Create(nil, True, False, '');
    FillChar(rNS, SizeOf(rNS), 0);
    rNS.nNMask := ZG_NF_CTR_KEY_TOP;
    rNS.hEvent := g_oEvent.Handle;
    rNS.nCheckStatePeriod := 3000;  // Период проверки верхней границы ключей
    CheckZGError(ZG_Ctr_SetNotification(g_hCtr, @rNS));
    StartNotifyThread();
    Writeln('-----');
    repeat
      Writeln('Enter command number:');
      Writeln('1 - show keys');
      Writeln('2 - find key by number...');
      Writeln('6 - set key...');
      Writeln('7 - clear key...');
      Writeln('8 - clear all keys...');
      Writeln('0 - quit');
      ReadLn(s);
      Writeln;
      case StrToIntDef(s, 0) of
        1: ShowKeys();
        2: DoFindKeyByNumber();
        6: DoSetKey();
        7: DoClearKey();
        8: DoClearAllKeys();
        0: break;
        else Writeln('Invalid command.');
      end;
      Writeln('-----');
    until False;
  finally
    StopNotifyThread();
    if g_hCtr <> 0 then
      ZG_CloseHandle(g_hCtr);
    if hCvt <> 0 then
      ZG_CloseHandle(hCvt);
    if g_oEvent <> nil then
      g_oEvent.Free();
    ZG_Finalyze();
  end;
end;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
    DoTest();
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;
end.
