program CtrEvents;

{$APPTYPE CONSOLE}


uses
  Windows, SysUtils, SyncObjs,
  ZBase, ZGuard, ZPort, ZGClasses, Utils;

const
  CvtPortType = ZP_PORT_COM; // Тип порта
  CvtPortName: WideString = 'COM49'; // Имя порта
  CtrAddr = 2; // Адрес контроллера
  KeyShow = True; // True, показывать номера ключей для событий прохода (иначе только позиции в списке ключей)
var
  g_hCtr: THandle;  // Контроллер
  g_nCtrMaxEvents: Integer;
  g_fWiegand: Boolean;  // True - Wiegand, иначе - Dallas
  g_nCtrFlags: Cardinal;
  g_fCtrNotifyEnabled: Boolean;
  g_nAppReadEventIdx: Integer;
  g_oEvent: TEvent = nil;
  g_fThreadActive: Boolean;
  g_hThread: THandle = 0;


procedure ShowEvents(AStart, ACount: Integer);
var
  aEvents: array[0..5] of TZG_CTR_EVENT;
  pEv: PZG_CTR_EVENT;
  i, j, nIdx, nCnt, nKeyIdx, nKeyBank, n: Integer;
  rTime: TZG_EV_TIME;
  rKeyNum: TZ_KEYNUM;
  nEcSubEv: TZG_EC_SUB_EV;
  nFireSubEv: TZG_FIRE_SUB_EV;
  nSecurSubEv: TZG_SECUR_SUB_EV;
  nFlags: Cardinal;
  nDirect: TZG_CTR_DIRECT;
  rKey: TZG_Ctr_Key;
  nMode: TZG_CTR_MODE;
  nModeSubEv: TZG_MODE_SUB_EV;
  nHMode: TZG_Hotel_Mode;
  nHSubEv: TZG_Hotel_Sub_Ev;
begin
  i := 0;
  while i < ACount do
  begin
    nIdx := (AStart + i) mod g_nCtrMaxEvents;
    nCnt := (ACount - i);
    if nCnt > Length(aEvents) then
      nCnt := Length(aEvents);
    if (nIdx + nCnt) > g_nCtrMaxEvents then
       nCnt := (g_nCtrMaxEvents - nIdx);

    CheckZGError(ZG_Ctr_ReadEvents(g_hCtr, nIdx, @aEvents, nCnt, nil, nil));
    for j := 0 to nCnt - 1 do
    begin
      pEv := @aEvents[j];
      case pEv.nType of
        ZG_EV_ELECTRO_ON,
        ZG_EV_ELECTRO_OFF:
        begin
          ZG_Ctr_DecodeEcEvent(g_hCtr, @pEv.aData, rTime, nEcSubEv, nFlags);
          Writeln(format('%.4d. %.2d.%.2d %.2d:%.2d:%.2d %s Sub_event: %s, Power flags: %d', [
              nIdx + j,
              rTime.nDay, rTime.nMonth,
              rTime.nHour, rTime.nMinute, rTime.nSecond,
              EvTypeStrs[pEv.nType],
              EcSubEvStrs[nEcSubEv], nFlags]));
        end;
        ZG_EV_FIRE_STATE:
        begin
          ZG_Ctr_DecodeFireEvent(g_hCtr, @pEv.aData, rTime, nFireSubEv, nFlags);
          Writeln(format('%.4d. %.2d.%.2d %.2d:%.2d:%.2d %s Sub_event: %s, Fire flags: %d', [
              nIdx + j,
              rTime.nDay, rTime.nMonth,
              rTime.nHour, rTime.nMinute, rTime.nSecond,
              EvTypeStrs[pEv.nType],
              FireSubEvStrs[nFireSubEv], nFlags]));
        end;
        ZG_EV_SECUR_STATE:
        begin
          ZG_Ctr_DecodeSecurEvent(g_hCtr, @pEv.aData, rTime, nSecurSubEv, nFlags);
          Writeln(format('%.4d. %.2d.%.2d %.2d:%.2d:%.2d %s Sub_event: %s, Security flags: %.2Xh', [
              nIdx + j,
              rTime.nDay, rTime.nMonth,
              rTime.nHour, rTime.nMinute, rTime.nSecond,
              EvTypeStrs[pEv.nType],
              SecurSubEvStrs[nSecurSubEv], nFlags]));
        end;
        ZG_EV_MODE_STATE:
        begin
          ZG_Ctr_DecodeModeEvent(g_hCtr, @pEv.aData, rTime, nMode, nModeSubEv);
          Writeln(format('%.4d. %.2d.%.2d %.2d:%.2d:%.2d %s Mode: %s, Sub_event: %s', [
              nIdx + j,
              rTime.nDay, rTime.nMonth,
              rTime.nHour, rTime.nMinute, rTime.nSecond,
              EvTypeStrs[pEv.nType],
              ModeStrs[nMode],
              ModeSubEvStrs[nModeSubEv]]));
        end;
        ZG_EV_UNKNOWN_KEY:
        begin
          ZG_Ctr_DecodeUnkKeyEvent(g_hCtr, @pEv.aData, rKeyNum);
          Writeln(format('%.4d. Key "%s"', [
              nIdx + j,
              ZKeyNumToStr(rKeyNum, g_fWiegand)]));
        end;
        ZG_EV_HOTEL40..ZG_EV_HOTEL41:
        begin
          ZG_DecodeHotelEvent(@pEv.aData, rTime, nHMode, nHSubEv, nFlags);
          Writeln(format('%.4d. %.2d.%.2d %.2d:%.2d:%.2d %s Mode: %s, Sub_event: %s, flags: %.2Xh', [
              nIdx + j,
              rTime.nDay, rTime.nMonth,
              rTime.nHour, rTime.nMinute, rTime.nSecond,
              EvTypeStrs[pEv.nType],
              HModeStrs[nHMode],
              HotelSubEvStrs[nHSubEv],
              nFlags]));
        end;
        else
        begin
          ZG_Ctr_DecodePassEvent(g_hCtr, @pEv.aData, rTime, nDirect, nKeyIdx, nKeyBank);
          Writeln(format('%.4d. %.2d.%.2d %.2d:%.2d:%.2d %s %s (key_idx: %d, bank#: %d)', [
              nIdx + j,
              rTime.nDay, rTime.nMonth,
              rTime.nHour, rTime.nMinute, rTime.nSecond,
              DirectStrs[nDirect],
              EvTypeStrs[pEv.nType],
              nKeyIdx, nKeyBank]));
          if KeyShow then
          begin
            CheckZGError(ZG_Ctr_ReadKeys(g_hCtr, nKeyIdx, @rKey, 1, nil, nil, nKeyBank));
            if rKey.fErased then
              Writeln('Key eraised.')
            else
            begin
              Writeln(format('%d,%d', [rKey.rNum[3], PWord(@rKey.rNum[1])^]));
              n := 0;
              Move(rKey.rNum[1], n, 3);
              Writeln(format('%.10d', [n]));
            end;
          end;
        end;
      end;
    end;
    Inc(i, nCnt);
  end;
end;

function CheckNotifyMsgs(): HResult;
var
  nMsg: Cardinal;
  nMsgParam: NativeInt;
  pInfo: PZG_N_NEW_EVENT_INFO;
begin
  repeat
    Result := ZG_Ctr_GetNextMessage(g_hCtr, nMsg, nMsgParam);
    if Result <> S_OK then
      break;
    case nMsg of
      ZG_N_CTR_NEW_EVENT:
      begin
        pInfo := PZG_N_NEW_EVENT_INFO(nMsgParam);
        Writeln(format('==> New Events: %d', [pInfo.nNewCount]));
        ShowEvents(pInfo.nReadIdx, pInfo.nNewCount);
        g_nAppReadEventIdx := pInfo.nWriteIdx;
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

procedure DoShowNewEvents();
var
  nWrIdx, nRdIdx, nNewCount, nShowCount: Integer;
begin
  CheckZGError(ZG_Ctr_ReadEventIdxs(g_hCtr, nWrIdx, nRdIdx));
  if nWrIdx >= g_nAppReadEventIdx then
    nNewCount := (nWrIdx - g_nAppReadEventIdx)
  else
    nNewCount := (g_nCtrMaxEvents - g_nAppReadEventIdx + nWrIdx);
  if nNewCount = 0 then
    Writeln('No new events')
  else
    Writeln(format('Available %d new events:', [nNewCount]));
  while nNewCount > 0 do
  begin
    nShowCount := 25;
    if nShowCount > nNewCount then
      nShowCount := nNewCount;
    ShowEvents(g_nAppReadEventIdx, nShowCount);
    Dec(nNewCount, nShowCount);
    g_nAppReadEventIdx := (g_nAppReadEventIdx + nShowCount) mod g_nCtrMaxEvents;
    Writeln('Press any key to continue...');
    Read;
  end;
  Writeln('Done.');
end;

procedure DoShowAllEvents();
var
  nWrIdx, nRdIdx, nIdx, nTotalCount, nShowCount: Integer;
begin
  CheckZGError(ZG_Ctr_ReadEventIdxs(g_hCtr, nWrIdx, nRdIdx));
  nIdx := nWrIdx;
  nTotalCount := g_nCtrMaxEvents;
  while nTotalCount > 0 do
	begin
    nShowCount := 25;
    if nShowCount > nTotalCount then
      nShowCount := nTotalCount;
    ShowEvents(nIdx, nShowCount);
    Dec(nTotalCount, nShowCount);
    nIdx := (nIdx + nShowCount) mod g_nCtrMaxEvents;
    Writeln('Press any key to continue...');
    Read;
  end;
  Writeln('Done.');
end;

procedure EnableNotification(AEnable: Boolean=True; AReport: Boolean=True);
var
  rNS: TZG_CTR_NOTIFY_SETTINGS;
begin
	if AEnable then
	begin
    if g_oEvent = nil then
      g_oEvent := TEvent.Create(nil, True, False, '');
		rNS.nNMask := ZG_NF_CTR_NEW_EVENT;
		rNS.hEvent := g_oEvent.Handle;
		rNS.nReadEvIdx := g_nAppReadEventIdx;
		rNS.nCheckStatePeriod := 300;
		CheckZGError(ZG_Ctr_SetNotification(g_hCtr, @rNS));
    StartNotifyThread();
  end
  else
  begin
    StopNotifyThread();
		CheckZGError(ZG_Ctr_SetNotification(g_hCtr, nil));
  end;
  g_fCtrNotifyEnabled := AEnable;
  if AReport then
    Writeln('Done.');
end;

procedure DoRestoreFactorySettings();
begin
  Writeln('Writing (0, 0)...');
  CheckZGError(ZG_Ctr_WriteEventIdxs(g_hCtr, $3, 0, 0));
  g_nAppReadEventIdx := 0;
  Writeln('Done.');
end;

procedure DoTest();
Const
  EnableStrs: array[Boolean] of String = ('Disable', 'Enable');
var
  hCvt: THandle;  // Конвертер
  rOp: TZG_CVT_OPEN_PARAMS;
  rCtrInfo: TZG_CTR_INFO;
  nWrIdx, nRdIdx: Integer;
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
    rOp.nSpeed := ZG_SPEED_57600;
    CheckZGError(ZG_Cvt_Open(hCvt, @rOp));
    FillChar(rCtrInfo, SizeOf(rCtrInfo), 0);
    CheckZGError(ZG_Ctr_Open(g_hCtr, hCvt, CtrAddr, 0, @rCtrInfo));
    g_nCtrMaxEvents := rCtrInfo.nMaxEvents;
    g_fWiegand := (rCtrInfo.nFlags and ZG_CTR_F_PROXIMITY) <> 0;
    g_nCtrFlags := rCtrInfo.nFlags;
    Writeln(format('%s addr: %d, sn: %d, v%d.%d, Max_Events: %d, Key_Type: %s', [
        CtrTypeStrs[rCtrInfo.nType],
        rCtrInfo.nAddr,
        rCtrInfo.nSn,
        LoByte(rCtrInfo.nVersion), HiByte(rCtrInfo.nVersion),
        rCtrInfo.nMaxEvents,
        RdModeStrs[g_fWiegand]]));
    g_fCtrNotifyEnabled := False;
    CheckZGError(ZG_Ctr_ReadEventIdxs(g_hCtr, nWrIdx, nRdIdx));
    g_nAppReadEventIdx := nWrIdx;
    EnableNotification(True, False);
    Writeln('-----');
    repeat
      Writeln('Enter command number:');
      Writeln('1 - Show new events');
      Writeln(format('2 - Show all events (%d)', [g_nCtrMaxEvents]));
      Writeln(format('3 - %s notifications for new events', [EnableStrs[not g_fCtrNotifyEnabled]]));
      Writeln('9 - Restore factory settings (reset indexs)');
      Writeln('0 - quit');
      ReadLn(s);
      Writeln;
      case StrToIntDef(s, 0) of
        1: DoShowNewEvents();
        2: DoShowAllEvents();
        3: EnableNotification(not g_fCtrNotifyEnabled);
        9: DoRestoreFactorySettings();
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
