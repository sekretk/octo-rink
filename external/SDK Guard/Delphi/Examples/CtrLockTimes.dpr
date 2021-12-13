program CtrLockTimes;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, Classes,
  ZBase, ZGuard, ZPort, ZGClasses, Utils;

const
  CvtPortType = ZP_PORT_FT;
  CvtPortName: WideString = 'RF007i2n';
  CtrAddr = 3;
var
  g_hCtr: THandle;  // Контроллер
  g_nCtrMaxBanks: Integer; // Максимум банков в контроллере

procedure ShowLockTimes();
var
  nOpen, nLet, nMax: Cardinal;
  i: Integer;
begin
  for i := 0 to g_nCtrMaxBanks - 1 do
  begin
    CheckZGError(ZG_Ctr_ReadLockTimes(g_hCtr, @nOpen, @nLet, @nMax, i));
    Writeln('------------');
    Writeln(format('Bank %d:', [i]));
    Writeln(format('Open (ms): %d', [nOpen]));
    Writeln(format('Let (ms): %d', [nLet]));
    Writeln(format('Max (ms): %d', [nMax]));
  end;
  Writeln('Done.');
end;

procedure DoSetLockTimes();
var
  nBankN, nOpen, nLet, nMax, nMask: Integer;
  s: String;
  oL : TStringList;
begin
  Writeln('Enter bank#, open time (ms), let time (ms), max time (ms) (-1 not change):');
  ReadLn(s);
  if s = '' then
  begin
    WriteLn('Cancelled.');
    exit;
  end;
  oL := TStringList.Create();
  try
    oL.Delimiter := ',';
    oL.StrictDelimiter := True;
    oL.DelimitedText := s;
    if (oL.Count < 4) or
        (not TryStrToInt(oL[0], nBankN)) or
        (not TryStrToInt(oL[1], nOpen)) or
        (not TryStrToInt(oL[2], nLet)) or
        (not TryStrToInt(oL[3], nMax)) then
    begin
      Writeln('Incorrect entry');
      exit;
    end;
    nMask := 0;
    if nOpen <> -1 then
      nMask := 1;
    if nLet <> -1 then
      nMask := nMask or 2;
    if nMax <> -1 then
      nMask := nMask or 4;
    if nMask = 0 then
    begin
      WriteLn('Do noting.');
      exit;
    end;
    WriteLn('Writing...');
    CheckZGError(ZG_Ctr_WriteLockTimes(g_hCtr, nMask, nOpen, nLet, nMax, nBankN));
    WriteLn('Done.');
  finally
    oL.Free();
  end;
end;

procedure DoOpenLock(ALockN: Integer);
begin
  CheckZGError(ZG_Ctr_OpenLock(g_hCtr, ALockN));
  WriteLn('Done.');
end;

procedure DoRestoreFactorySettings();
var
  i: Integer;
begin
  WriteLn('Writing (3000, 0, 0)...');
  for i := 0 to g_nCtrMaxBanks - 1 do
    CheckZGError(ZG_Ctr_WriteLockTimes(g_hCtr, $7, 3000, 0, 0, i));
  Writeln('Done.');
end;

procedure DoTest();
var
  hCvt: THandle;
  rOp: TZG_CVT_OPEN_PARAMS;
  rCtrInfo: TZG_CTR_INFO;
  s: String;
begin
  SetConsoleOutputCP(1251); // Переключение консоли на кодовую страницу CP1251 (Windows-1251).

  if not CheckZGVersion() then
  begin
    Writeln('Wrong version SDK Guard.');
    Readln;
    exit;
  end;
  hCvt := 0;
  g_hCtr := 0;
  CheckZGError(ZG_Initialize(ZP_IF_NO_MSG_LOOP));
  try
    FillCHar(rOp, SizeOf(rOp), 0);
    rOp.nType := CvtPortType;
    rOp.pszName := PWideChar(CvtPortName);
    rOp.nSpeed := ZG_SPEED_57600;
    CheckZGError(ZG_Cvt_Open(hCvt, @rOp));
    FillChar(rCtrInfo, SizeOf(rCtrInfo), 0);
    CheckZGError(ZG_Ctr_Open(g_hCtr, hCvt, CtrAddr, 0, @rCtrInfo));
    if (rCtrInfo.nFlags and ZG_CTR_F_2BANKS) <> 0 then
      g_nCtrMaxBanks := 2
    else
      g_nCtrMaxBanks := 1;
    Writeln(format('%s addr: %d, sn: %d, v%d.%d, Max_Banks: %d.', [
        CtrTypeStrs[rCtrInfo.nType],
        rCtrInfo.nAddr,
        rCtrInfo.nSn,
        LoByte(rCtrInfo.nVersion), HiByte(rCtrInfo.nVersion),
        g_nCtrMaxBanks]));
    Writeln('-----');
    repeat
      Writeln('Enter command number:');
      Writeln('1 - show lock times');
      Writeln('2 - set lock times...');
      Writeln('3 - open lock (In)');
      Writeln('4 - open lock (Out)');
      Writeln('9 - Restore factory settings (all banks)');
      Writeln('0 - quit');
      ReadLn(s);
      Writeln;
      case StrToIntDef(s, 0) of
        1: ShowLockTimes();
        2: DoSetLockTimes();
        3: DoOpenLock(0);
        4: DoOpenLock(1);
        9: DoRestoreFactorySettings();
        0: break;
        else Writeln('Invalid command.');
      end;
      Writeln('-----');
    until False;
  finally
    if g_hCtr <> 0 then
      ZG_CloseHandle(g_hCtr);
    if hCvt <> 0 then
      ZG_CloseHandle(hCvt);
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
