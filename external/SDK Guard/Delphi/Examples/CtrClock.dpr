program CtrClock;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, DateUtils, SyncObjs,
  ZBase, ZGuard, ZPort, ZGClasses, Utils;

const
  CvtPortType = ZP_PORT_FT;
  CvtPortName: WideString = 'RF007i2n';
  CtrAddr = 3;
var
  g_hCtr: THandle;  // Дескриптор контроллера
  g_oEvent: TEvent = nil;
  g_fThreadActive: Boolean;
  g_hThread: THandle = 0;

procedure ShowClock();
var
  rCtrTime: TZG_Ctr_Clock;
begin
  CheckZGError(ZG_Ctr_GetClock(g_hCtr, rCtrTime));
  Writeln(format('%.2d.%.2d.%.4d %.2d:%.2d:%.2d (stopped: %s)', [
      rCtrTime.nDay,
      rCtrTime.nMonth,
      rCtrTime.nYear,
      rCtrTime.nHour,
      rCtrTime.nMinute,
      rCtrTime.nSecond,
      BoolToStr(rCtrTime.fStopped, True)]));
end;

procedure SyncWithPC();
var
  rPcTime: TSystemTime;
  rCtrTime: TZG_Ctr_Clock;
begin
  GetLocalTime(rPcTime);
  FillChar(rCtrTime, SizeOf(rCtrTime), 0);
  with rCtrTime do
  begin
    nYear := rPcTime.wYear;
    nMonth := rPcTime.wMonth;
    nDay := rPcTime.wDay;
    nHour := rPcTime.wHour;
    nMinute := rPcTime.wMinute;
    nSecond := rPcTime.wSecond;
  end;
  CheckZGError(ZG_Ctr_SetClock(g_hCtr, rCtrTime));
  Writeln('Synchronized.');
end;

function CheckNotifyMsgs(): HResult;
var
  nMsg: Cardinal;
  nMsgParam: NativeInt;
  nOffs: Int64;
begin
  repeat
    Result := ZG_Ctr_GetNextMessage(g_hCtr, nMsg, nMsgParam);
    if Result <> S_OK then
      break;
    case nMsg of
      ZG_N_CTR_CLOCK:
      begin
        nOffs := PInt64(nMsgParam)^;
        Writeln(format('Clock desync: %d sec', [nOffs]));
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
  hCvt: THandle;  // Дескриптор конвертера
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
    rOp.nSpeed := ZG_SPEED_57600;
    CheckZGError(ZG_Cvt_Open(hCvt, @rOp));
    FillChar(rCtrInfo, SizeOf(rCtrInfo), 0);
    CheckZGError(ZG_Ctr_Open(g_hCtr, hCvt, CtrAddr, 0, @rCtrInfo));
    Writeln(format('%s addr: %d, sn: %d, v%d.%d.', [
        CtrTypeStrs[rCtrInfo.nType],
        rCtrInfo.nAddr,
        rCtrInfo.nSn,
        LoByte(rCtrInfo.nVersion), HiByte(rCtrInfo.nVersion)]));
    // Настройка уведомлений о рассинхронизации часов контроллера с часами ПК
    g_oEvent := TEvent.Create(nil, True, False, '');
    FillChar(rNS, SizeOf(rNS), 0);
    rNS.nNMask := ZG_NF_CTR_CLOCK;
    rNS.hEvent := g_oEvent.Handle;
    rNS.nClockOffs := 5; // Допустимое расхождение часов в секундах
    rNS.nCheckStatePeriod := 1000;  // Период проверки часов в миллисекундах
    CheckZGError(ZG_Ctr_SetNotification(g_hCtr, @rNS));
    StartNotifyThread();
    Writeln('-----');
    repeat
      Writeln('Enter command number:');
      Writeln('1 - read clock');
      Writeln('2 - synchronize with computer clock');
      Writeln('0 - quit');
      ReadLn(s);
      Writeln;
      case StrToIntDef(s, 0) of
        1: ShowClock();
        2: SyncWithPC();
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
