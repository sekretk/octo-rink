program EnumSerialPorts;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, SyncObjs,
  ZBase, ZGuard, ZPort, ZGClasses, Utils;

var
  g_hNotify: THandle = 0;
  g_oEvent: TEvent = nil;
  g_fThreadActive: Boolean;
  g_hThread: THandle = 0;


function CheckNotifyMsgs(): HResult;
var
  nMsg: Cardinal;
  nMsgParam: NativeInt;
  pInfo: PZP_DDN_Port_Info;
begin
  repeat
    Result := Zp_DD_GetNextMessage(g_hNotify, nMsg, nMsgParam);
    if Result <> S_OK then
      break;
    case nMsg of
      ZP_N_INSERT:
      begin
        pInfo  := PZP_DDN_Port_Info(nMsgParam);
        Writeln(format('Converter insert: %s %s (%s); %s', [
            PortTypeStrs[pInfo.rPort.nType],
            StrPas(pInfo.rPort.szName),
            StrPas(pInfo.rPort.szFriendly),
            BusyStrs[pInfo.rPort.nFlags and ZP_PIF_BUSY <> 0]]));
      end;
      ZP_N_REMOVE:
      begin
        pInfo  := PZP_DDN_Port_Info(nMsgParam);
        Writeln(format('Converter remove: %s %s (%s)', [
            PortTypeStrs[pInfo.rPort.nType],
            StrPas(pInfo.rPort.szName),
            StrPas(pInfo.rPort.szFriendly)]));
      end;
      ZP_N_CHANGE:
      begin
        pInfo  := PZP_DDN_Port_Info(nMsgParam);
        Writeln(format('State changed (%.2Xh): %s %s (%s); %s', [
            pInfo.nChangeMask,
            PortTypeStrs[pInfo.rPort.nType],
            StrPas(pInfo.rPort.szName),
            StrPas(pInfo.rPort.szFriendly),
            BusyStrs[pInfo.rPort.nFlags and ZP_PIF_BUSY <> 0]]));
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
      if g_hNotify <> 0 then
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
  rNS: TZp_DD_Notify_Settings;
  hList: THandle;
  nPortCount, i: Integer;
  rPI: TZp_Port_Info;
begin
  SetConsoleOutputCP(1251); // Переключение консоли на кодовую страницу CP1251 (Windows-1251).

  if not CheckZGVersion() then
  begin
    Writeln('Wrong version SDK Guard.');
    Readln;
    exit;
  end;

  CheckZGError(ZG_Initialize(ZP_IF_NO_MSG_LOOP));
  try
    Writeln('Enum serial ports...');
    nPortCount := 0;
    CheckZGError(ZG_GetPortInfoList(hList, nPortCount));
    try
      for i := 0 to nPortCount - 1 do
      begin
        ZP_GetPortInfo(hList, i, rPI);
        Writeln(format('%d. %s %s (%s); %s', [
            (i + 1),
            PortTypeStrs[rPI.nType],
            StrPas(rPI.szName),
            StrPas(rPI.szFriendly),
            BusyStrs[rPI.nFlags and ZP_PIF_BUSY <> 0]]));
      end;
    finally
      ZG_CloseHandle(hList);
    end;
    Writeln('--------------');
    if nPortCount > 0 then
      Writeln(format('Found %d ports', [nPortCount]))
    else
      Writeln('Ports not found.');

    // Настраиваем уведомления
    g_oEvent := TEvent.Create(nil, True, False, '');
    FillChar(rNS, SizeOf(rNS), 0);
    rNS.nNMask := ZP_NF_EXIST or ZP_NF_CHANGE;
    rNS.hEvent := g_oEvent.Handle;
    CheckZGError(ZG_SetNotification(g_hNotify, rNS, True, False));
    StartNotifyThread();

    Writeln('Wait events...');
    Readln;
  finally
    StopNotifyThread();
    if g_hNotify <> 0 then
      ZG_CloseHandle(g_hNotify);
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
