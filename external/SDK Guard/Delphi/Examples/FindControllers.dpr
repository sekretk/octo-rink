program FindControllers;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, SyncObjs,
  ZBase, ZGuard, ZPort, ZGClasses, Utils;

const
  CvtPortType = ZP_PORT_COM;
  CvtPortName: WideString = 'COM3';
var
  g_hCvt: THandle;
  g_nCtrCount: Integer;
  g_fNotifyEnabled: Boolean;
  g_oEvent: TEvent = nil;
  g_fThreadActive: Boolean;
  g_hThread: THandle = 0;


function EnumCtrsCB(AInfo: PZG_FIND_CTR_INFO; APos: Integer; AMax: Integer; AUserData: Pointer): LongBool; stdcall;
var
  sType: String;
begin
  if (AInfo.nType > ZG_CTR_UNDEF) and (AInfo.nType <= ZG_CTR_GUARDNET) then
    sType := CtrTypeStrs[AInfo.nType]
  else
    sType := format('Unknown[%.2Xh]', [AInfo.nTypeCode]);
  Writeln(format('%s, addr: %d, s/n: %d, FW v%d.%d, K: %d, E: %d, %s;', [
      sType,
      AInfo.nAddr,
      AInfo.nSn,
      LoByte(AInfo.nVersion), HiByte(AInfo.nVersion),
      AInfo.nMaxKeys,
      AInfo.nMaxEvents,
      RdModeStrs[(AInfo.nFlags and ZG_CTR_F_PROXIMITY) <> 0]]));
  Inc(g_nCtrCount);
  Result := True;
end;

procedure DoGuardFindCtrs();
begin
  Writeln('Search controllers...');
  Writeln('-------------');
  g_nCtrCount := 0;
  CheckZGError(ZG_Cvt_EnumControllers(g_hCvt, EnumCtrsCB, nil));
  if g_nCtrCount > 0 then
  begin
    Writeln('--');
    Writeln(format('Found %d controllers.', [g_nCtrCount]));
  end
  else
    Writeln('Controllers not found.');
end;

function CheckNotifyMsgs(): HResult;
Const
  aCtrActs: array[Boolean] of String = ('Remove', 'Insert');
var
  nMsg: Cardinal;
  nMsgParam: NativeInt;
  pFInfo: PZg_Find_Ctr_Info;
  sType: String;
begin
  repeat
    Result := ZG_Cvt_GetNextMessage(g_hCvt, nMsg, nMsgParam);
    if Result <> S_OK then
      break;
    case nMsg of
      ZG_N_CVT_CTR_INSERT,
      ZG_N_CVT_CTR_REMOVE:
      begin
        pFInfo := PZG_FIND_CTR_INFO(nMsgParam);
        if (pFInfo.nType > ZG_CTR_UNDEF) and (pFInfo.nType <= ZG_CTR_GUARDNET) then
          sType := CtrTypeStrs[pFInfo.nType]
        else
          sType := format('Unknown[%.2X]', [pFInfo.nTypeCode]);
        Writeln(format('%s %s, addr: %d, s/n: %d, FW v%d.%d, K: %d, E: %d, %s;', [
            aCtrActs[nMsg = ZG_N_CVT_CTR_INSERT],
            sType,
            pFInfo.nAddr,
            pFInfo.nSn,
            LoByte(pFInfo.nVersion), HiByte(pFInfo.nVersion),
            pFInfo.nMaxKeys,
            pFInfo.nMaxEvents,
            RdModeStrs[(pFInfo.nFlags and ZG_CTR_F_PROXIMITY) <> 0]]));
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
      if g_hCvt = 0 then
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

procedure EnableNotification(AEnable: Boolean=True; AReport: Boolean=True);
var
  rNS: TZG_Cvt_Notify_Settings;
begin
	if AEnable then
	begin
    if g_oEvent = nil then
      g_oEvent := TEvent.Create(nil, True, False, '');
    // Настройка уведомлений о подключении/отключении контроллеров
    FillChar(rNS, SizeOf(rNS), 0);
    rNS.nNMask := ZG_NF_CVT_CTR_EXIST;
    rNS.hEvent := g_oEvent.Handle;
    rNS.nScanCtrsPeriod := 1500; // Период сканирования контроллеров
    CheckZGError(ZG_Cvt_SetNotification(g_hCvt, @rNS));
    StartNotifyThread();
  end
  else
  begin
    StopNotifyThread();
    CheckZGError(ZG_Cvt_SetNotification(g_hCvt,nil));
  end;
  g_fNotifyEnabled := AEnable;
  if AReport then
    Writeln('Done.');
end;

procedure DoTest();
Const
  EnableStrs: array[Boolean] of String = ('Disable', 'Enable');
var
  rOp: TZG_CVT_OPEN_PARAMS;
  rInfo: TZG_Cvt_Info;
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
  g_hCvt := 0;
  try
    FillChar(rInfo, SizeOf(rInfo), 0);
    FillCHar(rOp, SizeOf(rOp), 0);
    rOp.nType := CvtPortType;
    rOp.pszName := PWideChar(CvtPortName);
    rOp.nSpeed := ZG_SPEED_57600;
    CheckZGError(ZG_Cvt_Open(g_hCvt, @rOp, @rInfo));
    Writeln;
    DoGuardFindCtrs();
    EnableNotification(True, False);
    Writeln('-----');
    repeat
      Writeln('Enter command number:');
      Writeln('1 - rescan');
      Writeln(format('2 - %s notifications', [EnableStrs[not g_fNotifyEnabled]]));
      Writeln('0 - quit');
      ReadLn(s);
      Writeln;
      case StrToIntDef(s, 0) of
        1: DoGuardFindCtrs();
        2: EnableNotification(not g_fNotifyEnabled);
        0: break;
        else Writeln('Invalid command.');
      end;
      Writeln('-----');
    until False;
  finally
    StopNotifyThread();
    if g_hCvt <> 0 then
      ZG_CloseHandle(g_hCvt);
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
