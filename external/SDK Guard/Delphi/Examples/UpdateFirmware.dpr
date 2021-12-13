program UpdateFirmware;

{$APPTYPE CONSOLE}

uses
  SysUtils, Windows, Classes,
  ZBase, ZGuard, ZPort, ZGClasses, Utils;



function UpdateFW_CB(APos: Integer; AMax: Integer; AUserData: Pointer): LongBool; stdcall;
begin
  Write(format(#13'Update %3d%%...', [(APos * 100) div AMax]));;
  Result := True;
end;

procedure DoCvtUpdateFW();
var
  s: String;
  oData: TMemoryStream;
  rOp: TZg_Cvt_Open_Params;
  hList: THandle;
  nPortCount, i: Integer;
  rPI: TZp_Port_Info;
begin
  Writeln('Enter FW-filename:');
  ReadLn(s);
  if s = '' then
  begin
    WriteLn('Cancelled.');
    exit;
  end;
  if not FileExists(s) then
  begin
    WriteLn('File not found.');
    exit;
  end;
  oData := TMemoryStream.Create();
  try
    oData.LoadFromFile(s);

    CheckZGError(ZG_GetPortInfoList(hList, nPortCount));
    try
      for i := 0 to nPortCount - 1 do
      begin
        CheckZGError(ZP_GetPortInfo(hList, i, rPI));
        if (rPI.nType = ZP_PORT_COM) or
            ((rPI.nType = ZP_PORT_FT) and (rPI.szFriendly[0] <> #0)) then
        begin
          if rPI.nType = ZP_PORT_COM then s := rPI.szName else s := rPI.szFriendly;
          Writeln(format('%s %s', [
              s,
              BusyStrs[rPI.nFlags and ZP_PIF_BUSY <> 0]]));
        end;
      end;
    finally
      ZG_CloseHandle(hList);
    end;
    Writeln('--');
    Writeln('Enter COM-port name:');
    ReadLn(s);
    if s = '' then
    begin
      WriteLn('Cancelled.');
      exit;
    end;
    FillCHar(rOp, SizeOf(rOp), 0);
    rOp.nType := ZP_PORT_COM;
    rOp.pszName := PWideChar(WideString(s));
    rOp.nSpeed := ZG_SPEED_19200;
    CheckZGError(ZG_UpdateCvtFirmware(@rOp, oData.Memory, oData.Size, UpdateFW_CB, nil));
    Writeln;
    Writeln('Done.');
  finally
    oData.Free();
  end;
end;

procedure DoCtrUpdateFW();
var
  s: String;
  oData: TMemoryStream;
  nCtrSn, nPortCount: Integer;
  hCvt, hSearch: THandle;
  rOp: TZg_Cvt_Open_Params;
  rSP: TZp_Search_Params;
  rDI: TZg_Enum_IpCvt_Info;
  rPI: TZp_Port_Info;
begin
  Writeln('Enter FW-filename:');
  ReadLn(s);
  if s = '' then
  begin
    WriteLn('Cancelled.');
    exit;
  end;
  if not FileExists(s) then
  begin
    WriteLn('File not found.');
    exit;
  end;
  hCvt := 0;
  oData := TMemoryStream.Create();
  try
    oData.LoadFromFile(s);

    FillChar(rSP, SizeOf(rSP), 0);
    CheckZGError(ZG_SearchDevices(hSearch, rSP, True, False));
    try
      while ZG_FindNextDevice(hSearch, rDI, @rPI, 1, nPortCount) = S_OK do
      begin
        if (rPI.nType = ZP_PORT_COM) or
            ((rPI.nType = ZP_PORT_FT) and (rPI.szFriendly[0] <> #0)) then
        begin
          if rPI.nType = ZP_PORT_COM then s := rPI.szName else s := rPI.szFriendly;
          if rDI.nType <> ZG_CVT_UNDEF then
            Writeln(format('%s, %s, %s s/n: %d, v%d.%d.%d', [
                StrPas(rPI.szFriendly),
                BusyStrs[rPI.nFlags and ZP_PIF_BUSY <> 0],
                CvtTypeStrs[rDI.nType],
                rDI.rBase.nSn,
                (rDI.rBase.nVersion and $ff), (rDI.rBase.nVersion shr 8) and $ff, (rDI.rBase.nVersion shr 16) and $ff]))
          else
            Writeln(format('%s, %s', [
                StrPas(rPI.szFriendly),
                BusyStrs[rPI.nFlags and ZP_PIF_BUSY <> 0]]));

        end;
      end;
    finally
      ZG_CloseHandle(hSearch);
    end;
    Writeln('--');
    Writeln('Enter COM-port name:');
    ReadLn(s);
    if s = '' then
    begin
      WriteLn('Cancelled.');
      exit;
    end;
    FillCHar(rOp, SizeOf(rOp), 0);
    rOp.nType := ZP_PORT_COM;
    rOp.pszName := PWideChar(WideString(s));
    rOp.nSpeed := ZG_SPEED_19200;
    CheckZGError(ZG_Cvt_Open(hCvt, @rOp));
    Writeln('Enter controller s/n:');
    ReadLn(s);
    if s = '' then
    begin
      WriteLn('Cancelled.');
      exit;
    end;
    if not (TryStrToInt(s, nCtrSn) and (nCtrSn > 0) and (nCtrSn < 65535)) then
    begin
      Writeln('Incorrect entry.');
      exit;
    end;
    CheckZGError(ZG_Cvt_UpdateCtrFirmware(hCvt, nCtrSn, oData.Memory^, oData.Size, nil, UpdateFW_CB, nil));
    Writeln;
    Writeln('Done.');
  finally
    if hCvt <> 0 then
      ZG_CloseHandle(hCvt);
    oData.Free();
  end;
end;

procedure DoTest();
var
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
  try
    Writeln('-----');
    repeat
      Writeln('Enter command number:');
      Writeln('1 - Update Converter Firmware...');
      Writeln('2 - Update Controller Firmware...');
      Writeln('0 - quit');
      ReadLn(s);
      Writeln;
      case StrToIntDef(s, 0) of
        1: DoCvtUpdateFW();
        2: DoCtrUpdateFW();
        0: break;
        else Writeln('Invalid command.');
      end;
      Writeln('-----');
    until False;
  finally
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
