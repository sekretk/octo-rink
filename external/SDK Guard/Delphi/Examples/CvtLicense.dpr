program CvtLicense;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, IniFiles, Classes,
  ZBase, ZGuard, ZPort, ZGClasses, Utils;

const
  CvtPortType = ZP_PORT_COM;
  CvtPortName: WideString = 'com11';
var
  g_hCvt: THandle;


procedure ShowLicense();
var
  rInfo: TZG_CVT_LIC_INFO;
begin
  CheckZGError(ZG_Cvt_GetLicense(g_hCvt, ZG_DEF_CVT_LICN, rInfo));
  Writeln(format('Status: %d', [rInfo.nStatus]));
  if rInfo.nMaxCtrs = $FF then
    Writeln('MaxCtrs: unlimited')
  else
    Writeln(format('MaxCtrs: %d', [rInfo.nMaxCtrs]));
  if rInfo.nMaxKeys = $FFFF then
    Writeln('MaxKeys: unlimited')
  else
    Writeln(format('MaxKeys: %d', [rInfo.nMaxKeys]));
  if rInfo.nMaxYear = $FFFF then
    Writeln('MaxDate: unlimited')
  else
    Writeln(format('MaxDate: %.2d.%.2d.%.4d', [rInfo.nMaxDay, rInfo.nMaxMon, rInfo.nMaxYear]));
  if rInfo.nDownCountTime = $FFFF then
    Writeln('DownCountTime: unlimited')
  else
    Writeln(format('DownCountTime: %d', [rInfo.nDownCountTime]));
end;

procedure ShowAllLicenses();
var
  aLic: array[0..ZG_MAX_LICENSES-1] of TZG_CVT_LIC_SINFO;
  nCount, i: Integer;
  pLic: PZG_CVT_LIC_SINFO;
begin
  CheckZGError(ZG_Cvt_GetAllLicenses(g_hCvt, @aLic, Length(aLic), nCount));
  if nCount = 0 then
  begin
    Writeln('Installed licenses not found.');
    exit;
  end;

  for i := 0 to nCount - 1 do
  begin
    pLic := @aLic[i];
    Writeln(format('%.2d(%d/%d);', [pLic.nLicN, pLic.nMaxCtrs, pLic.nMaxKeys]));
  end;
end;

procedure DoSetLicense();
var
  sFilename: String;
  sLicHex: String;
  sLicData: AnsiString;
begin
  Writeln('Enter license filename:');
  ReadLn(sFilename);
  if not FileExists(sFilename) then
  begin
    Writeln('File not found.');
    exit;
  end;
  with TMemIniFile.Create(sFilename) do
  begin
    try
      sLicHex := ReadString('LIC', 'TXT', '');
      if sLicHex <> '' then
      begin
        SetLength(sLicData, Length(sLicHex) div 2);
        SetLength(sLicData, HexToBin(PChar(sLicHex), @sLicData[1], Length(sLicData)));
      end;
    finally
      Free();
    end;
    if sLicData = '' then
    begin
      Writeln('No license data.');
      exit;
    end;
    CheckZGError(ZG_Cvt_SetLicenseData(g_hCvt, ZG_DEF_CVT_LICN, sLicData[1], Length(sLicData)));
    Writeln('Successfully.');
  end;
end;

procedure DoTest();
var
  rOp: TZG_CVT_OPEN_PARAMS;
  rInfo: TZG_CVT_INFO;
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
    FillChar(rInfo, SizeOf(rInfo), 0);
    FillCHar(rOp, SizeOf(rOp), 0);
    rOp.nType := CvtPortType;
    rOp.pszName := PWideChar(CvtPortName);
    rOp.nSpeed := ZG_SPEED_57600;
    CheckZGError(ZG_Cvt_Open(g_hCvt, @rOp, @rInfo));
    if rInfo.nMode <> ZG_GUARD_ADVANCED then
    begin
      Writeln('Not Advanced.');
      Readln;
      exit;
    end;
    ShowLicense();
    Writeln('-----');
    repeat
      Writeln('Enter command number:');
      Writeln('1 - show license');
      Writeln('2 - show all licenses...');
      Writeln('6 - set license...');
      Writeln('0 - quit');
      ReadLn(s);
      Writeln;
      case StrToIntDef(s, 0) of
        1: ShowLicense();
        2: ShowAllLicenses();
        6: DoSetLicense();
        0: break;
        else Writeln('Invalid command.');
      end;
      Writeln('-----');
    until False;
  finally
    if g_hCvt <> 0 then
      ZG_CloseHandle(g_hCvt);
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
