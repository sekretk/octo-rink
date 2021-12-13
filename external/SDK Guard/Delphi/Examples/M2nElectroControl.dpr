program M2nElectroControl;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, Classes,
  ZBase, ZGuard, ZPort, ZGClasses, Utils;

const
  CvtPortType = ZP_PORT_COM;
  CvtPortName: WideString = 'COM3';
  CtrAddr = 2;
var
  g_hCtr: THandle;  // Контроллер


procedure ShowConfig();
var
  rEC: TZG_Ctr_Electro_Config;
begin
  CheckZGError(ZG_Ctr_ReadElectroConfig(g_hCtr, rEC));
  Writeln(format('Power Config: %.2Xh', [rEC.nPowerConfig]));
  Writeln(format(#9'         Enabled: %s', [BoolToStr((rEC.nPowerConfig and ZG_EC_CF_ENABLED) <> 0, True)]));
  Writeln(format(#9'        Schedule: %s', [BoolToStr((rEC.nPowerConfig and ZG_EC_CF_SCHEDULE) <> 0, True)]));
  Writeln(format(#9' External reader: %s', [BoolToStr((rEC.nPowerConfig and ZG_EC_CF_EXT_READER) <> 0, True)]));
  Writeln(format(#9'          Invert: %s', [BoolToStr((rEC.nPowerConfig and ZG_EC_CF_INVERT) <> 0, True)]));
  Writeln(format(#9'Turn off at exit: %s', [BoolToStr((rEC.nPowerConfig and ZG_EC_CF_EXIT_OFF) <> 0, True)]));
  Writeln(format(#9' Opening of card: %s', [BoolToStr((rEC.nPowerConfig and ZG_EC_CF_CARD_OPEN) <> 0, True)]));
  Writeln(format('Power Delay (sec): %d', [rEC.nPowerDelay]));
end;

procedure ShowState();
var
  rES: TZG_Ctr_Electro_State;
begin
  CheckZGError(ZG_Ctr_GetElectroState(g_hCtr, rES));
  Writeln(format('Power Flags: %.2Xh', [rES.nPowerFlags]));
  Writeln(format(#9'         Enabled: %s', [BoolToStr((rES.nPowerFlags and ZG_EC_SF_ENABLED) <> 0, True)]));
  Writeln(format(#9'        Schedule: %s', [BoolToStr((rES.nPowerFlags and ZG_EC_SF_SCHEDULE) <> 0, True)]));
  Writeln(format(#9'     Open remote: %s', [BoolToStr((rES.nPowerFlags and ZG_EC_SF_REMOTE) <> 0, True)]));
  Writeln(format(#9'           Delay: %s', [BoolToStr((rES.nPowerFlags and ZG_EC_SF_DELAY) <> 0, True)]));
  Writeln(format(#9'            Card: %s', [BoolToStr((rES.nPowerFlags and ZG_EC_SF_CARD) <> 0, True)]));
end;

procedure DoSetConfig();
var
  rEC: TZG_Ctr_Electro_Config;
  s: String;
  nPowerDelay: Integer;
  oL : TStringList;
begin
  Writeln('Enter flag ''Enabled'' (0 or 1), flag ''Schedule'', flag ''External reader ' +
      'flag ''Invert'', flag ''Turn off at exit'', flag ''Opening of card'', Power Delay (sec):');
  Readln(s);
  oL := TStringList.Create();
  try
    oL.Delimiter := ',';
    oL.StrictDelimiter := True;
    oL.DelimitedText := s;
    if oL.Count < 7 then
    begin
      Writeln('Incorrect entry');
      exit;
    end;
    rEC.nPowerConfig := 0;
    if oL[0] = '1' then
      rEC.nPowerConfig := ZG_EC_CF_ENABLED;
    if oL[1] = '1' then
      Inc(rEC.nPowerConfig, ZG_EC_CF_SCHEDULE);
    if oL[2] = '1' then
      Inc(rEC.nPowerConfig, ZG_EC_CF_EXT_READER);
    if oL[3] = '1' then
      Inc(rEC.nPowerConfig, ZG_EC_CF_INVERT);
    if oL[4] = '1' then
      Inc(rEC.nPowerConfig, ZG_EC_CF_EXIT_OFF);
    if oL[5] = '1' then
      Inc(rEC.nPowerConfig, ZG_EC_CF_CARD_OPEN);
    if not TryStrToInt(oL[0], nPowerDelay) then
    begin
      Writeln('Incorrect entry');
      exit;
    end;
    rEC.nPowerDelay := nPowerDelay;
  finally
    oL.Free();
  end;
  CheckZGError(ZG_Ctr_WriteElectroConfig(g_hCtr, rEC, False));
  Writeln('Done.');
end;

procedure DoSetPowerSchedule();
var
  nDows, nBegHour, nBegMin, nEndHour, nEndMin: Integer;
  s: String;
  oL : TStringList;
  rTz: TZG_CTR_TIMEZONE;
  n: Integer;
begin
  Writeln('Enter days of week (hex), time from (hh:mm), time to (hh:mm):');
  Readln(s);
  oL := TStringList.Create();
  try
    oL.Delimiter := ',';
    oL.StrictDelimiter := True;
    oL.DelimitedText := s;
    if (oL.Count < 3) or
        (not TryStrToInt('$' + oL[0], nDows)) then
    begin
      Writeln('Incorrect entry');
      exit;
    end;
  finally
    oL.Free();
  end;
  rTz.nDayOfWeeks := nDows;

  n := Pos(':', oL[1]);
  if (n = 0) or
      (not TryStrToInt(Copy(oL[1], 1, n - 1), nBegHour)) or
      (not TryStrToInt(Copy(oL[1], n + 1, Length(oL[1]) - n), nBegMin)) then
  begin
    Writeln('Incorrect entry');
    exit;
  end;

  n := Pos(':', oL[2]);
  if (n = 0) or
      (not TryStrToInt(Copy(oL[2], 1, n - 1), nEndHour)) or
      (not TryStrToInt(Copy(oL[2], n + 1, Length(oL[2]) - n), nEndMin)) then
  begin
    Writeln('Incorrect entry');
    exit;
  end;

  rTz.nBegHour := nBegHour;
  rTz.nBegMinute := nBegMin;
  rTz.nEndHour := nEndHour;
  rTz.nEndMinute := nEndMin;
  Writeln('Writing...');
  CheckZGError(ZG_Ctr_WriteTimeZones(g_hCtr, 6, @rTz, 1, nil, nil));
  Writeln('Done.');
end;

procedure DoTogglePower();
var
  rES: TZG_Ctr_Electro_State;
  fOn: Boolean;
begin
  CheckZGError(ZG_Ctr_GetElectroState(g_hCtr, rES));
  fOn := ((rES.nPowerFlags and ZG_EC_SF_ENABLED) = 0);
  CheckZGError(ZG_Ctr_SetElectroPower(g_hCtr, fOn));
  Writeln('Done.');
end;

procedure DoTest();
var
  hCvt: THandle;
  s: String;
  rOp: TZG_CVT_OPEN_PARAMS;
  rCtrInfo: TZG_CTR_Info;
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
    if (rCtrInfo.nFlags and ZG_CTR_F_ELECTRO) = 0 then
    begin
      Writeln('ElectroControl function is not supported');
      exit;
    end;
    Writeln(format('%s addr: %d, s/n: %d, v%d.%d.', [
        CtrTypeStrs[rCtrInfo.nType],
        rCtrInfo.nAddr,
        rCtrInfo.nSn,
        (rCtrInfo.nVersion and $FF), (rCtrInfo.nVersion shr 8) and $FF]));
    Writeln('-----');
    repeat
      Writeln('Enter command number:');
      Writeln('1 - Show config');
      Writeln('2 - Show state');
      Writeln('6 - Set config...');
      Writeln('7 - Set power schedule...');
      Writeln('8 - Toggle power');
      Writeln('0 - quit');
      ReadLn(s);
      Writeln;
      case StrToIntDef(s, 0) of
        1: ShowConfig();
        2: ShowState();
        6: DoSetConfig();
        7: DoSetPowerSchedule();
        8: DoTogglePower();
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
