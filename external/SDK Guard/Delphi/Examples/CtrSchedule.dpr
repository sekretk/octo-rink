program CtrSchedule;

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
  g_nCtrMaxBanks: Integer;


procedure ShowSchedule();
var
  aTZs: array of TZG_CTR_TIMEZONE;
  pTz: PZG_CTR_TIMEZONE;
  i, j: Integer;
begin
  SetLength(aTZs, ZG_MAX_TIMEZONES);
  for i := 0 to g_nCtrMaxBanks - 1 do
  begin
    CheckZGError(ZG_Ctr_ReadTimeZones(g_hCtr, 0, @aTZs[0], Length(aTZs), nil, nil, i));
    Writeln('------------');
    Writeln(format('Bank %d:', [i]));
    for j := 0 to Length(aTZs) - 1 do
    begin
      pTz := @aTZs[j];
      Writeln(format('%d. Days of week: %.2Xh, Time: %.2d:%.2d - %.2d:%.2d', [
          j,
          pTz.nDayOfWeeks,
          pTz.nBegHour, pTz.nBegMinute,
          pTz.nEndHour, pTz.nEndMinute]));
    end;
  end;
  Writeln('Done.');
end;

procedure DoSetTimeZone();
var
  s: String;
  nBankN, nTzIdx, nDows, nBegHour, nBegMin, nEndHour, nEndMin, n: Integer;
  oL : TStringList;
  rTz: TZG_Ctr_TimeZone;
begin
  WriteLn('bank#, timezone index, days of week (hex), time from (hh:mm), time to (hh:mm):');
  ReadLn(s);
  oL := TStringList.Create();
  try
    oL.Delimiter := ',';
    oL.StrictDelimiter := True;
    oL.DelimitedText := s;
    if (oL.Count < 5) or
        (not TryStrToInt(oL[0], nBankN)) or
        (not TryStrToInt(oL[1], nTzIdx)) or
        (not TryStrToInt(oL[2], nDows)) then
    begin
      Writeln('Incorrect entry');
      exit;
    end;
    n := Pos(':', oL[3]);
    if (n = 0) or
        (not TryStrToInt(Copy(oL[3], 1, n - 1), nBegHour)) or
        (not TryStrToInt(Copy(oL[3], n + 1, Length(oL[3]) - n), nBegMin)) then
    begin
      Writeln('Incorrect entry');
      exit;
    end;
    n := Pos(':', oL[4]);
    if (n = 0) or
        (not TryStrToInt(Copy(oL[4], 1, n - 1), nEndHour)) or
        (not TryStrToInt(Copy(oL[4], n + 1, Length(oL[3]) - n), nEndMin)) then
    begin
      Writeln('Incorrect entry');
      exit;
    end;
  finally
    oL.Free();
  end;
  rTz.nDayOfWeeks := nDows;
  rTz.nBegHour := nBegHour;
  rTz.nBegMinute := nBegMin;
  rTz.nEndHour := nEndHour;
  rTz.nEndMinute := nEndMin;
  Writeln('Writing...');
  CheckZGError(ZG_Ctr_WriteTimeZones(g_hCtr, nTzIdx, @rTz, 1, nil, nil, nBankN));
  Writeln('Done.');
end;

procedure DoRestoreFactorySettings();
var
  aTZs: array of TZG_CTR_TIMEZONE;
  pTz: PZG_CTR_TIMEZONE;
  i: Integer;
begin
  SetLength(aTZs, ZG_MAX_TIMEZONES);
  // Подготовка данных для записи в контроллер
  for i := 0 to Length(aTZs) - 1 do
  begin
    pTz := @aTZs[i];
    pTz.nDayOfWeeks := $7F;
    pTz.nBegHour := 0;
    pTz.nBegMinute := 0;
    pTz.nEndHour := 23;
    pTz.nEndMinute := 59;
  end;
  Writeln('Writing (0x7F, 00:00-23:59)...');
  for i := 0 to g_nCtrMaxBanks - 1 do
    CheckZGError(ZG_Ctr_WriteTimeZones(g_hCtr, 0, @aTZs[0], Length(aTZs), nil, nil, i));
  Writeln('Done.');
end;

procedure DoTest();
var
  hCvt: THandle;  // Конвертер
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
    if (rCtrInfo.nType > ZG_CTR_UNDEF) and (rCtrInfo.nType <= ZG_CTR_GUARDNET) then
      s := CtrTypeStrs[rCtrInfo.nType]
    else
      s := format('Unknown[%.2Xh]', [rCtrInfo.nTypeCode]);
    if (rCtrInfo.nFlags and ZG_CTR_F_2BANKS) <> 0 then
      g_nCtrMaxBanks := 2
    else
      g_nCtrMaxBanks := 1;
    Writeln(format('%s addr: %d, sn: %d, v%d.%d, Max_Banks: %d', [
        s,
        rCtrInfo.nAddr,
        rCtrInfo.nSn,
        LoByte(rCtrInfo.nVersion), HiByte(rCtrInfo.nVersion),
        g_nCtrMaxBanks]));
    Writeln('-----');
    repeat
      Writeln('Enter command number:');
      Writeln('1 - Show schedule');
      Writeln('6 - Set time zone...');
      Writeln('9 - Restore factory settings (all banks)');
      Writeln('0 - quit');
      ReadLn(s);
      Writeln;
      case StrToIntDef(s, 0) of
        1: ShowSchedule();
        6: DoSetTimeZone();
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
