program EnumConverters;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils,
  ZBase, ZGuard, ZPort, ZGClasses, Utils;


procedure DoTest();
var
  nCvtCount, nPortCount, i: Integer;
  hSD: THandle;
  rSP: TZp_Search_Params;
  rDI: TZg_Enum_IpCvt_Info;
  aPIs: array[0..1] of TZp_Port_Info;
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
    Writeln('Enum converters...');
    nCvtCount := 0;
    FillChar(rSP, SizeOf(rSP), 0);
    CheckZGError(ZG_SearchDevices(hSD, rSP));
    try
      while ZG_FindNextDevice(hSD, rDI, @aPIs, Length(aPIs), nPortCount) = S_OK do
      begin
        Inc(nCvtCount);
        if rDI.nType <> ZG_CVT_UNDEF then
        begin
          Writeln(format('%d. %s s/n: %d, v%d.%d.%d, mode: %s;', [
              nCvtCount,
              CvtTypeStrs[rDI.nType],
              rDI.rBase.nSn,
              (rDI.rBase.nVersion and $ff), (rDI.rBase.nVersion shr 8) and $ff, (rDI.rBase.nVersion shr 16) and $ff,
              GuardModeStrs[rDI.nMode]]))
        end;
        for i := 0 to nPortCount - 1 do
          Writeln(format('%d. %s (%s); %s', [
              (i + 1),
              StrPas(aPIs[i].szName),
              StrPas(aPIs[i].szFriendly),
              BusyStrs[aPIs[i].nFlags and ZP_PIF_BUSY <> 0]]));
      end;
    finally
      Zg_CloseHandle(hSD);
    end;
    Writeln('--------------');
    if nCvtCount > 0 then
      Writeln(format('Found %d converters', [nCvtCount]))
    else
      Writeln('Converters not found.');

    Readln;
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
