unit uUtils;

interface

uses
  Windows, SysUtils, StrUtils, ZPort;

Const
  RdVidPids: array[0..3] of Cardinal = (
//    0,      // VCOM
    $403 + $1234 shl 16,  // Z2USB, Z2USB MF
    $403 + $1237 shl 16,  // rs-485 (z-397, z-397 guard)
    $403 + $6001 shl 16,
    $FFFFFFFF   // rs-232 (com1, com2)
  );
  CvtVidPids: array[0..3] of Cardinal = (
//    0,
    $403 + $1235 shl 16,	// Z-397
    $403 + $1237 shl 16,	// Z-397 Guard
    $403 + $6001 shl 16,
    $FFFFFFFF
  );


function GetTickSpan(AOld, ANew: Cardinal): Cardinal; inline;

function RdParse(AReply: Pointer; ACount: Cardinal;
      var VPartially: LongBool; VInfo: PZP_DEVICE_INFO;
      VPortArr: PZP_PORT_INFO; AArrLen: Integer; var VPortCount: Integer): LongBool; stdcall;
function CvtParse(AReply: Pointer; ACount: Cardinal;
      var VPartially: LongBool; VInfo: PZP_DEVICE_INFO;
      VPortArr: PZP_PORT_INFO; AArrLen: Integer; var VPortCount: Integer): LongBool; stdcall;
function IpParse(AReply: Pointer; ACount: Cardinal;
      var VPartially: LongBool; VInfo: PZP_DEVICE_INFO;
      VPortArr: PZP_PORT_INFO; AArrLen: Integer; var VPortCount: Integer): LongBool; stdcall;

implementation

uses
  Character, Math, uConst;


function GetTickSpan(AOld, ANew: Cardinal): Cardinal;
begin
  {This is just in case the TickCount rolled back to zero}
  if ANew >= AOld then
    Result := (ANew - AOld)
  else
    Result := (High(LongWord) - AOld + ANew);
end;

function RdParse(AReply: Pointer; ACount: Cardinal;
      var VPartially: LongBool; VInfo: PZP_DEVICE_INFO;
      VPortArr: PZP_PORT_INFO; AArrLen: Integer; var VPortCount: Integer): LongBool; stdcall;
var
  s: String;
  i, n, nPos, nPos2: Integer;
  nDevType: TDevType;
  pch, pch2: PAnsiChar;
begin
  if (AReply = nil) or (VInfo = nil) then
    Exit(False);
  try
    pch := AReply;
    VPortCount := 1;

    // Пропускаем первую строку если она пустая
    pch2 := StrScan(pch, #10);
    if (pch2 <> nil) and (pch2 < (pch + 2)) then
    begin
      pch := (pch2 + 1);
      pch2 := StrScan(pch, #10);
    end;
    if pch2 = nil then
    begin
      VPartially := True;
      Exit(True);
    end;

    SetLength(s, pch2 - pch);
    SetLength(s, MultiByteToWideChar(CP_ACP, 0, pch, (pch2 - pch), @s[1], Length(s)));
    // Парсим с/н
    nPos := Pos('S/N:', s);
    if nPos = 0 then
      Exit(False);
    Inc(nPos, 4);
    while (nPos <= Length(s)) and (s[nPos] = ' ') do
      Inc(nPos);
    nPos2 := nPos;
    while (nPos2 <= Length(s)) and IsDigit(s[nPos2]) do
      Inc(nPos2);
    if nPos2 = nPos then
      Exit(False);
    VInfo.nSn := StrToInt(Copy(s, nPos, nPos2 - nPos));

    // Парсим модель
    nPos := Pos('[', s);
    if nPos = 0 then
      Exit(False);
    Inc(nPos);
    nPos2 := Pos(']', s);
    if nPos2 = 0 then
      Exit(False);
    if nPos2 = nPos then
      Exit(False);
    if not TryStrToInt('$' + Copy(s, nPos, nPos2 - nPos), n) then
      Exit(False);
    case n of
      $301: nDevType := dev_Z2U;
      $302: nDevType := dev_Z2EHR;
      $501: nDevType := dev_M3A;
      $811: nDevType := dev_Z2M;
      $903: nDevType := dev_M3N;
      $F11: nDevType := dev_CPZ2MF;
      else nDevType := dev_UNDEF;
    end;
    VInfo.nModel := Cardinal(nDevType);

    // Парсим версию прошивки
    pch := (pch2 + 1);
    i := 0;
    repeat
      pch2 := StrScan(pch, #10);
      if pch2 = nil then
        break;
      Inc(i);
      if i = 4 then
        break;
      pch := (pch2 + 1);
    until False;
    if i < 4 then
    begin
      VPartially := True;
      Exit(True);
    end;

    VInfo.nVersion := 0;
    VPartially := False;
    VPortArr.nDevTypes := (1 shl VInfo.nTypeId);
    Result := True;
    if pch2 > pch then
    begin
      SetLength(s, pch2 - pch);
      SetLength(s, MultiByteToWideChar(CP_ACP, 0, pch, (pch2 - pch), @s[1], Length(s)));
      nPos := Pos('Software version:', s);
      if nPos <> 0 then
      begin
        Inc(nPos, 17);
        while (nPos <= Length(s)) and (s[nPos] = ' ') do
          Inc(nPos);
        if nPos <= Length(s) then
        begin
          for i := 0 to 3 do
          begin
            nPos2 := nPos;
            while (nPos2 <= Length(s)) and IsDigit(s[nPos2]) do
              Inc(nPos2);
            if nPos2 = nPos then
              break;
            n := (StrToInt(Copy(s, nPos, nPos2 - nPos)) and $ff);
            Inc(VInfo.nVersion, n shl (8 * i));
            if s[nPos2] <> '.' then
              break;
            nPos := (nPos2 + 1);
          end;
        end;
      end;
    end;
  except
    Result := False;
  end;
end;

function CvtParse(AReply: Pointer; ACount: Cardinal;
      var VPartially: LongBool; VInfo: PZP_DEVICE_INFO;
      VPortArr: PZP_PORT_INFO; AArrLen: Integer; var VPortCount: Integer): LongBool; stdcall;
var
  s: String;
  nPos, nPos2, i, n: Integer;
  pCvt: PCvtInfo;
  pch, pch2: PAnsiChar;
begin
  try
    VPortCount := 1;
    if AReply = nil then
    begin
      VInfo.nModel := Cardinal(dev_Z397);
      Exit(True);
    end;
    pch := AReply;
    pCvt := PCvtInfo(VInfo);

    VPartially := True;
    // Пропускаем первую строку если она пустая
    pch2 := StrScan(pch, #10);
    if (pch2 <> nil) and (pch2 < (pch + 6)) then
    begin
      pch := (pch2 + 1);
      pch2 := StrScan(pch, #10);
    end;
    if pch2 = nil then
      Exit(True);

    SetLength(s, pch2 - pch);
    SetLength(s, MultiByteToWideChar(CP_ACP, 0, pch, (pch2 - pch), @s[1], Length(s)));

    // Парсим модель
    if Pos('Z397-IP', s) > 0 then
      VInfo.nModel := Cardinal(dev_Z397_IP)
    else if Pos('Z397-WEB', s) > 0 then
      VInfo.nModel := Cardinal(dev_Z397_Web)
    else if Pos('Z5R-WEB', s) > 0 then
      VInfo.nModel := Cardinal(dev_Z5R_Web)
    else
      VInfo.nModel := Cardinal(dev_Z397_GUARD);

    // Парсим с/н
    nPos := Pos('S/N:', s);
    if nPos = 0 then
      Exit(False);
    Inc(nPos, 4);
    while (nPos <= Length(s)) and (s[nPos] = ' ') do
      Inc(nPos);
    nPos2 := nPos;
    while (nPos2 <= Length(s)) and IsDigit(s[nPos2]) do
      Inc(nPos2);
    if nPos2 = nPos then
      Exit(False);
    VInfo.nSn := StrToInt(Copy(s, nPos, nPos2 - nPos));

    // Парсим 5-ую строку с версией
    pch := (pch2 + 1);
    i := 0;
    repeat
      pch2 := StrScan(pch, #10);
      if pch2 = nil then
        break;
      Inc(i);
      if i = 4 then
        break;
      pch := (pch2 + 1);
    until False;
    if i < 4 then
      Exit(True);

    VInfo.nVersion := 0;
    ASSERT(VInfo.cbSize >= SizeOf(pCvt^));
    pCvt.nGuardMode := gmUndef;
    Result := True;
    if pch2 > pch then
    begin
      SetLength(s, pch2 - pch);
      SetLength(s, MultiByteToWideChar(CP_ACP, 0, pch, (pch2 - pch), @s[1], Length(s)));
      nPos := Pos('Version ', s);
      if nPos <> 0 then
      begin
        Inc(nPos, 8);
        while (nPos <= Length(s)) and (s[nPos] = ' ') do
          Inc(nPos);
        if nPos <= Length(s) then
        begin
          for i := 0 to 3 do
          begin
            nPos2 := nPos;
            while (nPos2 <= Length(s)) and IsDigit(s[nPos2]) do
              Inc(nPos2);
            if nPos2 = nPos then
              break;
            n := (StrToInt(Copy(s, nPos, nPos2 - nPos)) and $ff);
            Inc(VInfo.nVersion, n shl (8 * i));
            if s[nPos2] <> '.' then
              break;
            nPos := (nPos2 + 1);
          end;
        end;
      end;
    end;

    // Парсим 7-ую строку с режимом работы
    pch := (pch2 + 1);
    i := 0;
    repeat
      pch2 := StrScan(pch, #10);
      if pch2 = nil then
        break;
      Inc(i);
      if i = 2 then
        break;
      pch := (pch2 + 1);
    until False;
    if i < 2 then
      Exit(True);

    VPartially := False;
    if pch2 > pch then
    begin
      SetLength(s, pch2 - pch);
      SetLength(s, MultiByteToWideChar(CP_ACP, 0, pch, (pch2 - pch), @s[1], Length(s)));
      nPos := Pos('Current mode ', s);
      if nPos <> 0 then
      begin
        Inc(nPos, 13);
        while (nPos <= Length(s)) and CharInSet(s[nPos], [' ', '–', #$2d]) do
          Inc(nPos);
        if nPos < Length(s) then
        begin
          nPos2 := nPos;
          while (nPos2 <= Length(s)) and IsLetter(s[nPos]) do
            Inc(nPos2);
          if nPos2 > nPos then
          begin
            s := LowerCase(Copy(s, nPos, nPos2 - nPos - 1));
            if s = 'normal' then
              pCvt.nGuardMode := gmNormal
            else if s = 'advanced' then
              pCvt.nGuardMode := gmAdvanced
            else if s = 'test' then
              pCvt.nGuardMode := gmTest
            else if s = 'accept' then
              pCvt.nGuardMode := gmAccept
          end;
        end;
      end;
    end;
  except
    Result := False;
  end;
end;

function IpParse(AReply: Pointer; ACount: Cardinal;
      var VPartially: LongBool; VInfo: PZP_DEVICE_INFO;
      VPortArr: PZP_PORT_INFO; AArrLen: Integer; var VPortCount: Integer): LongBool; stdcall;
const
  _ip_fmt = '%s:%d';
var
  s, s2, sScanIp: String;
  i, n, nPos, nPos2: Integer;
  pch: PWideChar;
  pPI: PZP_Port_Info;
begin
  try
    ASSERT((AReply <> nil) and (ACount > 0) and (VInfo <> nil));
    // Z397IP-ALL-SW:0.1 SN000008 L1_Port:1000 L2_Port:1001 L1_Conn:90.11.11.31 L2_Conn:0.0.0.0
    // Z397-WEB-SW:3.0.15 SN17 L1_Port:1000 L2_Port:1001 L1_Conn:webs dk.con.ru:80 L2_Conn:/hw
    SetLength(s, ACount + 1);
    SetLength(s, MultiByteToWideChar(CP_ACP, 0, AReply, Integer(ACount), @s[1], Length(s)));

    nPos := Pos('-', s);
    if nPos < 2 then
      Exit(False);
    s2 := LowerCase(LeftStr(s, nPos - 1));
    if s2 = 'z397ip' then
      VInfo.nModel := Cardinal(dev_Z397_IP)
    else if s2 = 'z397' then
      VInfo.nModel := Cardinal(dev_Z397_Web)
    else if s2 = 'z5r' then
      VInfo.nModel := Cardinal(dev_Z5R_Web)
    else
      Exit(False);

    nPos := PosEx('SW:', s, nPos);
    if nPos <> 0 then
    begin
      VInfo.nVersion := 0;
      nPos2 := (nPos + 3);
      for i := 0 to 3 do
      begin
        nPos := nPos2;
        while (nPos2 <= Integer(ACount)) and IsDigit(s[nPos2]) do
          Inc(nPos2);
        if nPos2 <= nPos then
          break;
        n := (StrToInt(Copy(s, nPos, nPos2 - nPos)) and $ff);
        Inc(VInfo.nVersion, n shl (i * 8));
        if (Cardinal(nPos2) > ACount) or (s[nPos2] <> '.') then
          break;
        Inc(nPos2);
      end;
    end;

    nPos := PosEx(' SN', s, nPos);
    if nPos = 0 then
      Exit(False);
    Inc(nPos, 3);
    nPos2 := nPos;
    while (nPos2 <= Integer(ACount)) and IsDigit(s[nPos2]) do
      Inc(nPos2);
    if nPos2 > Integer(ACount) then
      Exit(False);
    VInfo.nSn := StrToInt(Copy(s, nPos, nPos2 - nPos));

    pch := StrRScan(VPortArr.szName, ':');
    if pch <> nil then
      pch^ := #0;
    sScanIp := StrPas(VPortArr.szName);

		VPortCount := min(AArrLen, 2);
    if VPortCount > 0 then
    begin
      FillChar(VPortArr^, SizeOf(TZP_Port_Info) * VPortCount, 0);
      pPI := VPortArr;
      for i := 0 to VPortCount - 1 do
      begin
        nPos := PosEx(format(' L%d_Port:', [i + 1]), s, nPos2);
        if nPos = 0 then
          Exit(False);
        Inc(nPos, 9);
        nPos2 := nPos;
        while (nPos2 <= Integer(ACount)) and IsDigit(s[nPos2]) do
          Inc(nPos2);
        if nPos2 > Integer(ACount) then
          Exit(False);
        n := StrToInt(Copy(s, nPos, nPos2 - nPos));
        FormatBuf(pPI.szName, Length(pPI.szName), _ip_fmt, Length(_ip_fmt), [sScanIp, n]);
        Inc(pPI);
      end;

      pPI := VPortArr;
      for i := 0 to VPortCount - 1 do
      begin
        nPos := PosEx(format(' L%d_Conn:', [i + 1]), s, nPos2);
        if nPos = 0 then
          Exit(False);
        Inc(nPos, 9);
        nPos2 := nPos;
        while (nPos2 <= Integer(ACount)) and (s[nPos2] <> ' ') do
          Inc(nPos2);
        if nPos2 > Integer(ACount) then
          s2 := Copy(s, nPos, Integer(ACount) - nPos + 1)
        else
          s2 := Copy(s, nPos, nPos2 - nPos);
        if s2 <> '0.0.0.0' then
        begin
          pPI.nFlags := pPI.nFlags or ZP_PIF_BUSY;
          if Length(s2) > 0 then
            StrLCopy(pPI.szOwner, PWideChar(s2), min(Length(s2), Length(pPI.szOwner)));
        end;
        if nPos2 > Integer(ACount) then
          break;
        Inc(pPI);
      end;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

end.
