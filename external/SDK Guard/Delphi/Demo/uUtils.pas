unit uUtils;

interface

uses
  SysUtils, Contnrs, StrUtils, ZBase, ZGuard, ZGClasses;

type
  TZEvent = class
    m_nEvIdx        : Integer;          // Индекс события в контроллере
    m_nType         : TZG_Ctr_Ev_Type;  // Тип события
    m_nEvCode       : Byte;             // Код события в контроллере
  end;
  TZPassEvent = class(TZEvent)
    m_nDirect       : TZG_Ctr_Direct;   // Направление прохода
    m_dtDate        : TDateTime;        // Дата и время события

    m_nKeyIdx       : Integer;          // Индекс ключа в банке
    m_nKeyBank      : Integer;
    m_rKeyNum       : TZ_KeyNum;        // Номер ключа
    m_fKeyNumValid  : Boolean;
  end;
  TZEcEvent = class(TZEvent)
    m_dtDate        : TDateTime;        // Дата и время события
    m_nSubEv        : TZG_Ec_Sub_EV;
    m_nPowerFlags   : Cardinal;
  end;
  TZFireEvent = class(TZEvent)
    m_dtDate        : TDateTime;        // Дата и время события
    m_nSubEv        : TZG_Fire_Sub_EV;
    m_nFireFlags    : Cardinal;
  end;
  TZSecurEvent = class(TZEvent)
    m_dtDate        : TDateTime;        // Дата и время события
    m_nSubEv        : TZG_Secur_Sub_Ev;
    m_nSecurFlags   : Cardinal;
  end;
  TZKeyEvent = class(TZEvent)
    m_rKeyNum       : TZ_KeyNum;        // Номер ключа
  end;
  TZModeEvent = class(TZEvent)
    m_dtDate        : TDateTime;        // Дата и время события
    m_nMode         : TZG_Ctr_Mode;
    m_nSubEv        : TZG_Mode_Sub_EV;
  end;
  TZHotelEvent = class(TZEvent)
    m_dtDate        : TDateTime;        // Дата и время события
    m_nMode         : TZG_Hotel_Mode;
    m_nSubEv        : TZG_Hotel_Sub_Ev;
    m_nFlags        : Cardinal;
  end;
  TZEventList = class(TObjectList)
  protected
    function GetItem(AIdx: Integer): TZEvent; inline;
    procedure SetItem(AIdx: Integer; AValue: TZEvent); inline;
  public
    property Items[Idx: Integer]: TZEvent read GetItem write SetItem; default;
  end;

  TNumFormat = (nfAuto, nfEmMarine, nfDallas);


function GetBit(AVal: Cardinal; AIdx: Integer): Boolean; inline;
function TryHexToDec(AHex: Char; var VDec: Integer): Boolean;
function NextCsvField(var P: PChar): String;

function KeyNumToStr(Const ANum: TZ_KeyNum; AProximity: Boolean; ADualIdx: Integer=-1): String;
function ZKeyCodeToStr(ACode: Integer; AErrChar: Char=#0): String;
function ZKeyStrToCode(Const AStr: String): Integer;
function ParseKeyNum(var VKeyNum: TZ_KEYNUM; Const AText: String; ADualIdx: Integer=-1): Boolean;
function KeyAccessToStr(AAccess: Cardinal): String;

function _NewCtrEvent(AList: TZEventList; ACtr: TZController; AEvIdx: Integer; Const AInfo: TZG_CTR_EVENT): TZEvent;
function GetTickSpan(AOld, ANew: Cardinal): Cardinal;

implementation

uses
  DateUtils, Math;


function GetBit(AVal: Cardinal; AIdx: Integer): Boolean;
begin
  Result := ((AVal shr AIdx) and 1) <> 0;
end;

function TryHexToDec(AHex: Char; var VDec: Integer): Boolean;
begin
  case AHex of
    '0'..'9': VDec := Ord(AHex) - Ord('0');
    'a'..'f': VDec := Ord(AHex) - Ord('a') + 10;
    'A'..'F': VDec := Ord(AHex) - Ord('A') + 10;
    else
    begin
      Result := False;
      exit;
    end;
  end;
  Result := True;
end;

function NextCsvField(var P: PChar): String;
Const
  CSV_SEP = ';';
var
  fQ: Boolean;
  pp: PChar;
begin
  pp := p;
  fQ := (pp^ = '"');

  if fQ then       // """a""b""c";        // """;";    // "";
  begin            // ftf tf tf t         // ftf t        ft
    Inc(pp);
    while p^ <> #0 do
    begin
      case p^ of
        '"': fQ := not fQ;
        CSV_SEP:
          if fQ then
          begin
            if (p - pp) > 1 then
              (p - 1)^ := #0;
            p^ := #0;
            Inc(p);
            break;
          end;
      end;
      Inc(p);
    end;
  end
  else
  begin   // a"b"c
    repeat
      if p^ = CSV_SEP then
      begin
        p^ := #0;
        Inc(p);
        break;
      end;
      Inc(p);
    until p^ = #0;
  end;
  if p^ = #0 then p := nil;
  Result := StrPas(pp);
end;

function KeyNumToStr(Const ANum: TZ_KeyNum; AProximity: Boolean; ADualIdx: Integer): String;
var
  i: Integer;
begin
  if AProximity then
  begin
    case ADualIdx of
      0: Result := format('%.3d,%.5d', [ANum[3], PWord(@ANum[1])^]);
      1: Result := format('%.3d,%.5d', [ANum[6], PWord(@ANum[4])^]);
      else Result := format('[%.2X%.2X] %.3d,%.5d', [ANum[5], ANum[4], ANum[3], PWord(@ANum[1])^]);
    end;
  end
  else
  begin
    case ADualIdx of
      0: Result := IntToHex(PInteger(@ANum[1])^ and $FFFFFF, 6);
      1: Result := IntToHex(PInteger(@ANum[4])^ and $FFFFFF, 6);
      else
      begin
        Result := '';
        for i := ANum[0] downto 1 do
          Result := Result + IntToHex(ANum[i], 2);
      end;
    end;
  end;
end;

function ZKeyCodeToStr(ACode: Integer; AErrChar: Char): String;
var
  i, n, nLen: Integer;
begin
  Result := '';
  nLen := 0;
  for i := 1 to 6 do
  begin
    n := (ACode and $F);
    if (n >= 1) and (n <= 10) then
    begin
      Result := Result + Chr(Ord('0') + (n mod 10));
      nLen := Length(Result);
    end
    else
    begin
      if AErrChar <> #0 then
        Result := Result + AErrChar
      else if n = 0 then
        Result := Result + 'A'
      else
        Result := Result + Chr(Ord('A') - 10 + n);
      if n <> 0 then
        nLen := Length(Result);
    end;
    ACode := ACode shr 4;
  end;
  SetLength(Result, nLen);
  Result := ReverseString(Result);
end;

function ZKeyStrToCode(Const AStr: String): Integer;
var
  i, n, nStart: Integer;
begin
  Result := 0;
  nStart := max(Length(AStr) - 5, 1);
  for i := nStart to Length(AStr) do
  begin
    n := Ord(AStr[i]) - Ord('0');
    if (n < 0) or (n > 9) then
      n := 0
    else if n = 0 then
      n := 10;
    if i <> nStart then
      Result := Result shl 4;
    Inc(Result, n);
  end;
end;

function ParseKeyNum(var VKeyNum: TZ_KEYNUM; Const AText: String; ADualIdx: Integer): Boolean;
var
  nGroup, nNumber, nFacility, n, n2, i, j: Integer;
begin
  if ADualIdx = -1 then
    FillChar(VKeyNum, SizeOf(VKeyNum), 0);
  VKeyNum[0] := 6;
  n := Pos(',', AText);
  if n <> 0 then
  begin
    nFacility := 0;
    n2 := Pos(']', AText);
    if n2 <> 0 then
      nFacility := StrToIntDef('$' + Copy(AText, 2, n2 - 2), 0);
    Inc(n2);
    if AText[n2] = ' ' then
      Inc(n2);
    if not (TryStrToInt(Copy(AText, n2, n - n2), nGroup) and
        TryStrToInt(Copy(AText, n + 1, Length(AText) - n), nNumber)) then
      Exit(False);
    case ADualIdx of
      0:
      begin
        PWord(@VKeyNum[1])^ := nNumber;
        VKeyNum[3] := nGroup;
      end;
      1:
      begin
        PWord(@VKeyNum[4])^ := nNumber;
        VKeyNum[6] := nGroup;
      end
      else
      begin
        PWord(@VKeyNum[1])^ := nNumber;
        VKeyNum[3] := nGroup;
        PWord(@VKeyNum[4])^ := nFacility;
      end;
    end;
  end
  else
  begin
//    i := Length(AText);
    case ADualIdx of
      0: j := 1;
      1: j := 4;
      else j := 1;
    end;
    for i := Length(AText) downto 1 do
    begin
      if not TryHexToDec(AText[i], n) then
        n := 0;
      if ((Length(AText) - i) mod 2) = 0 then
        VKeyNum[j] := (VKeyNum[j] and $F0) or n
      else
      begin
        VKeyNum[j] := (VKeyNum[j] and $0F) or (n shl 4);
        Inc(j);
        if j > 6 then
          break;
      end;
    end;

//    while i >= 1 do
//    begin
//      if not TryHexToDec(AText[i - 1], n) then
//        n := 0;
//      if not TryHexToDec(AText[i], n2) then
//        n2 := 0;
//      VKeyNum[j] := (n2 and $F) or ((n and $F) shl 4);
//      Inc(j);
//      if j > 6 then
//        break;
//      Dec(i, 2);
//    end;
  end;
  Result := True;
end;

function KeyAccessToStr(AAccess: Cardinal): String;
var
  i: Integer;
begin
  if AAccess = 0 then
    Result := 'never'
  else if AAccess = $ff then
    Result := 'ever'
  else
  begin
    Result := '1234567';
    for i := 0 to 6 do
      if not GetBit(AAccess, i) then
        Result[i + 1] := '-';
  end;
end;

function TZEventList.GetItem(AIdx: Integer): TZEvent;
begin
  Result := inherited Items[AIdx] as TZEvent;
end;

procedure TZEventList.SetItem(AIdx: Integer; AValue: TZEvent);
begin
  inherited Items[AIdx] := AValue;
end;

function _NewCtrEvent(AList: TZEventList; ACtr: TZController; AEvIdx: Integer; Const AInfo: TZG_CTR_EVENT): TZEvent;
var
  rTime: TZG_Ev_Time;
  rKeyNum: TZ_KeyNum;
  nEcSubEv: TZG_EC_Sub_EV;
  nFireSubEv: TZG_Fire_Sub_EV;
  nSecurSubEv: TZG_Secur_Sub_EV;
  nModeSubEv: TZG_Mode_Sub_EV;
  nMode: TZG_Ctr_Mode;
  nHMode: TZG_Hotel_Mode;
  nHSubEv: TZG_Hotel_Sub_Ev;
  nFlags: Cardinal;
  _nDirect: TZG_Ctr_Direct;
  _nKeyIdx, _nKeyBank: Integer;
begin
  Result := nil;
  try
    case AInfo.nType of
      ZG_EV_ELECTRO_ON,
      ZG_EV_ELECTRO_OFF:
      begin
        ACtr.DecodeEcEvent(AInfo.aData, rTime, nEcSubEv, nFlags);
        Result := TZEcEvent.Create();
        with Result as TZEcEvent do
        begin
          if not TryEncodeDateTime(CurrentYear, rTime.nMonth, rTime.nDay,
              rTime.nHour, rTime.nMinute, rTime.nSecond, 0, m_dtDate) then
            m_dtDate := 0
          else if CompareDateTime(Now, m_dtDate) < 0 then
            m_dtDate := IncYear(m_dtDate, -1);
          m_nSubEv := nEcSubEv;
          m_nPowerFlags := nFlags;
        end;
      end;
      ZG_EV_FIRE_STATE:
      begin
        ACtr.DecodeFireEvent(AInfo.aData, rTime, nFireSubEv, nFlags);
        Result := TZFireEvent.Create();
        with Result as TZFireEvent do
        begin
          if not TryEncodeDateTime(CurrentYear, rTime.nMonth, rTime.nDay,
              rTime.nHour, rTime.nMinute, rTime.nSecond, 0, m_dtDate) then
            m_dtDate := 0
          else if CompareDateTime(Now, m_dtDate) < 0 then
            m_dtDate := IncYear(m_dtDate, -1);
          m_nSubEv := nFireSubEv;
          m_nFireFlags := nFlags;
        end;
      end;
      ZG_EV_SECUR_STATE:
      begin
        ACtr.DecodeSecurEvent(AInfo.aData, rTime, nSecurSubEv, nFlags);
        Result := TZSecurEvent.Create();
        with Result as TZSecurEvent do
        begin
          if not TryEncodeDateTime(CurrentYear, rTime.nMonth, rTime.nDay,
              rTime.nHour, rTime.nMinute, rTime.nSecond, 0, m_dtDate) then
            m_dtDate := 0
          else if CompareDateTime(Now, m_dtDate) < 0 then
            m_dtDate := IncYear(m_dtDate, -1);
          m_nSubEv := nSecurSubEv;
          m_nSecurFlags := nFlags;
        end;
      end;
      ZG_EV_MODE_STATE:
      begin
        ACtr.DecodeModeEvent(AInfo.aData, rTime, nMode, nModeSubEv);
        Result := TZModeEvent.Create();
        with Result as TZModeEvent do
        begin
          if not TryEncodeDateTime(CurrentYear, rTime.nMonth, rTime.nDay,
              rTime.nHour, rTime.nMinute, rTime.nSecond, 0, m_dtDate) then
            m_dtDate := 0
          else if CompareDateTime(Now, m_dtDate) < 0 then
            m_dtDate := IncYear(m_dtDate, -1);
          m_nMode := nMode;
          m_nSubEv := nModeSubEv;
        end;
      end;
      ZG_EV_UNKNOWN_KEY:
      begin
        ACtr.DecodeUnkKeyEvent(AInfo.aData, rKeyNum);
        Result := TZKeyEvent.Create();
        (Result as TZKeyEvent).m_rKeyNum := rKeyNum;
      end;
      ZG_EV_HOTEL40..ZG_EV_HOTEL41:
      begin
        ACtr.DecodeHotelEvent(AInfo.aData, rTime, nHMode, nHSubEv, nFlags);
        Result := TZHotelEvent.Create();
        with Result as TZHotelEvent do
        begin
          if not TryEncodeDateTime(CurrentYear, rTime.nMonth, rTime.nDay,
              rTime.nHour, rTime.nMinute, rTime.nSecond, 0, m_dtDate) then
            m_dtDate := 0;
          m_nMode := nHMode;
          m_nSubEv := nHSubEv;
          m_nFlags := nFlags;
        end;
      end;
      else
      begin
        ACtr.DecodePassEvent(AInfo.aData, rTime, _nDirect, _nKeyIdx, _nKeyBank);
        Result := TZPassEvent.Create();
        with Result as TZPassEvent do
        begin
          if not TryEncodeDateTime(CurrentYear, rTime.nMonth, rTime.nDay,
              rTime.nHour, rTime.nMinute, rTime.nSecond, 0, m_dtDate) then
            m_dtDate := 0
          else if CompareDate(m_dtDate, Now) > 0 then
            m_dtDate := IncYear(m_dtDate, -1);
          m_nDirect := _nDirect;
          m_nKeyIdx := _nKeyIdx;
          m_nKeyBank := _nKeyBank;
          m_fKeyNumValid := False;
        end;
      end;
    end;
    Result.m_nEvIdx := AEvIdx;
    Result.m_nEvCode := AInfo.nCode;
    Result.m_nType := AInfo.nType;
    AList.Add(Result);
  except
    Result.Free();
    raise;
  end;
end;

function GetTickSpan(AOld, ANew: Cardinal): Cardinal;
begin
  {This is just in case the TickCount rolled back to zero}
  if ANew >= AOld then
    Result := (ANew - AOld)
  else
    Result := (High(LongWord) - AOld + ANew);
end;

end.
