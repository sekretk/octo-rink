program CtrUpdateKeys;

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils, Classes, Contnrs, StrUtils, Character,
  ZBase, ZGuard, ZPort, ZGClasses, Utils;

const
  CvtPortType = ZP_PORT_COM; // ��� �����
  CvtPortName: WideString = 'com12'; // ��� �����
  CtrAddr = 3; // ����� �����������

  KeysCacheFileName_D = 'ctr%d_keys.bin'; // ��� ����� ���� ������
var
  g_hCtr: THandle;  // ���������� �����������
  g_nSn: Integer;   // �/� �����������
  g_fWiegand: Boolean; // True - Wiegand, ����� - Dallas
  g_nMaxBanks: Integer; // ���������� ������
  g_nMaxKeys: Integer; // �������� ������
  g_nOptRead: Integer; // ���������� ������, ����������� �� 1 ������
  g_nOptWrite: Integer;// ���������� ������, ������������ �� 1 ������
Type
  TMyKey = class
    m_KeyNum        : TZ_KeyNum;
    m_nType         : TZG_Ctr_Key_Type;
    m_nAccess       : Integer;
  end;
  TDynZgKeyArray = array of TZG_Ctr_Key; // ������������ ������ ������ Sdk


// �������� ��� �����
procedure ShowKeys();
var
  sCacheName: String;
  i, j, nTop, nCount: Integer;
  aKeys: array of TZG_CTR_KEY;
  pKey: PZG_CTR_KEY;
  oFS: TFileStream;
begin
  SetLength(aKeys, g_nOptRead);
  sCacheName := format(KeysCacheFileName_D, [g_nSn]);
  if FileExists(sCacheName) then i := fmOpenWrite else i := fmCreate;
  oFS := TFileStream.Create(sCacheName, i);
  try
    for i := 0 to g_nMaxBanks - 1 do
    begin
      oFS.Seek(i * (g_nMaxKeys * SizeOf(TZg_Ctr_Key)), TSeekOrigin.soBeginning);
      Writeln('------------');
      Writeln(format('Bank %d:', [i]));
      CheckZGError(ZG_Ctr_GetKeyTopIndex(g_hCtr, nTop, i));
      if nTop = 0 then
        Writeln('List is empty.');
      for j := 0 to nTop - 1 do
      begin
        if (j mod Length(aKeys)) = 0 then
        begin
          nCount := (nTop - j);
          if nCount > Length(aKeys) then
            nCount := Length(aKeys);
          CheckZGError(ZG_Ctr_ReadKeys(g_hCtr, j, @aKeys[0], nCount, nil, nil, i));
          oFS.WriteBuffer(aKeys[0], nCount * SizeOf(TZG_Ctr_Key));
        end;
        pKey := @aKeys[j mod Length(aKeys)];
        if pKey.fErased then
          Writeln(format('%.4d empty', [j]))
        else
        begin
          Writeln(format('%.4d %s, %s, access: %.2Xh', [
            j,
            ZKeyNumToStr(pKey.rNum, g_fWiegand),
            KeyTypeStrs[pKey.nType],
            pKey.nAccess]));
        end;
      end;
      FillChar(aKeys[0], SizeOf(aKeys[0]), 0);
      aKeys[0].fErased := True;
      for j := nTop to g_nMaxKeys - 1 do
        oFS.WriteBuffer(aKeys[0], SizeOf(TZG_Ctr_Key));
    end;
  finally
    oFS.Free();
  end;
  Writeln('Done.');
end;

// �������� ��� ������
procedure DoClearKeyCache();
var
  nErr: Integer;
begin
  if not DeleteFile(format(KeysCacheFileName_D, [g_nSn])) then
  begin
    nErr := GetLastError();
    Writeln(format('%s (#%d)', [SysErrorMessage(nErr), nErr]));
    Readln;
    exit;
  end;
  Writeln('Done.');
end;

function TextToChar(Const AText: String; ACh: Char; var VPos: Integer): String;
var
  nNextPos: Integer;
begin
  nNextPos := PosEx(ACh, AText, VPos);
  if nNextPos = 0 then
    Result := Copy(AText, VPos, Length(AText) - VPos + 1)
  else
  begin
    Result := Copy(AText, VPos, nNextPos - VPos);
    Inc(nNextPos);
    if nNextPos > Length(AText) then nNextPos := 0;
  end;
  VPos := nNextPos;
end;

// ������������� ������ � ����� �����
function ParseKeyNum(var VKeyNum: TZ_KEYNUM; Const AText: String): Boolean;
var
  n, n2, i, j: Integer;
begin
  FillChar(VKeyNum, SizeOf(VKeyNum), 0);
  n := Pos(',', AText);
  if n <> 0 then
  begin
    n2 := n + 1;
    while (n2 <= Length(AText)) and IsNumber(AText[n2]) do
      Inc(n2);
    Dec(n2, n + 1);
    if n2 < 0 then
      Exit(False);
    PWord(@VKeyNum[1])^ := Word(StrToInt(Copy(AText, n + 1, n2)));
    n2 := n - 1;
    while (n2 >= 1) and IsNumber(AText[n2]) do
      Dec(n2);
    Dec(n, n2 + 1);
    if n < 0 then
      Exit(False);
    VKeyNum[3] := Byte(StrToInt(Copy(AText, n2 + 1, n)));
    VKeyNum[0] := 3;

    n := Pos('[', AText);
    if n <> 0 then
    begin
      Inc(n);
      n2 := PosEx(']', AText, n);
      if (n2 <> 0) and (n2 > n) and TryStrToInt('$' + Copy(AText, n, n2 - n), n) then
      begin
        PWord(@VKeyNum[4])^ := Word(n);
        VKeyNum[0] := 5;
      end;
    end;
  end
  else
  begin
    i := Length(AText) - 1;
    j := 1;
    while i >= 1 do
    begin
      if not TryStrToInt('$' + AText[i] + AText[i + 1], n) then
      begin
        Result := False;
        Exit;
      end;
      VKeyNum[j] := n and $FF;
      Inc(j);
      if j > 6 then
        break;
      Dec(i, 2);
    end;
    VKeyNum[0] := (j - 1);
  end;
  Result := True;
end;

// ��������� ����� ������ ������ �� ���������� �����
function GetNewList(VList: TObjectList): Boolean;
var
  s, s2: String;
  oSL: TStringList;
  i, nPos, nAccess: Integer;
  num: TZ_KeyNum;
  nType: TZG_Ctr_Key_Type;
  oKey: TMyKey;
begin
  Writeln('Enter keys filename:');
  ReadLn(s);
  if not FileExists(s) then
  begin
    Writeln('File not found.');
    Exit(False);
  end;
  oSL := TStringList.Create();
  try
    oSL.LoadFromFile(s);
    for i := 0 to oSL.Count - 1 do
    begin
      s := oSL[i]; // keynum; type (n/b/m); access (hex)
      nPos := 1;
      s2 := Trim(TextToChar(s, ';', nPos));
      if not ParseKeyNum(num, s2) then
        continue;
      nType := ZG_KEY_NORMAL;
      nAccess := $FF;
      if nPos <> 0 then
      begin
        s2 := Trim(TextToChar(s, ';', nPos));
        if s2 <> '' then
        begin
          case ToUpper(s2[1]) of
            'B': nType := ZG_KEY_BLOCKING;
            'M': nType := ZG_KEY_MASTER;
          end;
        end;
      end;
      if nPos <> 0 then
      begin
        s2 := Trim(TextToChar(s, ';', nPos));
        if s2 <> '' then
          nAccess := StrToInt('$' + s2);
      end;
      oKey := TMyKey.Create();
      try
        oKey.m_KeyNum := num;
        oKey.m_nType := nType;
        oKey.m_nAccess := nAccess;
        VList.Add(oKey);
      except
        oKey.Free();
        raise;
      end;
    end;
  finally
    oSL.Free();
  end;
  Writeln(format('Loaded %d keys. Continue [y/n]?', [VList.Count]));
  ReadLn(s);
  Result := (s <> '') and (ToUpper(s[1]) = 'Y');
end;

function ZgProcessCb(APos: Integer; AMax: Integer; AUserData: Pointer): LongBool; stdcall;
begin
  Write(format(#13'%5d / %d', [APos, AMax]));
  Result := True;
end;

// ��������� ������� ������ ������ �� ���� ��� �� �����������
procedure GetCtrList(var VList: TDynZgKeyArray);
var
  sCacheName: String;
  oFS: TFileStream;
  i, nTop, nPos, j: Integer;
  rCK: TZG_Ctr_Key;
begin
  SetLength(VList, g_nMaxKeys * g_nMaxBanks);
  sCacheName := format(KeysCacheFileName_D, [g_nSn]);
  if FileExists(sCacheName) then
  begin
    oFS := TFileStream.Create(sCacheName, fmOpenRead);
    try
      oFS.ReadBuffer(VList[0], g_nMaxKeys * g_nMaxBanks * SizeOf(TZG_Ctr_Key));
    finally
      oFS.Free();
    end;
  end
  else
  begin
    WriteLn('Load keys from controller...');
    oFS := TFileStream.Create(sCacheName, fmCreate);
    try
      for i := 0 to g_nMaxBanks - 1 do
      begin
        CheckZGError(ZG_Ctr_GetKeyTopIndex(g_hCtr, nTop, i));
        if nTop > 0 then
        begin
          nPos := (i * g_nMaxKeys);
          CheckZGError(ZG_Ctr_ReadKeys(g_hCtr, 0, @VList[nPos], nTop, ZgProcessCb, nil, i));
          oFS.WriteBuffer(VList[nPos], nTop * SizeOf(TZG_Ctr_Key));
        end;
        if nTop < g_nMaxKeys then
        begin
          FillChar(rCK, SizeOf(rCK), 0);
          rCK.fErased := True;
          for j := nTop to g_nMaxKeys - 1 do
          begin
            oFS.WriteBuffer(rCK, SizeOf(rCK));
            VList[j] := rCK;
          end;
        end;
      end;
    finally
      oFS.Free();
    end;
    WriteLn(' completed.');
  end;
end;

// ������� ��������� ��� ���������� ������ ������ �� �� �������
function MyKeysSortCompare1(Item1, Item2: Pointer): Integer;
begin
  Result := CompareZKeyNums(TMyKey(Item1).m_KeyNum, TMyKey(Item2).m_KeyNum);
end;

// �������� ����� ����� �� ��� ������
function BFindByNum(AList: TObjectList; Const ANum: TZ_KeyNum): Integer;
var
  L, H, I, C: Integer;
begin
  Result := -1;
  L := 0;
  H := AList.Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareZKeyNums(TMyKey(AList[I]).m_KeyNum, ANum);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := I;
        L := I;
      end;
    end;
  end;
end;

// ����� ��������� ������ � ������ ������ Sdk
function FindEraised(Const A: TDynZgKeyArray; AStart, ABank: Integer): Integer;
var
  n: Integer;
begin
  n := ABank * g_nMaxKeys;
  ASSERT((n + g_nMaxKeys) <= Length(A));
  for Result := n + AStart to n + g_nMaxKeys - 1 do
    if A[Result].fErased then
      Exit;
  Result := -1;
end;

// ���������� ������ ������ � ���������� � ����� ��������� � ���
procedure SetCtrList(Const A: TDynZgKeyArray; ASync: TBits);
var
  i, nIdx, nPos, nWIdx, nWPos, nWCnt, nEnd: Integer;
  oFS: TFileStream;
  fChanged: Boolean;
  sCacheName: String;
begin
  ASSERT(Length(A) = ASync.Size);
  ASSERT(Length(A) = (g_nMaxBanks * g_nMaxKeys));
  fChanged := False;
  // ���������� � ����������
  for i := 0 to g_nMaxBanks - 1 do
  begin
    nIdx := 0;
    nPos := (i * g_nMaxKeys);
    while nIdx < g_nMaxKeys do
    begin
      if ASync[nPos] then
      begin
        Inc(nIdx);
        Inc(nPos);
        continue;
      end;
      nWIdx := nIdx;
      nWPos := nPos;
      nWCnt := 1;
      Inc(nIdx);
      Inc(nPos);
      nEnd := (i + 1) * g_nMaxKeys;
      while nPos < nEnd do
      begin
        if (nPos - nWPos) >= g_nOptWrite then
          break;
        if not ASync[nPos] then
          nWCnt := (nPos - nWPos + 1);
        Inc(nIdx);
        Inc(nPos);
      end;

      if nWCnt > 0 then
      begin
        CheckZGError(ZG_Ctr_WriteKeys(g_hCtr, nWIdx, @A[nWPos], nWCnt, nil, nil, i, False));
        Writeln(format('Updated keys %d-%d (bank#%d)', [
            nWIdx, nWIdx + nWCnt - 1, i]));
        fChanged := True;
      end;
    end;
  end;
  if fChanged then
  begin
    // ��������� ��� ������ � �����
    sCacheName := format(KeysCacheFileName_D, [g_nSn]);
    if FileExists(sCacheName) then
      i := fmOpenWrite
    else
      i := fmCreate;
    oFS := TFileStream.Create(sCacheName, i);
    try
      oFS.WriteBuffer(A[0], Length(A) * SizeOf(TZG_Ctr_Key));
    finally
      oFS.Free();
    end;
  end
  else
    Writeln('List of keys of controller is not changed.');
end;

// ���������� ����� ������ ������ �� �����
procedure DoLoadKeysFromFile();
var
  oNewList: TObjectList;
  aCtrList: TDynZgKeyArray;
  i, nIdx, j: Integer;
  pCK: PZG_Ctr_Key;
  pMK: TMyKey;
  oSync: TBits;
  aNext: array[0..1] of Integer;
begin
  oNewList := TObjectList.Create();
  try
    if not GetNewList(oNewList) then
      Exit;
    GetCtrList(aCtrList);
    // ��������� ����� ������ �� ����������� ������� ����� ����� ���� ������ �� ������
    oNewList.Sort(MyKeysSortCompare1);
    oSync := TBits.Create();
    try
      oSync.Size := Length(aCtrList);
      // ������� �� CtrList �����, ������� ��� � NewList
      for i := 0 to Length(aCtrList) - 1 do
      begin
        pCK := @aCtrList[i];
        if pCK.fErased then
        begin
          oSync[i] := True;
          continue;
        end;
        nIdx := BFindByNum(oNewList, pCK.rNum);
        if nIdx <> -1 then
        begin
          pMK := TMyKey(oNewList[nIdx]);
          ASSERT(CompareZKeyNums(pMK.m_KeyNum, pCK.rNum) = 0);
          if (pCK.nType <> pMK.m_nType) or (pCK.nAccess <> Cardinal(pMK.m_nAccess)) then
          begin
            pCK.nType := pMK.m_nType;
            pCK.nAccess := pMK.m_nAccess;
          end
          else
            oSync[i] := True;
          oNewList.Delete(nIdx);
        end
        else
          pCK.fErased := True;
      end;
      // ��������� �� NewList � CtrList �����, ������� ��� � CtrList
      FillChar(aNext, SizeOf(aNext), 0);
      for i := 0 to oNewList.Count - 1 do
      begin
        pMK := TMyKey(oNewList[i]);
        for j := 0 to g_nMaxBanks - 1 do
        begin
          ASSERT(j < Length(aNext));
          nIdx := FindEraised(aCtrList, aNext[j], j);
          if nIdx = -1 then
          begin
            Writeln(format('Key list is full (bank:%d)', [j]));
            Readln;
            Exit;
          end;
          pCK := @aCtrList[nIdx];
          pCK.fErased := False;
          pCK.rNum := pMK.m_KeyNum;
          pCK.nType := pMK.m_nType;
          pCK.nAccess := pMK.m_nAccess;
          oSync[nIdx] := False;
          aNext[j] := (nIdx + 1);
        end;
      end;
      SetCtrList(aCtrList, oSync);
    finally
      oSync.Free();
    end;
  finally
    oNewList.Free();
  end;
  Writeln('Done.');
end;

// ��������� ����� � ����, �������� �� ����������� ��� �� ����
procedure DoSaveKeysToFile();
Const
  KeyTypeAbbrs: array[TZG_Ctr_Key_Type] of String = ('', 'N', 'B', 'M');
var
  s: String;
  aCtrList: TDynZgKeyArray;
  oSL: TStringList;
  i: Integer;
  pCK: PZG_Ctr_Key;
begin
  Writeln('Enter keys filename:');
  ReadLn(s);
  if s = '' then
    Exit;
  GetCtrList(aCtrList);
  oSL := TStringList.Create();
  try
    for i := 0 to g_nMaxKeys - 1 do
    begin
      pCK := @aCtrList[i];
      if pCK.fErased then
        continue;
      oSL.Add(format('%s; %s; %.2X', [
        ZKeyNumToStr(pCK.rNum, g_fWiegand),
        KeyTypeAbbrs[pCK.nType],
        pCK.nAccess
      ]));
    end;
    oSL.SaveToFile(s);
  finally
    oSL.Free();
  end;
  Writeln('Done.');
end;

procedure DoTest();
var
  hCvt: THandle;  // ���������
  rOp: TZG_CVT_OPEN_PARAMS;
  rCtrInfo: TZG_CTR_INFO;
  s: String;
begin
  SetConsoleOutputCP(1251); // ������������ ������� �� ������� �������� CP1251 (Windows-1251).

  if not CheckZGVersion() then
  begin
    Writeln('Wrong version SDK Guard.');
    Readln;
    exit;
  end;
  // �������������� ����������
  CheckZGError(ZG_Initialize(ZP_IF_NO_MSG_LOOP));
  hCvt := 0;
  g_hCtr := 0;
  try
    // ������������ � ����������
    FillCHar(rOp, SizeOf(rOp), 0);
    rOp.nType := CvtPortType;
    rOp.pszName := PWideChar(CvtPortName);
    rOp.nSpeed := ZG_SPEED_19200;
    CheckZGError(ZG_Cvt_Open(hCvt, @rOp));
    // ������������ � �����������
    FillChar(rCtrInfo, SizeOf(rCtrInfo), 0);
    CheckZGError(ZG_Ctr_Open(g_hCtr, hCvt, CtrAddr, 0, @rCtrInfo));
    // ���������� ��������� ��������� �����������
    g_nSn := rCtrInfo.nSn;
    g_fWiegand := (rCtrInfo.nFlags and ZG_CTR_F_PROXIMITY) <> 0;
    if (rCtrInfo.nFlags and ZG_CTR_F_2BANKS) <> 0 then
      g_nMaxBanks := 2
    else
      g_nMaxBanks := 1;
    g_nMaxKeys := rCtrInfo.nMaxKeys;
    g_nOptRead := rCtrInfo.nOptReadItems;
    g_nOptWrite := rCtrInfo.nOptWriteItems;
    // ������� �� ����� ���������� � �����������
    Writeln(format('%s addr: %d, s/n: %d, v%d.%d, Max_Banks: %d', [
        CtrTypeStrs[rCtrInfo.nType],
        rCtrInfo.nAddr,
        rCtrInfo.nSn,
        LoByte(rCtrInfo.nVersion), HiByte(rCtrInfo.nVersion),
        g_nMaxBanks]));

    Writeln('-----');
    repeat
      Writeln('Enter command number:');
      Writeln('1 - show keys'); // �������� ��� �����
      Writeln('6 - clear keys cache'); // �������� ��� ������ (�������� �����)
      Writeln('7 - set new keys from file...'); // ���������� ����� ������ �� ����� � ����������...
      Writeln('8 - save keys to file...'); // ��������� ����� � ����, �������� �� ����������� ��� �� ����
      Writeln('0 - quit'); // ��������� ���������
      ReadLn(s);
      Writeln;
      case StrToIntDef(s, 0) of
        1: ShowKeys();
        6: DoClearKeyCache();
        7: DoLoadKeysFromFile();
        8: DoSaveKeysToFile();
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
      ReadLn;
    end;
  end;
end.
