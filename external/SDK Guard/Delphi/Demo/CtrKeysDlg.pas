unit CtrKeysDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Menus, Contnrs,
  ZGuard, ZBase, ZGClasses, uUtils;

type
  TZKey = class
    m_nIdx          : Integer;          // Индекс ключа в банке
    m_Num           : TZ_KeyNum;        // Номер ключа
    m_nType         : TZG_Ctr_Key_Type; // Тип ключа
    m_nAccess       : Cardinal;         // Маска временных зон
    m_nFlags        : Cardinal;         // Флаги (бит 5 - короткий номер)
  end;
  TZKeyList = TObjectList;

  TfmCtrKeys = class(TForm)
    grKeys: TGroupBox;
    grKeysOut: TGroupBox;
    lvKeys: TListView;
    lvKeysOut: TListView;
    btnClearAll: TButton;
    btnReadAll: TButton;
    btnRemove: TButton;
    btnChange: TButton;
    btnRemoveOut: TButton;
    btnChangeOut: TButton;
    btnReadAllOut: TButton;
    btnClearAllOut: TButton;
    btnAdd: TButton;
    btnAddOut: TButton;
    PopupMenu1: TPopupMenu;
    miImportCSV1: TMenuItem;
    OpenDialogCSV1: TOpenDialog;
    PopupMenu2: TPopupMenu;
    miImportCSV2: TMenuItem;
    miCopyToBankOut: TMenuItem;
    miReadAll1: TMenuItem;
    N1: TMenuItem;
    miClearAll1: TMenuItem;
    miReadAll2: TMenuItem;
    N2: TMenuItem;
    miClearAll2: TMenuItem;
    N3: TMenuItem;
    miImportExcel1: TMenuItem;
    OpenDialogXls1: TOpenDialog;
    miImportExcel2: TMenuItem;
    Numberformat1: TMenuItem;
    miNumFmtAuto: TMenuItem;
    miNumFmtEm: TMenuItem;
    miNumFmtDS: TMenuItem;
    N4: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure lvKeysData(Sender: TObject; Item: TListItem);
    procedure btnRemoveClick(Sender: TObject);
    procedure btnClearAllClick(Sender: TObject);
    procedure lvKeysOutData(Sender: TObject; Item: TListItem);
    procedure btnReadAllClick(Sender: TObject);
    procedure btnReadAllOutClick(Sender: TObject);
    procedure btnClearAllOutClick(Sender: TObject);
    procedure btnChangeClick(Sender: TObject);
    procedure btnChangeOutClick(Sender: TObject);
    procedure btnRemoveOutClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnAddOutClick(Sender: TObject);
    procedure miImportCSV1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure miImportCSV2Click(Sender: TObject);
    procedure miCopyToBankOutClick(Sender: TObject);
    procedure miReadAll1Click(Sender: TObject);
    procedure miClearAll1Click(Sender: TObject);
    procedure miReadAll2Click(Sender: TObject);
    procedure miClearAll2Click(Sender: TObject);
    procedure miImportExcel1Click(Sender: TObject);
    procedure miImportExcel2Click(Sender: TObject);
    procedure miNumFmtAutoClick(Sender: TObject);
    procedure miNumFmtDSClick(Sender: TObject);
    procedure miNumFmtEmClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure lvKeysDblClick(Sender: TObject);
    procedure lvKeysOutDblClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    m_nMaxBanks     : Integer;
    m_nMaxKeys      : Integer;
    m_fProximity    : Boolean;
    m_aKeys         : TZKeyList;
    m_aKeysOut      : TZKeyList;
    m_fScheduleInit : Boolean;
    m_nNumFmt       : TNumFormat;

    procedure DoReadAll(ABankN: Integer=0);
    procedure DoWriteAll(ABankN: Integer=0);
    procedure DoClearAll(ABankN: Integer=0);
    procedure DoAdd(ABankN: Integer=0);
    procedure DoChange(ABankN: Integer=0);
    procedure DoRemove(ABankN: Integer=0);
    procedure DoImportCsv(Const AFileName: String; ABankN: Integer=0);
    procedure DoImportXls(Const AFileName: String; ABankN: Integer=0);
    procedure SetNumFmt(AFmt: TNumFormat);
    procedure UpdateView(ABankN: Integer);
  public
    { Public declarations }
    FCtr            : TZController;
  end;

var
  fmCtrKeys: TfmCtrKeys;

implementation

{$R *.dfm}

uses
  Math, ComObj, uConst, uProcessDlg, CtrKeyDlg;


procedure TfmCtrKeys.DoAdd(ABankN: Integer);
var
  nKeyIdx, i, nNext: Integer;
  rKey: TZG_Ctr_Key;
  pArr: TZKeyList;
  oLV: TListView;
  oKey: TZKey;
begin
  if ABankN = 1 then
  begin
    pArr := m_aKeysOut;
    oLV := lvKeysOut;
  end
  else
  begin
    pArr := m_aKeys;
    oLV := lvKeys;
  end;
  // Определение позиции для нового ключа
  nKeyIdx := -1;
  nNext := 0;
  for i := 0 to pArr.Count - 1 do
  begin
    if TZKey(pArr[i]).m_nIdx <> nNext then
    begin
      nKeyIdx := nNext;
      break;
    end;
    Inc(nNext);
  end;
  if nKeyIdx = -1 then
  begin
    nKeyIdx := FCtr.GetKeyTopIndex(ABankN);
    if nKeyIdx >= m_nMaxKeys then
    begin
      MessageDlg(StrEBankFull, mtError, [mbOk], 0);
      exit;
    end;
  end;
  // В качестве номера ключа по умолчанию используем последний поднесенный ключ к считывателю
  FCtr.ReadLastKeyNum(rKey.rNum);
  with fmCtrKey do
  begin
    m_rNum := rKey.rNum;
    m_nType := ZG_KEY_NORMAL;
    m_nAccess := $FF;
    m_nFlags := 0;
    m_nMaxBanks := Self.m_nMaxBanks;
    m_fProximity := Self.m_fProximity;
    if not m_fScheduleInit then
    begin
      FCtr.ReadTimeZones(0, @m_aSchedule[0], Length(m_aSchedule), nil, nil);
      if m_nMaxBanks = 2 then
        FCtr.ReadTimeZones(0, @m_aScheduleOut[0], Length(m_aScheduleOut), nil, nil, 1);
      m_fScheduleInit := True;
    end;
    m_nKeyIdx := nKeyIdx;
  end;
  if fmCtrKey.ShowModal() <> mrOk then
    exit;
  FillChar(rKey, SizeOf(rKey), 0);
  with rKey do
  begin
    rNum := fmCtrKey.m_rNum;
    nType := fmCtrKey.m_nType;
    nAccess := fmCtrKey.m_nAccess;
    nFlags := fmCtrKey.m_nFlags;
  end;
  FCtr.WriteKeys(nKeyIdx, @rKey, 1, nil, nil, ABankN);
  oKey := TZKey.Create();
  with oKey do
  begin
    m_nIdx := nKeyIdx;
    m_Num := rKey.rNum;
    m_nType := rKey.nType;
    m_nAccess := rKey.nAccess;
    m_nFlags := rKey.nFlags;
  end;
  pArr.Add(oKey);
  oLV.Items.Count := pArr.Count;
  oLV.Invalidate();
end;

procedure TfmCtrKeys.btnAddClick(Sender: TObject);
begin
  DoAdd(0);
end;

procedure TfmCtrKeys.btnAddOutClick(Sender: TObject);
begin
  DoAdd(1);
end;

procedure TfmCtrKeys.DoChange(ABankN: Integer);
var
  nIdx, nKeyIdx: Integer;
  p: TZKey;
  rKey: TZG_Ctr_Key;
  pArr: TZKeyList;
  oLV: TListView;
begin
  if ABankN = 1 then
  begin
    pArr := m_aKeysOut;
    oLV := lvKeysOut;
  end
  else
  begin
    pArr := m_aKeys;
    oLV := lvKeys;
  end;
  nIdx := oLV.ItemIndex;
  if nIdx = -1 then
    exit;
  p := TZKey(pArr[nIdx]);
  nKeyIdx := p.m_nIdx;
  with fmCtrKey do
  begin
    m_rNum := p.m_Num;
    m_nType := p.m_nType;
    m_nAccess := p.m_nAccess;
    m_nFlags := p.m_nFlags;
    m_nMaxBanks := Self.m_nMaxBanks;
    m_fProximity := Self.m_fProximity;
    if not m_fScheduleInit then
    begin
      FCtr.ReadTimeZones(0, @m_aSchedule[0], Length(m_aSchedule), nil, nil);
      if m_nMaxBanks = 2 then
        FCtr.ReadTimeZones(0, @m_aScheduleOut[0], Length(m_aScheduleOut), nil, nil, 1);
      m_fScheduleInit := True;
    end;
    m_nKeyIdx := nKeyIdx;
  end;
  if fmCtrKey.ShowModal() <> mrOk then
    exit;
  FillChar(rKey, SizeOf(rKey), 0);
  with rKey do
  begin
    rNum := fmCtrKey.m_rNum;
    nType := fmCtrKey.m_nType;
    nAccess := fmCtrKey.m_nAccess;
    nFlags := fmCtrKey.m_nFlags;
  end;
  FCtr.WriteKeys(nKeyIdx, @rKey, 1, nil, nil, ABankN);
  with p do
  begin
    m_Num := rKey.rNum;
    m_nType := rKey.nType;
    m_nAccess := rKey.nAccess;
    m_nFlags := rKey.nFlags;
  end;
  oLV.Invalidate();
end;

procedure TfmCtrKeys.btnChangeClick(Sender: TObject);
begin
  DoChange();
end;

procedure TfmCtrKeys.btnChangeOutClick(Sender: TObject);
begin
  DoChange(1);
end;

function ClearKeysCB(APos: Integer; AMax: Integer; AUserData: Pointer): LongBool; stdcall;
var
  nPct: Integer;
begin
  nPct := (APos * 100) div AMax;
  fmProcess.SetPct(nPct);
  Result := not fmProcess.m_fCancelled;
end;

procedure TfmCtrKeys.DoClearAll(ABankN: Integer);
var
  pArr: TZKeyList;
  oLV: TListView;
begin
  if ABankN = 1 then
  begin
    pArr := m_aKeysOut;
    oLV := lvKeysOut;
  end
  else
  begin
    pArr := m_aKeys;
    oLV := lvKeys;
  end;
  oLV.Items.Count := 0;
  Enabled := False;
  try
    fmProcess.Init(Self, format(StrClearKeys_DD, [ABankN]));
    pArr.Clear();
    FCtr.ClearKeys(0, MAXINT, ClearKeysCB, Pointer(ABankN), ABankN);
  finally
    Enabled := True;
    fmProcess.Hide();
    oLV.Items.Count := pArr.Count;
    oLV.Invalidate();
  end;
end;

procedure TfmCtrKeys.btnClearAllClick(Sender: TObject);
begin
  if MessageDlg(StrReally, mtConfirmation, [mbYes, mbNo, mbCancel], 0) <> mrYes then
    exit;
  DoClearAll();
end;

procedure TfmCtrKeys.btnClearAllOutClick(Sender: TObject);
begin
  if MessageDlg(StrReally, mtConfirmation, [mbYes, mbNo, mbCancel], 0) <> mrYes then
    exit;
  DoClearAll(1);
end;

procedure TfmCtrKeys.DoRemove(ABankN: Integer);
var
  nIdx: Integer;
  pArr: TZKeyList;
  oLV: TListView;
begin
  if ABankN = 1 then
  begin
    pArr := m_aKeysOut;
    oLV := lvKeysOut;
  end
  else
  begin
    pArr := m_aKeys;
    oLV := lvKeys;
  end;
  nIdx := oLV.ItemIndex;
  if nIdx = -1 then
    exit;
  FCtr.ClearKeys(TZKey(pArr[nIdx]).m_nIdx, 1, nil, nil, ABankN);
  pArr.Delete(nIdx);
  oLV.Items.Count := pArr.Count;
  oLV.Invalidate();
end;

procedure TfmCtrKeys.btnRemoveClick(Sender: TObject);
begin
  if MessageDlg(StrReally, mtConfirmation, [mbYes, mbNo, mbCancel], 0) <> mrYes then
    exit;
  DoRemove();
end;

procedure TfmCtrKeys.btnRemoveOutClick(Sender: TObject);
begin
  if MessageDlg(StrReally, mtConfirmation, [mbYes, mbNo, mbCancel], 0) <> mrYes then
    exit;
  DoRemove(1);
end;

procedure TfmCtrKeys.btnReadAllClick(Sender: TObject);
begin
  DoReadAll();
end;

procedure TfmCtrKeys.btnReadAllOutClick(Sender: TObject);
begin
  DoReadAll(1);
end;

function EnumKeysCB(AIdx: Integer; AKey: PZG_CTR_KEY; APos, AMax: Integer; AUserData: Pointer): LongBool; stdcall;
var
  nBankN, nPct: Integer;
  pKeys: TZKeyList;
  oKey: TZKey;
begin
  nBankN := Integer(AUserData);
  nPct := (APos * 100) div AMax;
  if not AKey.fErased then
  begin
    if nBankN = 1 then
      pKeys := fmCtrKeys.m_aKeysOut
    else
      pKeys := fmCtrKeys.m_aKeys;
    oKey := TZKey.Create();
    with oKey do
    begin
      m_nIdx := AIdx;
      m_Num := AKey.rNum;
      m_nType := AKey.nType;
      m_nAccess := AKey.nAccess;
      m_nFlags := AKey.nFlags;
    end;
    pKeys.Add(oKey);
  end;
  fmProcess.SetPct(nPct);
  Result := not fmProcess.m_fCancelled;
end;

procedure TfmCtrKeys.DoReadAll(ABankN: Integer);
var
  pArr: TZKeyList;
  oLV: TListView;
begin
  if ABankN = 1 then
  begin
    pArr := m_aKeysOut;
    oLV := lvKeysOut;
  end
  else
  begin
    pArr := m_aKeys;
    oLV := lvKeys;
  end;
  oLV.Items.Count := 0;
  Enabled := False;
  try
    fmProcess.Init(Self, format(StrReadKeys_DD, [ABankN]));
    pArr.Clear();
    FCtr.EnumKeys(0, EnumKeysCB, Pointer(ABankN), ABankN);
  finally
    Enabled := True;
    fmProcess.Hide();
    UpdateView(ABankN);
  end;
end;

function WriteKeyCb(APos: Integer; AMax: Integer; AUserData: Pointer): LongBool; stdcall;
var
  nPct: Integer;
begin
  nPct := (APos * 100) div AMax;
  fmProcess.SetPct(nPct);
  Result := not fmProcess.m_fCancelled;
end;

procedure TfmCtrKeys.DoWriteAll(ABankN: Integer);
var
  pArr: TZKeyList;
  oLV: TListView;
  a: array of TZG_Ctr_Key;
  i, nMax: Integer;
  pKey: TZKey;
begin
  if ABankN = 1 then
  begin
    pArr := m_aKeysOut;
    oLV := lvKeysOut;
  end
  else
  begin
    pArr := m_aKeys;
    oLV := lvKeys;
  end;
  oLV.Items.Count := 0;
  Enabled := False;
  try
    fmProcess.Init(Self, format(StrWriteKeys_DD, [ABankN]));
    nMax := FCtr.GetKeyTopIndex(ABankN);
    for i := 0 to pArr.Count - 1 do
    begin
      pKey := pArr[i] as TZKey;
      if pKey.m_nIdx >= nMax then
        nMax := (pKey.m_nIdx + 1);
    end;
    SetLength(a, nMax);
    for i := 0 to Length(a) - 1 do
      a[i].fErased := True;
    for i := 0 to pArr.Count - 1 do
    begin
      pKey := pArr[i] as TZKey;
      with a[pKey.m_nIdx] do
      begin
        fErased := False;
        rNum := pKey.m_Num;
        nType := pKey.m_nType;
        nFlags := pKey.m_nFlags;
        nAccess := pKey.m_nAccess;
      end;
    end;
    FCtr.WriteKeys(0, @a[0], Length(a), WriteKeyCb, Pointer(ABankN), ABankN);
  finally
    Enabled := True;
    fmProcess.Hide();
    oLV.Items.Count := pArr.Count;
    oLV.Invalidate();
  end;
end;

procedure TfmCtrKeys.FormCreate(Sender: TObject);
begin
  m_aKeys := TZKeyList.Create();
  m_aKeysOut := TZKeyList.Create();
end;

procedure TfmCtrKeys.FormDestroy(Sender: TObject);
begin
  m_aKeys.Free();
  m_aKeysOut.Free();
end;

procedure TfmCtrKeys.FormResize(Sender: TObject);
var
  n: Integer;
begin
  if m_nMaxBanks = 2 then
  begin
    n := (ClientWidth div 2);
    grKeys.Width := n;
    grKeysOut.Width := (ClientWidth - n);
    grKeys.Height := ClientHeight;
    grKeysOut.Height := ClientHeight;
  end
  else
  begin
    grKeys.Width := ClientWidth;
    grKeys.Height := ClientHeight;
  end;
end;

procedure TfmCtrKeys.FormShow(Sender: TObject);
var
  rInfo: TZG_Ctr_Info;
begin
  FillChar(rInfo, SizeOf(rInfo), 0);
  FCtr.GetInformation(rInfo);
  if (rInfo.nFlags and ZG_CTR_F_2BANKS) <> 0 then
    m_nMaxBanks := 2
  else
    m_nMaxBanks := 1;
  m_nMaxKeys := rInfo.nMaxKeys;
  m_fProximity := (rInfo.nFlags and ZG_CTR_F_PROXIMITY) <> 0;
  m_fScheduleInit := False;

  if m_nMaxBanks = 2 then
  begin
//    ClientWidth := (grKeysOut.Left + grKeysOut.Width + 4);
    grKeysOut.Visible := True;
    miCopyToBankOut.Enabled := True;
  end
  else
  begin
//    ClientWidth := (grKeysOut.Left - 4);
    grKeysOut.Visible := False;
    miCopyToBankOut.Enabled := False;
  end;
  DoReadAll();
  if m_nMaxBanks = 2 then
    DoReadAll(1);
end;

procedure TfmCtrKeys.lvKeysData(Sender: TObject; Item: TListItem);
var
  nIdx: Integer;
  pK: TZKey;
  f: Boolean;
begin
  nIdx := Item.Index;

  Item.Caption := IntToStr(nIdx + 1);
  while Item.SubItems.Count < 3 do
    Item.SubItems.Add('');

  ASSERT((nIdx >= 0) and (nIdx < m_aKeys.Count));
  pK := TZKey(m_aKeys[nIdx]);
  case m_nNumFmt of
    nfEmMarine: f := True;
    nfDallas: f := False;
    else f := m_fProximity;
  end;
  if (pK.m_nFlags and ZG_KF_DUAL) <> 0 then
    Item.SubItems[0] := format('(%s+%s)[%s+%s]', [
      KeyNumToStr(pK.m_Num, f, 0),
      KeyNumToStr(pK.m_Num, f, 1),
      ZKeyCodeToStr(PInteger(@pK.m_Num[1])^ and $FFFFFF, '#'),
      ZKeyCodeToStr(PInteger(@pK.m_Num[4])^ and $FFFFFF, '#')
    ])
  else
    Item.SubItems[0] := KeyNumToStr(pK.m_Num, f);
  Item.SubItems[1] := KeyTypeStrs[pK.m_nType];
  Item.SubItems[2] := KeyAccessToStr(pK.m_nAccess);
end;

procedure TfmCtrKeys.lvKeysDblClick(Sender: TObject);
begin
  DoChange(0);
end;

procedure TfmCtrKeys.lvKeysOutData(Sender: TObject; Item: TListItem);
var
  nIdx: Integer;
  pK: TZKey;
  f: Boolean;
begin
  nIdx := Item.Index;

  Item.Caption := IntToStr(nIdx + 1);
  while Item.SubItems.Count < 3 do
    Item.SubItems.Add('');

  ASSERT((nIdx >= 0) and (nIdx < m_aKeysOut.Count));
  pK := TZKey(m_aKeysOut[nIdx]);
  case m_nNumFmt of
    nfEmMarine: f := True;
    nfDallas: f := False;
    else f := m_fProximity;
  end;
  if (pK.m_nFlags and ZG_KF_DUAL) <> 0 then
    Item.SubItems[0] := format('(%s+%s)[%s+%s]', [
      KeyNumToStr(pK.m_Num, f, 0),
      KeyNumToStr(pK.m_Num, f, 1),
      ZKeyCodeToStr(PInteger(@pK.m_Num[1])^ and $FFFFFF, '#'),
      ZKeyCodeToStr(PInteger(@pK.m_Num[4])^ and $FFFFFF, '#')
    ])
  else
    Item.SubItems[0] := KeyNumToStr(pK.m_Num, f);
  Item.SubItems[1] := KeyTypeStrs[pK.m_nType];
  Item.SubItems[2] := KeyAccessToStr(pK.m_nAccess);
end;

procedure TfmCtrKeys.lvKeysOutDblClick(Sender: TObject);
begin
  DoChange(1);
end;

procedure TfmCtrKeys.DoImportCsv(Const AFileName: String; ABankN: Integer);
var
  pArr: TZKeyList;
  oLV: TListView;
  lst: TStringList;
  i, n: Integer;
  s, ss: String;
  p: PChar;
  aKeyNum: TZ_KeyNum; // Номер ключа
  nFlags: Cardinal; // флаги
  nType: TZG_Ctr_Key_Type; // Тип ключа
  nAccess: Cardinal; // маска временных зон
  oKey: TZKey;
begin
  if ABankN = 1 then
  begin
    pArr := m_aKeysOut;
    oLV := lvKeysOut;
  end
  else
  begin
    pArr := m_aKeys;
    oLV := lvKeys;
  end;
  lst := TStringList.Create();
  try
    lst.LoadFromFile(AFileName);
    pArr.Clear();
    for i := 0 to lst.Count - 1 do
    begin
      s := lst[i];
      p := PChar(s);

      // idx
      ss := NextCsvField(p);
      if p = nil then
        break;
      // KeyNum
      ss := NextCsvField(p);
      if (ss <> '') and (ss[1] = '''') then
        Delete(ss, 1, 1);
      if not ParseKeyNum(aKeyNum, ss) then
        continue;
      nFlags := 0;
      if aKeyNum[0] = 3 then
        Inc(nFlags, ZG_KF_SHORTNUM); // помечаю, что ключ короткий
          // Type abbr
      ss := NextCsvField(p);
      if (ss = 'B') or (ss = 'Б') then
        nType := ZG_KEY_BLOCKING
      else if (ss = 'M') or (ss = 'М') then
        nType := ZG_KEY_MASTER
      else
        nType := ZG_KEY_NORMAL;

      // TimeZones
      ss := NextCsvField(p);
      if (ss = 'Never') or (ss = 'Никогда') then
        nAccess := 0
      else if (ss = 'Ever') or (ss = 'Всегда') then
        nAccess := $ff
      else
      begin
        nAccess := 0;
        for n := 1 to min(7, Length(ss)) do
        begin
          if ss[n] <> '-' then
            Inc(nAccess, (1 shl (n - 1)));
        end;
      end;
      oKey := TZKey.Create();
      with oKey do
      begin
        m_nIdx := m_aKeys.Count;
        m_Num := aKeyNum;
        m_nType := nType;
        m_nAccess := nAccess;
        m_nFlags := nFlags;
      end;
      pArr.Add(oKey);
    end;
  finally
    lst.Free();
    oLV.Items.Count := pArr.Count;
    oLV.Invalidate();
  end;
  DoWriteAll(ABankN);
end;

procedure TfmCtrKeys.miClearAll1Click(Sender: TObject);
begin
  DoClearAll();
end;

procedure TfmCtrKeys.miClearAll2Click(Sender: TObject);
begin
  DoClearAll(1);
end;

procedure TfmCtrKeys.miCopyToBankOutClick(Sender: TObject);
var
  i: Integer;
  pKey: TZKey;
begin
  try
    m_aKeysOut.Clear();
    m_aKeysOut.Count := m_aKeys.Count;
    for i := 0 to m_aKeysOut.Count - 1 do
    begin
      pKey := TZKey(m_aKeys[i]);
      m_aKeysOut[i] := TZKey.Create();
      with TZKey(m_aKeysOut[i]) do
      begin
        m_nIdx := pKey.m_nIdx;
        m_Num := pKey.m_Num;
        m_nType := pKey.m_nType;
        m_nAccess := pKey.m_nAccess;
        m_nFlags := pKey.m_nFlags;
      end;
    end;
  finally
    lvKeysOut.Items.Count := m_aKeysOut.Count;
    lvKeysOut.Invalidate();
  end;
  DoWriteAll(1);
end;

procedure TfmCtrKeys.miImportCSV1Click(Sender: TObject);
begin
  if not OpenDialogCSV1.Execute(Handle) then
    exit;
  DoImportCsv(OpenDialogCSV1.FileName, 0);
end;

procedure TfmCtrKeys.miImportCSV2Click(Sender: TObject);
begin
  if not OpenDialogCSV1.Execute(Handle) then
    exit;
  DoImportCsv(OpenDialogCSV1.FileName, 1);
end;

procedure TfmCtrKeys.DoImportXls(Const AFileName: String; ABankN: Integer);
Const
  xlCellTypeLastCell = 11; // - число которое определяет последнюю незаполненную ячейку.
var
  pArr: TZKeyList;
  oLV: TListView;
  XLApp, Sheet: OLEVariant;
  RangeMatrix: Variant;
  x, y, i, n: Integer;
  s: String;
  aKeyNum: TZ_KeyNum; // Номер ключа
  nFlags: Cardinal; // флаги
  nType: TZG_Ctr_Key_Type; // Тип ключа
  nAccess: Cardinal; // маска временных зон
  oKey: TZKey;
begin
  if ABankN = 1 then
  begin
    pArr := m_aKeysOut;
    oLV := lvKeysOut;
  end
  else
  begin
    pArr := m_aKeys;
    oLV := lvKeys;
  end;
  XLApp := CreateOleObject('Excel.Application');
  try
    // Hide Excel
    XLApp.Visible := False;

    // Open the Workbook
    XLApp.Workbooks.Open(AFilename);

    // Sheet := XLApp.Workbooks[1].WorkSheets[1];
    Sheet := XLApp.Workbooks[ExtractFileName(AFilename)].WorkSheets[1];

    // In order to know the dimension of the WorkSheet, i.e the number of rows
    // and the number of columns, we activate the last non-empty cell of it

    Sheet.Cells.SpecialCells(xlCellTypeLastCell, EmptyParam).Activate;
    // Get the value of the last row
    x := XLApp.ActiveCell.Row;
    // Get the value of the last column
    y := XLApp.ActiveCell.Column;

    // Assign the Variant associated with the WorkSheet to the Delphi Variant
    RangeMatrix := XLApp.Range['A1', XLApp.Cells.Item[X, Y]].Value;

    for i := 2 to x do
    begin
      // Keynum
      s := RangeMatrix[i, 2];
      if not ParseKeyNum(aKeyNum, s) then
        continue;
      nFlags := 0;
      if aKeyNum[0] = 3 then
        Inc(nFlags, ZG_KF_SHORTNUM); // помечаю, что ключ короткий
      // Type abbr
      s := RangeMatrix[i, 3];
      if (s = 'B') or (s = 'Б') then
        nType := ZG_KEY_BLOCKING
      else if (s = 'M') or (s = 'М') then
        nType := ZG_KEY_MASTER
      else
        nType := ZG_KEY_NORMAL;

      // TimeZones
      s := RangeMatrix[i, 4];
      if (s = 'Never') or (s = 'Никогда') then
        nAccess := 0
      else if (s = 'Ever') or (s = 'Всегда') then
        nAccess := $ff
      else
      begin
        nAccess := 0;
        for n := 1 to min(7, Length(s)) do
        begin
          if s[n] <> '-' then
            Inc(nAccess, (1 shl (n - 1)));
        end;
      end;
      oKey := TZKey.Create();
      with oKey do
      begin
        m_nIdx := m_aKeys.Count;
        m_Num := aKeyNum;
        m_nType := nType;
        m_nAccess := nAccess;
        m_nFlags := nFlags;
      end;
      pArr.Add(oKey);
    end;
    // Unassign the Delphi Variant Matrix
    RangeMatrix := Unassigned;
  finally
    // Quit Excel
    if not VarIsEmpty(XLApp) then
    begin
      XLApp.Quit;
      XLAPP := Unassigned;
      Sheet := Unassigned;
    end;
    oLV.Items.Count := pArr.Count;
    oLV.Invalidate();
  end;
  DoWriteAll(ABankN);
end;

procedure TfmCtrKeys.miImportExcel1Click(Sender: TObject);
begin
  if not OpenDialogXls1.Execute(Handle) then
    exit;
  DoImportXls(OpenDialogXls1.FileName);
end;

procedure TfmCtrKeys.miImportExcel2Click(Sender: TObject);
begin
  if not OpenDialogXls1.Execute(Handle) then
    exit;
  DoImportXls(OpenDialogXls1.FileName, 1);
end;

procedure TfmCtrKeys.UpdateView(ABankN: Integer);
var
  pArr: TZKeyList;
  oLV: TListView;
begin
  if ABankN = 1 then
  begin
    pArr := m_aKeysOut;
    oLV := lvKeysOut;
  end
  else
  begin
    pArr := m_aKeys;
    oLV := lvKeys;
  end;
  oLV.Items.Count := pArr.Count;
  oLV.Invalidate();
end;

procedure TfmCtrKeys.SetNumFmt(AFmt: TNumFormat);
begin
  if m_nNumFmt = AFmt then
    Exit;
  m_nNumFmt := AFmt;
  UpdateView(0);
  if m_nMaxBanks = 2 then
    UpdateView(1);
end;

procedure TfmCtrKeys.miNumFmtAutoClick(Sender: TObject);
begin
  SetNumFmt(nfAuto);
end;

procedure TfmCtrKeys.miNumFmtDSClick(Sender: TObject);
begin
  SetNumFmt(nfDallas);
end;

procedure TfmCtrKeys.miNumFmtEmClick(Sender: TObject);
begin
  SetNumFmt(nfEmMarine);
end;

procedure TfmCtrKeys.miReadAll1Click(Sender: TObject);
begin
  DoReadAll();
end;

procedure TfmCtrKeys.miReadAll2Click(Sender: TObject);
begin
  DoReadAll(1);
end;

procedure TfmCtrKeys.PopupMenu1Popup(Sender: TObject);
begin
  case m_nNumFmt of
    nfEmMarine: miNumFmtEm.Checked := True;
    nfDallas: miNumFmtDS.Checked := True;
    else miNumFmtAuto.Checked := True;
  end;
end;

end.
