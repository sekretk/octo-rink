unit CtrEventsDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ZGuard, ZBase, ZGClasses, uUtils, Menus;

Const
  EV_WR_IDX_COLOR   = TColor($C0C0FF);
  EV_RD_IDX_COLOR   = TColor($C0FFC0);

type
  TfmCtrEvents = class(TForm)
    lvEvents: TListView;
    btnReadNew: TButton;
    btnReadAll: TButton;
    btnDump: TButton;
    PopupMenu1: TPopupMenu;
    Numberformat1: TMenuItem;
    miNumFmtAuto: TMenuItem;
    miNumFmtEm: TMenuItem;
    miNumFmtDS: TMenuItem;
    procedure btnReadAllClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnReadNewClick(Sender: TObject);
    procedure lvEventsData(Sender: TObject; Item: TListItem);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvEventsCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure btnDumpClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure miNumFmtAutoClick(Sender: TObject);
    procedure miNumFmtEmClick(Sender: TObject);
    procedure miNumFmtDSClick(Sender: TObject);
  private
    { Private declarations }
    m_nMaxBanks     : Integer;
    m_nMaxEvents    : Integer;
    m_fProximity    : Boolean;
    m_nOptReadItems : Integer;
    m_aEvents       : TZEventList;
    m_nEvWrIdx      : Integer;
    m_nEvRdIdx      : Integer;
    m_fAllEvents    : Boolean;
    m_nNumFmt       : TNumFormat;

    procedure DoReadAll();
    procedure DoReadNew();
    procedure ReadKeyNumsForEvents();
    procedure DoDump();
    procedure SetNumFmt(AFmt: TNumFormat);
    procedure UpdateView();
  public
    { Public declarations }
    FCvt            : TZConverter;
    FCtr            : TZController;
  end;

var
  fmCtrEvents: TfmCtrEvents;

implementation

{$R *.dfm}

uses
  DateUtils, Contnrs, Math,
  uConst, uProcessDlg;


procedure TfmCtrEvents.FormCreate(Sender: TObject);
begin
  m_aEvents := TZEventList.Create();
end;

procedure TfmCtrEvents.FormDestroy(Sender: TObject);
begin
  m_aEvents.Free();
end;

procedure TfmCtrEvents.FormShow(Sender: TObject);
var
  rInfo: TZG_Ctr_Info;
begin
  FillChar(rInfo, SizeOf(rInfo), 0);
  FCtr.GetInformation(rInfo);
  if (rInfo.nFlags and ZG_CTR_F_2BANKS) <> 0 then
    m_nMaxBanks := 2
  else
    m_nMaxBanks := 1;
  m_nMaxEvents := rInfo.nMaxEvents;
  m_fProximity := (rInfo.nFlags and ZG_CTR_F_PROXIMITY) <> 0;
  m_nOptReadItems := rInfo.nOptReadItems;

  lvEvents.Items.Count := 0;
  m_aEvents.Clear();
end;

procedure TfmCtrEvents.lvEventsCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin
  if m_fAllEvents then
  begin
    with lvEvents.Canvas do
    begin
      if Item.Index = m_nEvWrIdx then
        Brush.Color := EV_WR_IDX_COLOR
      else if Item.Index = m_nEvRdIdx then
        Brush.Color := EV_RD_IDX_COLOR
    end;
  end;
end;

procedure TfmCtrEvents.lvEventsData(Sender: TObject; Item: TListItem);
var
  p: TZEvent;
  s: String;
  pPE: TZPassEvent;
  pEE: TZEcEvent;
  pFE: TZFireEvent;
  pSE: TZSecurEvent;
  pME: TZModeEvent;
  pKE: TZKeyEvent;
  pHE: TZHotelEvent;
  f: Boolean;
begin
  p := m_aEvents[Item.Index] as TZEvent;
  Item.Caption := IntToStr(p.m_nEvIdx);
  while Item.SubItems.Count < 3 do
    Item.SubItems.Add('');
  if p.m_nType <> ZG_EV_UNKNOWN then
    Item.SubItems[1] := EvTypeStrs[p.m_nType]
  else
    Item.SubItems[1] := format(StrUnkEvent_D, [p.m_nEvCode]);
  if p is TZPassEvent then
  begin
    pPE := (p as TZPassEvent);
    if pPE.m_dtDate <> 0 then
      Item.SubItems[0] := DateTimeToStr(pPE.m_dtDate)
    else
      Item.SubItems[0] := '';
    s := DirectStrs[pPE.m_nDirect];
    if pPE.m_nKeyIdx <> -1 then
    begin
      s := s + format(StrPassEventOpt_DD, [pPE.m_nKeyIdx, pPE.m_nKeyBank]);
      if pPE.m_fKeyNumValid then
      begin
        case m_nNumFmt of
          nfEmMarine: f := True;
          nfDallas: f := False;
          else f := m_fProximity;
        end;
        s := s + ', ' + KeyNumToStr(pPE.m_rKeyNum, f);
      end;
    end;
  end
  else if p is TZEcEvent then
  begin
    pEE := (p as TZEcEvent);
    if pEE.m_dtDate <> 0 then
      Item.SubItems[0] := DateTimeToStr(pEE.m_dtDate)
    else
      Item.SubItems[0] := '';
    s := format(StrEcEventOpt_SD, [EcSubEvStrs[pEE.m_nSubEv], pEE.m_nPowerFlags]);
  end
  else if p is TZFireEvent then
  begin
    pFE := (p as TZFireEvent);
    if pFE.m_dtDate <> 0 then
      Item.SubItems[0] := DateTimeToStr(pFE.m_dtDate)
    else
      Item.SubItems[0] := '';
    s := format(StrFireEventOpt_SD, [FireSubEvStrs[pFE.m_nSubEv], pFE.m_nFireFlags]);
  end
  else if p is TZSecurEvent then
  begin
    pSE := (p as TZSecurEvent);
    if pSE.m_dtDate <> 0 then
      Item.SubItems[0] := DateTimeToStr(pSE.m_dtDate)
    else
      Item.SubItems[0] := '';
    s := format(StrSecurEventOpt_SD, [SecurSubEvStrs[pSE.m_nSubEv], pSE.m_nSecurFlags]);
  end
  else if p is TZModeEvent then
  begin
    pME := (p as TZModeEvent);
    if pME.m_dtDate <> 0 then
      Item.SubItems[0] := DateTimeToStr(pME.m_dtDate)
    else
      Item.SubItems[0] := '';
    s := format(StrModeEventOpt_SS, [ModeStrs[pME.m_nMode], ModeSubEvStrs[pME.m_nSubEv]]);
  end
  else if p is TZKeyEvent then
  begin
    pKE := (p as TZKeyEvent);
    case m_nNumFmt of
      nfEmMarine: f := True;
      nfDallas: f := False;
      else f := m_fProximity;
    end;
    s := KeyNumToStr(pKE.m_rKeyNum, f);
  end
  else if p is TZHotelEvent then
  begin
    pHE := (p as TZHotelEvent);
    if pHE.m_dtDate <> 0 then
      Item.SubItems[0] := DateTimeToStr(pHE.m_dtDate)
    else
      Item.SubItems[0] := '';
    s := format(StrHotelEventOpt_SSD, [HModeStrs[pHE.m_nMode],
        HotelSubEvStrs[pHE.m_nSubEv], pHE.m_nFlags]);
  end
  else
    s := '';
  Item.SubItems[2] := s;
end;

procedure TfmCtrEvents.UpdateView();
var
  cx, i, n: Integer;
begin
  lvEvents.Items.Count := m_aEvents.Count;
  lvEvents.Invalidate();
  cx := lvEvents.ClientWidth;
  n := lvEvents.Columns.Count - 1;
  for i := 0 to n - 1 do
    Dec(cx, lvEvents.Column[i].Width);
  lvEvents.Column[n].Width := cx;
end;

procedure TfmCtrEvents.SetNumFmt(AFmt: TNumFormat);
begin
  if m_nNumFmt = AFmt then
    Exit;
  m_nNumFmt := AFmt;
  UpdateView();
end;

procedure TfmCtrEvents.miNumFmtAutoClick(Sender: TObject);
begin
  SetNumFmt(nfAuto);
end;

procedure TfmCtrEvents.miNumFmtDSClick(Sender: TObject);
begin
  SetNumFmt(nfDallas);
end;

procedure TfmCtrEvents.miNumFmtEmClick(Sender: TObject);
begin
  SetNumFmt(nfEmMarine);
end;

procedure TfmCtrEvents.PopupMenu1Popup(Sender: TObject);
begin
  case m_nNumFmt of
    nfEmMarine: miNumFmtEm.Checked := True;
    nfDallas: miNumFmtDS.Checked := True;
    else miNumFmtAuto.Checked := True;
  end;
end;

function EnumNewEventsCB(AIdx: Integer; AEvent: PZG_CTR_EVENT; APos, AMax: Integer; AUserData: Pointer): LongBool; stdcall;
var
  nPct: Integer;
begin
  try
    if AMax <> 0 then
      nPct := (APos * 100) div AMax
    else
      nPct := 0;
    with fmCtrEvents do
      _NewCtrEvent(m_aEvents, FCtr, AIdx, AEvent^);
    fmProcess.SetPct(nPct);
    Result := not fmProcess.m_fCancelled;
  except
    Result := False;
  end;
end;

procedure TfmCtrEvents.DoReadNew();
var
  nWrIdx, nRdIdx, nNewCount: Integer;
begin
  lvEvents.Items.Count := 0;
  Enabled := False;
  try
    fmProcess.Init(Self, StrReadNewEvents_D);
    FCtr.ReadEventIdxs(nWrIdx, nRdIdx);
    if nWrIdx >= nRdIdx then
      nNewCount := nWrIdx - nRdIdx
    else
      nNewCount := m_nMaxEvents - nRdIdx + nWrIdx;
    m_aEvents.Clear();
    if nNewCount > 0 then
    begin
      FCtr.EnumEvents(nRdIdx, nNewCount, EnumNewEventsCB, nil);
      ReadKeyNumsForEvents();
    end;
    m_fAllEvents := False;
  finally
    Enabled := True;
    fmProcess.Hide();
    UpdateView();
  end;
end;

procedure TfmCtrEvents.btnReadNewClick(Sender: TObject);
begin
  DoReadNew();
end;

function EnumAllEventsCB(AIdx: Integer; AEvent: PZG_CTR_EVENT; APos, AMax: Integer; AUserData: Pointer): LongBool; stdcall;
var
  nPct: Integer;
begin
  try
    nPct := (APos * 100) div AMax;
    with fmCtrEvents do
      _NewCtrEvent(m_aEvents, FCtr, AIdx, AEvent^);
    fmProcess.SetPct(nPct);
    Result := not fmProcess.m_fCancelled;
  except
    Result := False;
  end;
end;

procedure TfmCtrEvents.ReadKeyNumsForEvents();
var
  i, j, nKeyIdx, nKeyBank, nPct: Integer;
  p: TZEvent;
  rKey: TZG_Ctr_Key;
  pPE: TZPassEvent;
begin
  fmProcess.Init(Self, StrReadKeyNumForEvs_D);
  FCvt.SetCapture();
  try
    for i := 0 to m_aEvents.Count - 1 do
    begin
      // определение позиции следующего считываемого ключа
      p := m_aEvents[i] as TZEvent;
      if (p is TZPassEvent) and
          (TZPassEvent(p).m_nKeyIdx <> -1) and (not TZPassEvent(p).m_fKeyNumValid) then
      begin
        pPE := (p as TZPassEvent);
        nKeyIdx := pPE.m_nKeyIdx;
        nKeyBank := pPE.m_nKeyBank;
        FCtr.ReadKeys(nKeyIdx, @rKey, 1, nil, nil, nKeyBank);
        if rKey.fErased then
          pPE.m_rKeyNum[0] := 0
        else
          pPE.m_rKeyNum := rKey.rNum;
        pPE.m_fKeyNumValid := True;
        for j := (i + 1) to m_aEvents.Count - 1 do
        begin
          p := m_aEvents[i] as TZEvent;
          if (p is TZPassEvent) and (not TZPassEvent(p).m_fKeyNumValid) and
              (TZPassEvent(p).m_nKeyIdx = nKeyIdx) and (TZPassEvent(p).m_nKeyBank = nKeyBank) then
          begin
            pPE := (p as TZPassEvent);
            if rKey.fErased then
              pPE.m_rKeyNum[0] := 0
            else
              pPE.m_rKeyNum := rKey.rNum;
            pPE.m_fKeyNumValid := True;
          end;
        end;
      end;
      nPct := (i * 100) div m_aEvents.Count;
      fmProcess.SetPct(nPct);
      if fmProcess.m_fCancelled then
        break;
    end;
  finally
    FCvt.ReleaseCapture();
  end;
end;

procedure TfmCtrEvents.DoReadAll();
begin
  lvEvents.Items.Count := 0;
  Enabled := False;
  try
    fmProcess.Init(Self, StrReadAllEvents_D);
    m_aEvents.Clear();
    FCtr.ReadEventIdxs(m_nEvWrIdx, m_nEvRdIdx);
    FCtr.EnumEvents(0, MaxInt, EnumAllEventsCB, nil);
    if not fmProcess.m_fCancelled then
      ReadKeyNumsForEvents();
    m_fAllEvents := True;
  finally
    Enabled := True;
    fmProcess.Hide();
    UpdateView();
  end;
end;

procedure TfmCtrEvents.DoDump();
var
  oDlg: TSaveDialog;
  sFileName: String;
  oData: TFileStream;
  nPos, nMax, nOptRead, nRead: Integer;
  aBuf: TBytes;
begin
  oDlg := TSaveDialog.Create(Self);
  try
    oDlg.Filter := 'Binary files (*.bin)|*.bin|All files|*.*';
    oDlg.FileName := format('ev_dump_%d.bin', [FCtr.Sn]);
    if not oDlg.Execute(Handle) then
      Exit;
    sFileName := oDlg.FileName;
    if (oDlg.FilterIndex = 0) and (ExtractFileExt(sFileName) = '') then
      sFileName := sFileName + '.bin';
  finally
    oDlg.Free();
  end;
  oData := nil;
  FCvt.SetCapture();
  Enabled := False;
  try
    oData := TFileStream.Create(sFileName, fmCreate);
    fmProcess.Init(Self, StrReadMemory_D);
    nOptRead := (m_nOptReadItems * 8);
    SetLength(aBuf, nOptRead);
    nPos := 0;
    nMax := (m_nMaxEvents * 8);
    while nPos < nMax do
    begin
      nRead := min(nMax - nPos, nOptRead);
      FCtr.ReadData(2, nPos, nRead, aBuf[0]);
      oData.WriteBuffer(aBuf[0], nRead);
      Inc(nPos, nRead);
      fmProcess.SetPct((nPos * 100) div nMax);
      if fmProcess.m_fCancelled then
        break;
    end;
  finally
    Enabled := True;
    FCvt.ReleaseCapture();
    fmProcess.Hide();
    oData.Free();
  end;
end;

procedure TfmCtrEvents.btnDumpClick(Sender: TObject);
begin
  DoDump();
end;

procedure TfmCtrEvents.btnReadAllClick(Sender: TObject);
begin
  DoReadAll();
end;

end.
