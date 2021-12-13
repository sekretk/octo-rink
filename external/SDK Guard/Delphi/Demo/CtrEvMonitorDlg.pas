unit CtrEvMonitorDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ZGuard, ZBase, ZPort, ZGClasses, uUtils, Menus,
  ExtCtrls;

type
  TfmCtrEvMonitor = class(TForm)
    lvEvents: TListView;
    PopupMenu1: TPopupMenu;
    Numberformat1: TMenuItem;
    miNumFmtAuto: TMenuItem;
    miNumFmtEm: TMenuItem;
    miNumFmtDS: TMenuItem;
    edtLastKeyNum: TEdit;
    Label1: TLabel;
    btnSchedule: TButton;
    btnClear: TButton;
    btnModes: TButton;
    btnLock: TButton;
    btnKeys: TButton;
    btnElectro: TButton;
    btnOpenIn: TButton;
    btnOpenOut: TButton;
    procedure FormShow(Sender: TObject);
    procedure lvEventsData(Sender: TObject; Item: TListItem);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure miNumFmtAutoClick(Sender: TObject);
    procedure miNumFmtDSClick(Sender: TObject);
    procedure miNumFmtEmClick(Sender: TObject);
    procedure btnScheduleClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnModesClick(Sender: TObject);
    procedure btnLockClick(Sender: TObject);
    procedure btnKeysClick(Sender: TObject);
    procedure btnElectroClick(Sender: TObject);
    procedure btnOpenInClick(Sender: TObject);
    procedure btnOpenOutClick(Sender: TObject);
  private
    { Private declarations }
    m_nMaxBanks     : Integer;
    m_nMaxEvents    : Integer;
    m_fProximity    : Boolean;
    m_aEvents       : TZEventList;
    m_nNumFmt       : TNumFormat;

    procedure CtrNewEvent(ASender: TObject; Const AInfo: TZG_N_New_Event_Info);

    procedure ReadEvents(AStart, ACount: Integer);
    procedure ReadKeyNumsForEvents(ACount: Integer);
    procedure SetNumFmt(AFmt: TNumFormat);
    procedure DoClear();
    procedure DoShowSchedule();
    procedure DoShowLock();
    procedure DoShowKeys();
    procedure DoShowModes();
    procedure DoShowElectroControl();
    procedure UpdateView();
  public
    { Public declarations }
    FCtr            : TZController;
  end;

var
  fmCtrEvMonitor: TfmCtrEvMonitor;

implementation

{$R *.dfm}

uses
  DateUtils, Contnrs, uConst, CtrScheduleDlg, CtrModesDlg, CtrLockDlg,
  CtrKeysDlg, CtrElectroDlg;


procedure TfmCtrEvMonitor.UpdateView();
begin
  lvEvents.Items.Count := m_aEvents.Count;
  lvEvents.Invalidate();
end;

procedure TfmCtrEvMonitor.SetNumFmt(AFmt: TNumFormat);
begin
  if m_nNumFmt = AFmt then
    Exit;
  m_nNumFmt := AFmt;
  UpdateView();
end;

procedure TfmCtrEvMonitor.lvEventsData(Sender: TObject; Item: TListItem);
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
  p := m_aEvents[Item.Index];
  while Item.SubItems.Count < 3 do
    Item.SubItems.Add('');

  if p.m_nType <> ZG_EV_UNKNOWN then
    Item.SubItems[0] := EvTypeStrs[p.m_nType]
  else
    Item.SubItems[0] := format(StrUnkEvent_D, [p.m_nEvCode]);
  if p is TZPassEvent then
  begin
    pPE := (p as TZPassEvent);
    if pPE.m_dtDate <> 0 then
      Item.Caption := DateTimeToStr(pPE.m_dtDate);
    Item.SubItems[1] := DirectStrs[pPE.m_nDirect];
    s := '';
    if pPE.m_nKeyIdx <> -1 then
    begin
      s := format(StrPassEventOpt_DD, [pPE.m_nKeyIdx, pPE.m_nKeyBank]);
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
    Item.SubItems[2] := s;
  end
  else if p is TZEcEvent then
  begin
    pEE := (p as TZEcEvent);
    if pEE.m_dtDate <> 0 then
      Item.Caption := DateTimeToStr(pEE.m_dtDate);
    Item.SubItems[2] := format(StrEcEventOpt_SD, [EcSubEvStrs[pEE.m_nSubEv], pEE.m_nPowerFlags]);
  end
  else if p is TZFireEvent then
  begin
    pFE := (p as TZFireEvent);
    if pFE.m_dtDate <> 0 then
      Item.Caption := DateTimeToStr(pFE.m_dtDate);
    Item.SubItems[2] := format(StrFireEventOpt_SD, [FireSubEvStrs[pFE.m_nSubEv], pFE.m_nFireFlags]);
  end
  else if p is TZSecurEvent then
  begin
    pSE := (p as TZSecurEvent);
    if pSE.m_dtDate <> 0 then
      Item.Caption := DateTimeToStr(pSE.m_dtDate);
    Item.SubItems[2] := format(StrSecurEventOpt_SD, [SecurSubEvStrs[pSE.m_nSubEv], pSE.m_nSecurFlags]);
  end
  else if p is TZModeEvent then
  begin
    pME := (p as TZModeEvent);
    if pME.m_dtDate <> 0 then
      Item.Caption := DateTimeToStr(pME.m_dtDate);
    Item.SubItems[2] := format(StrModeEventOpt_SS, [ModeStrs[pME.m_nMode], ModeSubEvStrs[pME.m_nSubEv]]);
  end
  else if p is TZKeyEvent then
  begin
    pKE := (p as TZKeyEvent);
    case m_nNumFmt of
      nfEmMarine: f := True;
      nfDallas: f := False;
      else f := m_fProximity;
    end;
    Item.SubItems[2] := KeyNumToStr(pKE.m_rKeyNum, f);
  end
  else if p is TZHotelEvent then
  begin
    pHE := (p as TZHotelEvent);
    if pHE.m_dtDate <> 0 then
      Item.Caption := DateTimeToStr(pHE.m_dtDate);
    Item.SubItems[2] := format(StrHotelEventOpt_SSD, [HModeStrs[pHE.m_nMode],
        HotelSubEvStrs[pHE.m_nSubEv], pHE.m_nFlags]);
  end;
end;

procedure TfmCtrEvMonitor.miNumFmtAutoClick(Sender: TObject);
begin
  SetNumFmt(nfAuto);
end;

procedure TfmCtrEvMonitor.miNumFmtDSClick(Sender: TObject);
begin
  SetNumFmt(nfDallas);
end;

procedure TfmCtrEvMonitor.miNumFmtEmClick(Sender: TObject);
begin
  SetNumFmt(nfEmMarine);
end;

procedure TfmCtrEvMonitor.PopupMenu1Popup(Sender: TObject);
begin
  case m_nNumFmt of
    nfEmMarine: miNumFmtEm.Checked := True;
    nfDallas: miNumFmtDS.Checked := True;
    else miNumFmtAuto.Checked := True;
  end;
end;

procedure TfmCtrEvMonitor.ReadKeyNumsForEvents(ACount: Integer);
var
  i, j, nKeyIdx, nKeyBank: Integer;
  p: TZEvent;
  rKey: TZG_Ctr_Key;
  pPE: TZPassEvent;
begin
  for i := 0 to ACount - 1 do
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
  end;
end;

procedure TfmCtrEvMonitor.ReadEvents(AStart, ACount: Integer);
var
  i, j, nIdx, nCnt: Integer;
  aEvents: array[0..5] of TZG_CTR_EVENT;
  pEv: PZG_CTR_EVENT;
begin
  try
    i := 0;
    while i < ACount do
    begin
      nIdx := (AStart + i) mod m_nMaxEvents;
      nCnt := (ACount - i);
      if nCnt > Length(aEvents) then
        nCnt := Length(aEvents);
      if (nIdx + nCnt) > m_nMaxEvents then
         nCnt := (m_nMaxEvents - nIdx);
      FCtr.ReadEvents(nIdx, @aEvents, nCnt, nil, nil);

      for j := 0 to nCnt - 1 do
      begin
        pEv := @aEvents[j];
        _NewCtrEvent(m_aEvents, FCtr, i + j, pEv^);
      end;
      Inc(i, nCnt);
    end;
    ReadKeyNumsForEvents(ACount);
  finally
    UpdateView();
  end;
end;

procedure TfmCtrEvMonitor.FormCreate(Sender: TObject);
begin
  m_aEvents := TZEventList.Create();
end;

procedure TfmCtrEvMonitor.FormDestroy(Sender: TObject);
begin
  m_aEvents.Free();
end;

procedure TfmCtrEvMonitor.FormHide(Sender: TObject);
begin
  FCtr.Notifications := [];
end;

procedure TfmCtrEvMonitor.DoClear();
begin
  lvEvents.Items.Count := 0;
  m_aEvents.Clear();
end;

procedure TfmCtrEvMonitor.btnClearClick(Sender: TObject);
begin
  DoClear();
end;

procedure TfmCtrEvMonitor.DoShowSchedule();
begin
  fmCtrSchedule.FCtr := FCtr;
  fmCtrSchedule.ShowModal();
end;

procedure TfmCtrEvMonitor.DoShowModes();
begin
  fmCtrModes.FCtr := FCtr;
  fmCtrModes.ShowModal();
end;

procedure TfmCtrEvMonitor.DoShowLock();
begin
  fmCtrLock.FCtr := FCtr;
  fmCtrLock.ShowModal();
end;

procedure TfmCtrEvMonitor.DoShowKeys();
begin
  fmCtrKeys.PopupParent := Self;
  fmCtrKeys.FCtr := FCtr;
  fmCtrKeys.ShowModal();
end;

procedure TfmCtrEvMonitor.DoShowElectroControl();
begin
  fmCtrElectro.PopupParent := Self;
  fmCtrElectro.FCtr := FCtr;
  fmCtrElectro.ShowModal();
end;

procedure TfmCtrEvMonitor.btnElectroClick(Sender: TObject);
begin
  DoShowElectroControl();
end;

procedure TfmCtrEvMonitor.btnKeysClick(Sender: TObject);
begin
  DoShowKeys();
end;

procedure TfmCtrEvMonitor.btnLockClick(Sender: TObject);
begin
  DoShowLock();
end;

procedure TfmCtrEvMonitor.btnModesClick(Sender: TObject);
begin
  DoShowModes();
end;

procedure TfmCtrEvMonitor.btnOpenInClick(Sender: TObject);
begin
  FCtr.OpenLock(0);
end;

procedure TfmCtrEvMonitor.btnOpenOutClick(Sender: TObject);
begin
  FCtr.OpenLock(1);
end;

procedure TfmCtrEvMonitor.btnScheduleClick(Sender: TObject);
begin
  DoShowSchedule();
end;

procedure TfmCtrEvMonitor.CtrNewEvent(ASender: TObject; Const AInfo: TZG_N_New_Event_Info);
begin
  edtLastKeyNum.Text := KeyNumToStr(AInfo.rLastNum, m_fProximity);
  ReadEvents(AInfo.nReadIdx, AInfo.nNewCount);
end;

procedure TfmCtrEvMonitor.FormShow(Sender: TObject);
var
  rInfo: TZG_Ctr_Info;
  nWrIdx, nRdIdx: Integer;
  rNum: TZ_KeyNum;
begin
  // Получаем параметры контроллера: флаг "Proximity"
  FillChar(rInfo, SizeOf(rInfo), 0);
  FCtr.GetInformation(rInfo);
  if (rInfo.nFlags and ZG_CTR_F_2BANKS) <> 0 then
    m_nMaxBanks := 2
  else
    m_nMaxBanks := 1;
  m_nMaxEvents := rInfo.nMaxEvents;
  m_fProximity := (rInfo.nFlags and ZG_CTR_F_PROXIMITY) <> 0;

  // Получаем указатели событий и последний считанный номер ключа
  FCtr.ReadRTCState(nil, @nWrIdx, @nRdIdx, @rNum);
  edtLastKeyNum.Text := KeyNumToStr(rNum, m_fProximity);
  // Настраиваем уведомления о новых событиях
  FCtr.ReadEvIdx := nWrIdx;
  FCtr.OnNewEvent := CtrNewEvent;
  FCtr.Notifications := [cnNewEvent];

  btnModes.Visible := (rInfo.nFlags and ZG_CTR_F_MODES) <> 0;
  btnElectro.Visible := (rInfo.nFlags and ZG_CTR_F_ELECTRO) <> 0;

  lvEvents.Items.Count := 0;
  m_aEvents.Clear();
end;

end.
