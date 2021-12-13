unit CtrModesDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ZGuard, ZGClasses, StdCtrls;

type
  TModeTimeZones = array[0..1] of TZG_Ctr_TimeZone;

  TfmCtrModes = class(TForm)
    labCurrMode: TLabel;
    cbCurrMode: TComboBox;
    grTimeZones: TGroupBox;
    btnTzDefault: TButton;
    lbTzs: TListBox;
    btnChangeTZ: TButton;
    btnRefresh: TButton;
    lbStateFlags: TListBox;
    procedure FormShow(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure cbCurrModeChange(Sender: TObject);
    procedure btnChangeTZClick(Sender: TObject);
    procedure btnTzDefaultClick(Sender: TObject);
  private
    { Private declarations }
    m_nCtrlUpdate   : Integer;
    m_aTimeZones    : TModeTimeZones;

    procedure DoRefresh();
    procedure UpdateStateFlagLbItem(AItem: Integer; AFlag: Boolean);
    procedure DoChangeTz();
    procedure DoDefaultTz();
    procedure UpdateTzLb();
    procedure UpdateTzLbItem(AIdx: Integer;
        Const ATz: TZG_Ctr_TimeZone; ARedraw: Boolean=True);
  public
    { Public declarations }
    FCtr            : TZController;
  end;

var
  fmCtrModes: TfmCtrModes;

implementation

{$R *.dfm}

uses
  uUtils, uConst, CtrTimeZoneDlg;


procedure TfmCtrModes.cbCurrModeChange(Sender: TObject);
var
  nIdx: Integer;
begin
  if m_nCtrlUpdate > 0 then
    Exit;
  nIdx := cbCurrMode.ItemIndex;
  if nIdx = -1 then
    Exit;
  FCtr.SetCtrMode(TZg_Ctr_Mode(Integer(ZG_MODE_NORMAL) + nIdx));
end;

procedure TfmCtrModes.UpdateStateFlagLbItem(AItem: Integer; AFlag: Boolean);
Const
  _Bools: array[Boolean] of Char = (' ', 'v');
var
  s: String;
begin
  s := lbStateFlags.Items[AItem];
  s[2] := _Bools[AFlag];
  lbStateFlags.Items[AItem] := s;
end;

procedure TfmCtrModes.DoRefresh();
var
  nMode: TZg_Ctr_Mode;
  nFlags: Cardinal;
  i: Integer;
begin
  FCtr.GetCtrModeInfo(nMode, nFlags);
  Inc(m_nCtrlUpdate);
  try
    cbCurrMode.ItemIndex := Integer(nMode) - Integer(ZG_MODE_NORMAL);
    for i := 0 to 2 do
      UpdateStateFlagLbItem(i, GetBit(nFlags, i));
  finally
    Dec(m_nCtrlUpdate);
  end;
end;

procedure TfmCtrModes.DoChangeTz();
var
  nIdx: Integer;
begin
  nIdx := lbTzs.ItemIndex;
  if nIdx = -1 then
    exit;
  with fmCtrTimeZone do
  begin
    m_rTz := m_aTimeZones[nIdx];
    if m_rTz.nMode = ZG_MODE_UNDEF then
      m_rTz.nMode := ZG_MODE_NORMAL;
    m_nTimeZoneIdx := (ZG_MODES_TZ0 + nIdx);
  end;
  if fmCtrTimeZone.ShowModal() <> mrOk then
    exit;
  FCtr.WriteTimeZones(fmCtrTimeZone.m_nTimeZoneIdx, @fmCtrTimeZone.m_rTz, 1, nil, nil);
  m_aTimeZones[nIdx] := fmCtrTimeZone.m_rTz;
  UpdateTzLbItem(nIdx, m_aTimeZones[nIdx]);
end;

procedure TfmCtrModes.btnChangeTZClick(Sender: TObject);
begin
  DoChangeTz();
end;

procedure TfmCtrModes.btnRefreshClick(Sender: TObject);
begin
  DoRefresh();
end;

procedure TfmCtrModes.DoDefaultTz();
var
  i: Integer;
  aNewTZs: TModeTimeZones;
begin
  for i := 0 to Length(aNewTZs) - 1 do
  begin
    with aNewTZs[i] do
    begin
      nDayOfWeeks := $7F;
      nBegHour := 0;
      nBegMinute := 0;
      nEndHour := 23;
      nEndMinute := 59;
    end;
  end;
  FCtr.WriteTimeZones(ZG_MODES_TZ0, @aNewTZs[0], 2, nil, nil);
  m_aTimeZones := aNewTZs;
  UpdateTzLb();
end;

procedure TfmCtrModes.btnTzDefaultClick(Sender: TObject);
begin
  if MessageDlg(StrReally, mtConfirmation, [mbYes, mbNo, mbCancel], 0) <> mrYes then
    exit;
  DoDefaultTz();
end;

procedure TfmCtrModes.UpdateTzLbItem(AIdx: Integer;
    Const ATz: TZG_Ctr_TimeZone; ARedraw: Boolean);
var
  s: String;
  i, n: Integer;
begin
  s := 'M T W T F S S';
  n := 1;
  for i := 0 to 6 do
  begin
    if not GetBit(ATz.nDayOfWeeks, i) then
      s[n] := '-';
    Inc(n, 2);
  end;
  with lbTzs do
  begin
    Items[AIdx] := format('%d  %s  %.2d:%.2d-%.2d:%.2d - %s', [
        AIdx + 1,
        s,
        ATz.nBegHour, ATz.nBegMinute,
        ATz.nEndHour, ATz.nEndMinute,
        ModeStrs[ATz.nMode]]);

    if ARedraw then
      Invalidate();
  end;
end;

procedure TfmCtrModes.UpdateTzLb();
var
  i: Integer;
begin
  with lbTzs do
  begin
    Items.BeginUpdate();
    try
      for i := 0 to Length(m_aTimeZones) - 1 do
        UpdateTzLbItem(i, m_aTimeZones[i], False);
    finally
      Items.EndUpdate();
    end;
    Invalidate();
  end;
end;

procedure TfmCtrModes.FormShow(Sender: TObject);
begin
  DoRefresh();
  FCtr.ReadTimeZones(ZG_MODES_TZ0, @m_aTimeZones[0], 2, nil, nil);
  UpdateTzLb();
end;

end.
