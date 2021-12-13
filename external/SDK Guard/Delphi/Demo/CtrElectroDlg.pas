unit CtrElectroDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Spin, ZGuard, ZGClasses;

type
  TfmCtrElectro = class(TForm)
    grElConfig: TGroupBox;
    labElSchedVal: TLabel;
    grElCard: TGroupBox;
    chkElCardNoBlock: TCheckBox;
    rgElCardReader: TRadioGroup;
    chkElEnable: TCheckBox;
    chkElSchedule: TCheckBox;
    chkElInvInput: TCheckBox;
    chkElPwrOffForExit: TCheckBox;
    grElPwrDelay: TGroupBox;
    labElPwrDelay: TLabel;
    edtElPwrDelay: TSpinEdit;
    grElState: TGroupBox;
    panElState: TPanel;
    lbElStatusEx: TListBox;
    btnElRefresh: TButton;
    btnRead: TButton;
    btnWrite: TButton;
    btnPowerOn: TButton;
    btnPowerOff: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnElRefreshClick(Sender: TObject);
    procedure labElSchedValDblClick(Sender: TObject);
    procedure btnReadClick(Sender: TObject);
    procedure btnWriteClick(Sender: TObject);
    procedure chkElEnableClick(Sender: TObject);
    procedure chkElScheduleClick(Sender: TObject);
    procedure btnPowerOnClick(Sender: TObject);
    procedure btnPowerOffClick(Sender: TObject);
  private
    { Private declarations }
    m_nCtrlUpdate   : Integer;
    m_rConfig       : TZg_Ctr_Electro_Config;
    m_dtElStTime    : TDateTime;
    m_nElState      : Cardinal;
    m_rTz           : TZg_Ctr_TimeZone;

    function UpdateControlData(ASave: Boolean): Boolean;
    procedure UpdateElectroState();
    procedure SetLBElState(AFlagIdx: Integer; AActive: Boolean;
        AOpts: Cardinal=0);
    procedure UpdateScheduleLabel();
    procedure UpdateCfgControlState();
    procedure DoChangeSchedule();
    procedure DoReadConfig();
    procedure DoWriteConfig();
  public
    { Public declarations }
    FCtr            : TZController;
  end;

var
  fmCtrElectro: TfmCtrElectro;

implementation

{$R *.dfm}

uses
  uConst, uUtils, CtrTimeZoneDlg;


procedure TfmCtrElectro.UpdateCfgControlState();
var
  f: Boolean;
begin
  f := (m_rConfig.nPowerConfig and ZG_EC_CF_ENABLED) <> 0;
  grElCard.Enabled := f;
  rgElCardReader.Enabled := f;
  chkElCardNoBlock.Enabled := f;
  grElPwrDelay.Enabled := f;
  edtElPwrDelay.Enabled := f;
  labElPwrDelay.Enabled := f;
  chkElPwrOffForExit.Enabled := f;
  chkElInvInput.Enabled := f;
  chkElSchedule.Enabled := f;
  f := f and ((m_rConfig.nPowerConfig and ZG_EC_CF_SCHEDULE) <> 0);
  labElSchedVal.Enabled := f;
end;

procedure TfmCtrElectro.UpdateScheduleLabel();
var
  s: String;
  i, n: Integer;
begin
  s := StrSchedDows;
  ASSERT(Length(s) = 7);
  n := 1;
  for i := 0 to 6 do
  begin
    if not GetBit(m_rTz.nDayOfWeeks, i) then
      s[n] := '-';
    Inc(n);
  end;
  labElSchedVal.Caption := format('%s  %.2d:%.2d - %.2d:%.2d', [
        s,
        m_rTz.nBegHour, m_rTz.nBegMinute,
        m_rTz.nEndHour, m_rTz.nEndMinute]);
end;

procedure TfmCtrElectro.SetLBElState(AFlagIdx: Integer; AActive: Boolean;
    AOpts: Cardinal);
Const
  ElBools: array[Boolean] of Char = (' ', 'v');
var
  s: String;
begin
  if AFlagIdx = 2 then
  begin
    s := '[ ] ';
    if (AOpts and ZG_EC_SF_CARD) <> 0 then
      s := s + StrElStKeyProcDelay
    else
      s := s + StrElStKey;
  end
  else
    s := lbElStatusEx.Items[AFlagIdx];
  s[2] := ElBools[AActive];
  lbElStatusEx.Items[AFlagIdx] := s;
end;

procedure TfmCtrElectro.UpdateElectroState();
var
  nH, nM, nS, nMs: Word;
begin
  if m_dtElStTime <> 0 then
  begin
    DecodeTime(m_dtElStTime, nH, nM, nS, nMs);
    if (m_nElState and ZG_EC_SF_ENABLED) <> 0 then
      panElState.Caption := format(StrElPowerOn_DDD, [nH, nM, nS])
    else
      panElState.Caption := format(StrElPowerOff_DDD, [nH, nM, nS]);
    SetLBElState(0, (m_nElState and ZG_EC_SF_SCHEDULE) <> 0);
    SetLBElState(1, (m_nElState and ZG_EC_SF_REMOTE) <> 0);
    SetLBElState(2, (m_nElState and ZG_EC_SF_DELAY) <> 0, m_nElState);
  end
  else
  begin
    panElState.Caption := '';
    SetLBElState(0, False);
    SetLBElState(1, False);
    SetLBElState(2, False);
  end;
end;

function TfmCtrElectro.UpdateControlData(ASave: Boolean): Boolean;
var
  n: Cardinal;
begin
  if ASave then
  begin
    n := 0;
    if chkElEnable.Checked then
      Inc(n, ZG_EC_CF_ENABLED);
    if rgElCardReader.ItemIndex = 1 then
      Inc(n, ZG_EC_CF_EXT_READER);
    if chkElCardNoBlock.Checked then
      Inc(n, ZG_EC_CF_CARD_OPEN);
    if chkElPwrOffForExit.Checked then
      Inc(n, ZG_EC_CF_EXIT_OFF);
    if chkElInvInput.Checked then
      Inc(n, ZG_EC_CF_INVERT);
    if chkElSchedule.Checked then
      Inc(n, ZG_EC_CF_SCHEDULE);
    if n <> m_rConfig.nPowerConfig then
      m_rConfig.nPowerConfig := n;
    n := Cardinal(edtElPwrDelay.Value);
    if n <> m_rConfig.nPowerDelay then
      m_rConfig.nPowerDelay := n;
  end
  else
  begin
    Inc(m_nCtrlUpdate);
    try
      n := m_rConfig.nPowerConfig;
      chkElEnable.Checked := (n and ZG_EC_CF_ENABLED) <> 0;
      if (n and ZG_EC_CF_EXT_READER) <> 0 then
        rgElCardReader.ItemIndex := 1
      else
        rgElCardReader.ItemIndex := 0;
      chkElCardNoBlock.Checked := (n and ZG_EC_CF_CARD_OPEN) <> 0;
      edtElPwrDelay.Value := Integer(m_rConfig.nPowerDelay);
      chkElPwrOffForExit.Checked := (n and ZG_EC_CF_EXIT_OFF) <> 0;
      chkElInvInput.Checked := (n and ZG_EC_CF_INVERT) <> 0;
      chkElSchedule.Checked := (n and ZG_EC_CF_SCHEDULE) <> 0;
    finally
      Dec(m_nCtrlUpdate);
    end;
    UpdateElectroState();
    UpdateScheduleLabel();
    UpdateCfgControlState();
  end;
  Result := True;
end;

procedure TfmCtrElectro.btnElRefreshClick(Sender: TObject);
var
  rState: TZg_Ctr_Electro_State;
begin
  FCtr.GetElectroState(rState);
  m_dtElStTime := Now();
  m_nElState := rState.nPowerFlags;
  UpdateElectroState();
end;

procedure TfmCtrElectro.FormShow(Sender: TObject);
begin
  FCtr.ReadElectroConfig(m_rConfig);
  FCtr.ReadTimeZones(ZG_EC_SCHEDULE_TZ, @m_rTz, 1, nil, nil);
  UpdateControlData(False);
end;

procedure TfmCtrElectro.DoReadConfig();
begin
  FCtr.ReadElectroConfig(m_rConfig);
  UpdateControlData(False);
end;

procedure TfmCtrElectro.btnPowerOffClick(Sender: TObject);
begin
  FCtr.SetElectroPower(False);
end;

procedure TfmCtrElectro.btnPowerOnClick(Sender: TObject);
begin
  FCtr.SetElectroPower(True);
end;

procedure TfmCtrElectro.btnReadClick(Sender: TObject);
begin
  DoReadConfig();
end;

procedure TfmCtrElectro.DoWriteConfig();
begin
  UpdateControlData(True);
  FCtr.WriteElectroConfig(m_rConfig);
end;

procedure TfmCtrElectro.btnWriteClick(Sender: TObject);
begin
  DoWriteConfig();
end;

procedure TfmCtrElectro.chkElEnableClick(Sender: TObject);
begin
  if m_nCtrlUpdate > 0 then
    Exit;
  if chkElEnable.Checked then
    m_rConfig.nPowerConfig := m_rConfig.nPowerConfig or ZG_EC_CF_ENABLED
  else
    m_rConfig.nPowerConfig := m_rConfig.nPowerConfig and not ZG_EC_CF_ENABLED;
  UpdateCfgControlState();
end;

procedure TfmCtrElectro.chkElScheduleClick(Sender: TObject);
begin
  if m_nCtrlUpdate > 0 then
    Exit;
  if chkElSchedule.Checked then
    m_rConfig.nPowerConfig := m_rConfig.nPowerConfig or ZG_EC_CF_SCHEDULE
  else
    m_rConfig.nPowerConfig := m_rConfig.nPowerConfig and not ZG_EC_CF_SCHEDULE;
  UpdateCfgControlState();
end;

procedure TfmCtrElectro.DoChangeSchedule();
begin
  with fmCtrTimeZone do
  begin
    m_rTz := Self.m_rTz;
    m_nTimeZoneIdx := ZG_EC_SCHEDULE_TZ;
  end;
  if fmCtrTimeZone.ShowModal() <> mrOk then
    Exit;
  m_rTz := fmCtrTimeZone.m_rTz;
  FCtr.WriteTimeZones(ZG_EC_SCHEDULE_TZ, @m_rTz, 1, nil, nil);
  UpdateScheduleLabel();
end;

procedure TfmCtrElectro.labElSchedValDblClick(Sender: TObject);
begin
  DoChangeSchedule();
end;

end.
