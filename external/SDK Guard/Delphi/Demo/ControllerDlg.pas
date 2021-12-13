unit ControllerDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ZGuard, ZGClasses, uConst;

type
  TfmController = class(TForm)
    labVersion: TLabel;
    Label3: TLabel;
    labSn: TLabel;
    Label1: TLabel;
    Label4: TLabel;
    labKeyMode: TLabel;
    Label5: TLabel;
    labType: TLabel;
    edtInfoLines: TMemo;
    Label2: TLabel;
    labMaxEvents: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    labMaxKeys: TLabel;
    grCtrConfig: TGroupBox;
    btnLock: TButton;
    btnSchedule: TButton;
    btnKeys: TButton;
    grCtrControl: TGroupBox;
    btnRemoteOpenDoor: TButton;
    GroupBox1: TGroupBox;
    btnEvents: TButton;
    btnMonitor: TButton;
    btnRemoteOpenDoorOut: TButton;
    btnDisconnectLocks: TButton;
    btnCfgDump: TButton;
    btnElectro: TButton;
    chkEmergUnlock: TCheckBox;
    btnModes: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnLockClick(Sender: TObject);
    procedure btnScheduleClick(Sender: TObject);
    procedure btnKeysClick(Sender: TObject);
    procedure btnRemoteOpenDoorClick(Sender: TObject);
    procedure btnRemoteOpenDoorOutClick(Sender: TObject);
    procedure btnEventsClick(Sender: TObject);
    procedure btnMonitorClick(Sender: TObject);
    procedure btnDisconnectLocksClick(Sender: TObject);
    procedure btnCfgDumpClick(Sender: TObject);
    procedure btnElectroClick(Sender: TObject);
    procedure chkEmergUnlockClick(Sender: TObject);
    procedure btnModesClick(Sender: TObject);
  private
    { Private declarations }
    m_nCtrlUpdate   : Integer;
    m_nMaxBanks     : Integer;
    m_nOptReadItems : Integer;
    m_nMaxEvents    : Integer;
    m_nCtrFlags     : Cardinal;

    procedure DoCfgDump();
  public
    { Public declarations }
    FCvt            : TZConverter;
    FCtr            : TZController;
  end;

var
  fmController: TfmController;

implementation

{$R *.dfm}

uses
  Math,
  CtrLockDlg, CtrScheduleDlg, CtrKeysDlg, CtrEventsDlg, CtrEvMonitorDlg,
  CtrElectroDlg, CtrModesDlg, uProcessDlg;


procedure TfmController.DoCfgDump();
var
  oDlg: TSaveDialog;
  sFileName: String;
  oData: TFileStream;
  nPos, nMax, nOptRead, nRead, nBankCnt, nBankN, nAddr, nSize: Integer;
  aBuf: TBytes;
begin
  oDlg := TSaveDialog.Create(Self);
  try
    oDlg.Filter := 'Binary files (*.bin)|*.bin|All files|*.*';
    oDlg.FileName := format('cfg_dump_%d.bin', [FCtr.Sn]);
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
    nBankCnt := m_nMaxBanks;
    if (nBankCnt = 1) and ((m_nCtrFlags and ZG_CTR_F_JOIN) <> 0) then
      nBankCnt := 2;
    nPos := 0;
    nMax := (m_nMaxEvents * 8) * nBankCnt;
    for nBankN := 0 to nBankCnt - 1 do
    begin
      nAddr := 0;
      nSize := (m_nMaxEvents * 8);
      while nAddr < nSize do
      begin
        nRead := min(nSize - nAddr, nOptRead);
        FCtr.ReadData(nBankN, nAddr, nRead, aBuf[0]);
        oData.WriteBuffer(aBuf[0], nRead);
        Inc(nAddr, nRead);
        Inc(nPos, nRead);
        fmProcess.SetPct((nPos * 100) div nMax);
        if fmProcess.m_fCancelled then
          break;
      end;
    end;
  finally
    Enabled := True;
    FCvt.ReleaseCapture();
    fmProcess.Hide();
    oData.Free();
  end;
end;

procedure TfmController.btnCfgDumpClick(Sender: TObject);
begin
  DoCfgDump();
end;

procedure TfmController.btnDisconnectLocksClick(Sender: TObject);
begin
  FCtr.CloseLock();
end;

procedure TfmController.btnElectroClick(Sender: TObject);
begin
  fmCtrElectro.PopupParent := Self;
  fmCtrElectro.FCtr := FCtr;
  fmCtrElectro.ShowModal();
end;

procedure TfmController.btnEventsClick(Sender: TObject);
begin
  fmCtrEvents.FCvt := FCvt;
  fmCtrEvents.FCtr := FCtr;
  fmCtrEvents.ShowModal();
end;

procedure TfmController.btnKeysClick(Sender: TObject);
begin
  fmCtrKeys.PopupParent := Self;
  fmCtrKeys.FCtr := FCtr;
  fmCtrKeys.ShowModal();
end;

procedure TfmController.btnLockClick(Sender: TObject);
begin
  fmCtrLock.FCtr := FCtr;
  fmCtrLock.ShowModal();
end;

procedure TfmController.btnModesClick(Sender: TObject);
begin
  fmCtrModes.FCtr := FCtr;
  fmCtrModes.ShowModal();
end;

procedure TfmController.btnMonitorClick(Sender: TObject);
begin
  fmCtrEvMonitor.FCtr := FCtr;
  fmCtrEvMonitor.ShowModal();
end;

procedure TfmController.btnRemoteOpenDoorClick(Sender: TObject);
begin
  FCtr.OpenLock(0);
end;

procedure TfmController.btnRemoteOpenDoorOutClick(Sender: TObject);
begin
  FCtr.OpenLock(1);
end;

procedure TfmController.btnScheduleClick(Sender: TObject);
begin
  fmCtrSchedule.FCtr := FCtr;
  fmCtrSchedule.ShowModal();
end;

procedure TfmController.chkEmergUnlockClick(Sender: TObject);
begin
  if m_nCtrlUpdate > 0 then exit;
  FCtr.EnableEmergencyUnlocking(chkEmergUnlock.Checked);
end;

procedure TfmController.FormShow(Sender: TObject);
var
  rInfo: TZG_Ctr_Info;
  szLines: array[0..1023] of WideChar;
begin
  FillChar(rInfo, SizeOf(rInfo), 0);
  rInfo.pszLinesBuf := szLines;
  rInfo.nLinesBufMax := Length(szLines);
  FCtr.GetInformation(rInfo);
  if rInfo.nType <> ZG_CTR_UNDEF then
    labType.Caption := CtrTypeStrs[rInfo.nType]
  else
    labType.Caption := format('[%.2X]', [rInfo.nTypeCode]);
  labSn.Caption := IntToStr(rInfo.nSn);
  labVersion.Caption := format('%d.%d', [
      rInfo.nVersion and $ff,
      (rInfo.nVersion shr 8) and $ff]);
  labKeyMode.Caption := KeyModeStrs[(rInfo.nFlags and ZG_CTR_F_PROXIMITY) <> 0];
  labMaxKeys.Caption := IntToStr(rInfo.nMaxKeys);
  labMaxEvents.Caption := IntToStr(rInfo.nMaxEvents);
  if (rInfo.nFlags and ZG_CTR_F_2BANKS) <> 0 then
    m_nMaxBanks := 2
  else
    m_nMaxBanks := 1;
//  btnRemoteOpenDoorOut.Visible := (m_nMaxBanks = 2);
  edtInfoLines.Text := StrPas(szLines);
  m_nOptReadItems := rInfo.nOptReadItems;
  m_nMaxEvents := rInfo.nMaxEvents;
  m_nCtrFlags := rInfo.nFlags;
  btnElectro.Visible := (m_nCtrFlags and ZG_CTR_F_ELECTRO) <> 0;
  btnModes.Visible := (m_nCtrFlags and ZG_CTR_F_MODES) <> 0;
  chkEmergUnlock.Visible := (rInfo.nType in [ZG_CTR_MATRIX2NET..ZG_CTR_Z5RNET8K]) and
      (((rInfo.nVersion and $ff) > 2) or
      (((rInfo.nVersion and $ff) = 2) and (((rInfo.nVersion shr 8) and $ff) >= 5)));
  if chkEmergUnlock.Visible then
  begin
    Inc(m_nCtrlUpdate);
    try
      chkEmergUnlock.Checked := FCtr.IsEmergencyUnlockingEnabled;
    finally
      Dec(m_nCtrlUpdate);
    end;
  end;
end;

end.
