unit ConverterDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ZGuard, ZPort, ZGClasses, uConst, Menus;

type
  TZControllerInfo = record
    nType           : TZG_Ctr_Type;
    nTypeCode       : Byte;
    nAddr           : Byte;
    nSn             : Word;
    nVersion        : Word;
    nMaxKeys        : Integer;
    nMaxEvents      : Integer;
    nFlags          : Cardinal;
  end;
  PZControllerInfo = ^TZControllerInfo;

  TAZControllerInfs = array of TZControllerInfo;

  TfmConverter = class(TForm)
    edtInfoLines: TMemo;
    labCvtVer: TLabel;
    Label3: TLabel;
    labCvtSn: TLabel;
    Label1: TLabel;
    Label4: TLabel;
    labCvtMode: TLabel;
    grCtrs: TGroupBox;
    lvCtrs: TListView;
    btnCtrOpen: TButton;
    Label5: TLabel;
    labCvtType: TLabel;
    btnUpdateFW: TButton;
    PopupMenu1: TPopupMenu;
    miRefresh: TMenuItem;
    grLicense: TGroupBox;
    Label6: TLabel;
    labLicNum: TLabel;
    btnClearAllLic: TButton;
    btnSetLicense: TButton;
    Label7: TLabel;
    labLicCtrs: TLabel;
    Label8: TLabel;
    labLicKeys: TLabel;
    Label9: TLabel;
    labLicDate: TLabel;
    Label10: TLabel;
    labLicCounter: TLabel;
    Devicetypes1: TMenuItem;
    miDevTypesGATE: TMenuItem;
    miDevTypesEurolock: TMenuItem;
    miDevTypesAll: TMenuItem;
    labStatus: TLabel;
    labStatusValue: TLabel;
    procedure FormShow(Sender: TObject);
    procedure btnCtrRescanClick(Sender: TObject);
    procedure btnCtrOpenClick(Sender: TObject);
    procedure lvCtrsData(Sender: TObject; Item: TListItem);
    procedure btnUpdateFWClick(Sender: TObject);
    procedure miRefreshClick(Sender: TObject);
    procedure lvCtrsEdited(Sender: TObject; Item: TListItem; var S: string);
    procedure btnSetLicenseClick(Sender: TObject);
    procedure lvCtrsDblClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure btnClearAllLicClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure miDevTypesGATEClick(Sender: TObject);
    procedure miDevTypesEurolockClick(Sender: TObject);
    procedure miDevTypesAllClick(Sender: TObject);
  private
    { Private declarations }
    m_fGuard        : Boolean;
    m_nCvtSn        : Word;
    m_aCtrs         : TAZControllerInfs;

    procedure CvtCtrInsert(Sender: TObject; Const AInfo: TZG_Find_Ctr_Info);
    procedure CvtCtrRemove(Sender: TObject; Const AInfo: TZG_Find_Ctr_Info);
    procedure CvtConnectionChange(Sender: TObject);

    procedure UpdateConnectStatus();
    procedure UpdateLicLimInfo();
    procedure UpdateControllers(AForce: Boolean=True);
    procedure DoOpenSelCtr();
    procedure DoSetLicense();
    procedure DoClearAllLicenses();
    procedure RemoveCtr(AIdx: Integer);
    function FindCtrIdx(ASn, AAddr: Integer): Integer;
    procedure SetDevTypes(AGate, AEurolock: Boolean);
  public
    { Public declarations }
    FCvt            : TZConverter;
  end;

var
  fmConverter: TfmConverter;

implementation

{$R *.dfm}

uses
  IniFiles, Math,
  ControllerDlg, uProcessDlg;


procedure TfmConverter.DoClearAllLicenses();
begin
  if MessageDlg(StrConfirm, mtConfirmation, [mbYes, mbNo, mbCancel], 0) <> mrYes then
    exit;
  FCvt.ClearAllLicenses();
  UpdateLicLimInfo();
end;

procedure TfmConverter.btnClearAllLicClick(Sender: TObject);
begin
  DoClearAllLicenses();
end;

procedure TfmConverter.btnCtrOpenClick(Sender: TObject);
begin
  DoOpenSelCtr();
end;

procedure TfmConverter.btnCtrRescanClick(Sender: TObject);
begin
  UpdateControllers();
end;

procedure TfmConverter.UpdateLicLimInfo();
var
  rLic: TZG_Cvt_Lic_Info;
begin
  FillChar(rLic, SizeOf(rLic), 0);
  FCvt.GetLicense(ZG_DEF_CVT_LICN, rLic);
  if rLic.nMaxCtrs <> $FF then
    labLicCtrs.Caption := IntToStr(rLic.nMaxCtrs)
  else
    labLicCtrs.Caption := StrUnlimited;
  if rLic.nMaxKeys <> $FFFF then
    labLicKeys.Caption := IntToStr(rLic.nMaxKeys)
  else
    labLicKeys.Caption := StrUnlimited;
  if rLic.nMaxYear <> $FFFF then
    labLicDate.Caption := format('%.2d.%.2d.%.4d', [rLic.nMaxDay, rLic.nMaxMon, rLic.nMaxYear])
  else
    labLicDate.Caption := StrUnlimited;
  if rLic.nDownCountTime <> $FFFF then
    labLicCounter.Caption := IntToStr(rLic.nDownCountTime)
  else
    labLicCounter.Caption := StrUnlimited;
end;

procedure TfmConverter.DoSetLicense();
var
  oDlg: TOpenDialog;
  sFileName, sLic: String;
  oLicIni: TCustomIniFile;
  nLicSn: Integer;
  sLicData: AnsiString;
  nLicStatus: Word;
begin
  oDlg := TOpenDialog.Create(Self);
  try
    oDlg.Filter := StrLicFilter;
    if not oDlg.Execute(Handle) then
      exit;
    sFileName := oDlg.FileName;
  finally
    oDlg.Free();
  end;
  oLicIni := TMemIniFile.Create(sFileName);
  try
    nLicSn := oLicIni.ReadInteger('Param', 'Num', 0);
    if (nLicSn <> 0) and (nLicSn <> m_nCvtSn) then
    begin
      MessageDlg(StrEInvalidSn, mtError, [mbOk], 0);
      exit;
    end;
    sLic := oLicIni.ReadString('Lic', 'Txt', '');
    if sLic <> '' then
    begin
      SetLength(sLicData, Length(sLic) div 2);
      SetLength(sLicData, HexToBin(PChar(sLic), @sLicData[1], Length(sLicData)));
    end;
  finally
    oLicIni.Free();
  end;
  if sLicData = '' then
  begin
    MessageDlg(StrEInvalidLicData, mtError, [mbOk], 0);
    exit;
  end;
  FCvt.SetLicenseData(ZG_DEF_CVT_LICN, sLicData[1], Length(sLicData), @nLicStatus);
  UpdateLicLimInfo();
end;

procedure TfmConverter.btnSetLicenseClick(Sender: TObject);
begin
  DoSetLicense();
end;

function UpdateFW_CB(APos: Integer; AMax: Integer; AUserData: Pointer): LongBool; stdcall;
var
  nPct: Integer;
begin
  if AMax <> 0 then
    nPct := (APos * 100) div AMax
  else
    nPct := 0;
  fmProcess.SetPct(nPct);
  Result := not fmProcess.m_fCancelled;
end;

procedure TfmConverter.btnUpdateFWClick(Sender: TObject);
var
  nIdx: Integer;
  p: PZControllerInfo;
  oDlg: TOpenDialog;
  sFileName: String;
  oData: TMemoryStream;
  s: AnsiString;
begin
  nIdx := lvCtrs.ItemIndex;
  if nIdx = -1 then
    exit;
  p := @m_aCtrs[nIdx];
  oDlg := TOpenDialog.Create(Self);
  try
    oDlg.Filter := StrCvtFWFilter;
    if not oDlg.Execute(Handle) then
      exit;
    sFileName := oDlg.FileName;
  finally
    oDlg.Free();
  end;
  oData := TMemoryStream.Create();
  Enabled := False;
  try
    oData.LoadFromFile(sFileName);
    fmProcess.Init(Self, StrUpdateCtrFW_D);
    s := AnsiString(ExtractFileName(sFileName));
    FCvt.UpdateCtrFirmware(p.nSn,
        oData.Memory^, oData.Size, PAnsiChar(s), UpdateFW_CB, nil);
  finally
    Enabled := True;
    fmProcess.Hide();
    oData.Free();
  end;
end;

procedure TfmConverter.RemoveCtr(AIdx: Integer);
var
  i: Integer;
begin
  for i := AIdx + 1 to High(m_aCtrs) do
    m_aCtrs[i - 1] := m_aCtrs[i];
  SetLength(m_aCtrs, Length(m_aCtrs) - 1);
  lvCtrs.Items.Count := Length(m_aCtrs);
  lvCtrs.Invalidate();
end;

function TfmConverter.FindCtrIdx(ASn, AAddr: Integer): Integer;
begin
  if ASn <> 0 then
  begin
    for Result := 0 to High(m_aCtrs) do
    begin
      if m_aCtrs[Result].nSn = ASn then
        Exit;
    end;
  end
  else
  begin
    for Result := 0 to High(m_aCtrs) do
    begin
      if m_aCtrs[Result].nAddr = AAddr then
        Exit;
    end;
  end;
  Result := -1;
end;

procedure TfmConverter.SetDevTypes(AGate, AEurolock: Boolean);
begin
  if AGate then
    FCvt.ScanOptions := FCvt.ScanOptions - [csoNoGate]
  else
    FCvt.ScanOptions := FCvt.ScanOptions + [csoNoGate];
  if AEurolock then
    FCvt.ScanOptions := FCvt.ScanOptions - [csoNoEurolock]
  else
    FCvt.ScanOptions := FCvt.ScanOptions + [csoNoEurolock];
  UpdateControllers();
end;

procedure TfmConverter.FormHide(Sender: TObject);
begin
  FCvt.Active := False;
//  FCvt.Notifications := False;
end;

procedure TfmConverter.CvtCtrInsert(Sender: TObject; Const AInfo: TZG_Find_Ctr_Info);
var
  n: Integer;
begin
  n := FindCtrIdx(AInfo.nSn, AInfo.nAddr);
  if n = -1 then
  begin
    n := Length(m_aCtrs);
    SetLength(m_aCtrs, n + 1);
  end;
  with m_aCtrs[n] do
  begin
    nType := AInfo.nType;
    nTypeCode := AInfo.nTypeCode;
    nAddr := AInfo.nAddr;
    nSn := AInfo.nSn;
    nVersion := AInfo.nVersion;
    nMaxKeys := AInfo.nMaxKeys;
    nMaxEvents := AInfo.nMaxEvents;
    nFlags := AInfo.nFlags;
  end;
  lvCtrs.Items.Count := Length(m_aCtrs);
  lvCtrs.Invalidate();
end;

procedure TfmConverter.CvtCtrRemove(Sender: TObject; Const AInfo: TZG_Find_Ctr_Info);
var
  n: Integer;
begin
  n := FindCtrIdx(AInfo.nSn, AInfo.nAddr);
  if n <> -1 then
    RemoveCtr(n);
end;

procedure TfmConverter.UpdateConnectStatus();
begin
  labStatusValue.Caption := ConnStStrs[FCvt.GetConnectionStatus];
end;

procedure TfmConverter.CvtConnectionChange(Sender: TObject);
begin
  UpdateConnectStatus();
end;

procedure TfmConverter.FormShow(Sender: TObject);
var
  rInfo: TZG_Cvt_Info;
  szLines: array[0..255] of WideChar;
begin
  FillChar(rInfo, SizeOf(rInfo), 0);
  rInfo.pszLinesBuf := szLines;
  rInfo.nLinesBufMax := Length(szLines);
  FCvt.GetInformation(rInfo);
  labCvtType.Caption := CvtTypeStrs[rInfo.nType];
  m_fGuard := (rInfo.nType >= ZG_CVT_Z397_GUARD) and (rInfo.nType <= ZG_CVT_Z5R_WEB);
  m_nCvtSn := rInfo.rBase.nSn;
  if m_fGuard then
  begin
    labCvtSn.Caption := IntToStr(rInfo.rBase.nSn);
    labCvtVer.Caption := format('%d.%d.%d.%d', [
        (rInfo.rBase.nVersion and $ff),
        (rInfo.rBase.nVersion shr 8) and $ff,
        (rInfo.rBase.nVersion shr 16) and $ff,
        (rInfo.rBase.nVersion shr 24) and $ff]);
    labCvtMode.Caption := CvtModeStrs[rInfo.nMode];
  end
  else
  begin
    labCvtSn.Caption := '-';
    labCvtVer.Caption := '-';
    labCvtMode.Caption := '-';
  end;
  if m_fGuard and (rInfo.nMode = ZG_GUARD_ADVANCED) then
  begin
    grLicense.Visible := True;
    labLicNum.Caption := IntToStr(ZG_DEF_CVT_LICN);
    UpdateLicLimInfo();
  end
  else
    grLicense.Visible := False;
  edtInfoLines.Text := StrPas(szLines);

  FCvt.ScanCtrsPeriod := 500;
  FCvt.ScanCtrsLastAddr := 31;
  FCvt.OnCtrInsert := CvtCtrInsert;
  FCvt.OnCtrRemove := CvtCtrRemove;
  FCvt.OnConnectionChange := CvtConnectionChange;
  FCvt.Notifications := True;
  UpdateConnectStatus();
  UpdateControllers(False);
end;

procedure TfmConverter.lvCtrsData(Sender: TObject; Item: TListItem);
var
  p: PZControllerInfo;
begin
  p := @m_aCtrs[Item.Index];
  Item.Caption := IntToStr(p.nAddr);
  while Item.SubItems.Count < 7 do
    Item.SubItems.Add('');
  if p.nType <> ZG_CTR_UNDEF then
    Item.SubItems[0] := CtrTypeStrs[p.nType]
  else
    Item.SubItems[0] := format('[%.2X]', [p.nTypeCode]);
  Item.SubItems[1] := IntToStr(p.nSn);
  Item.SubItems[2] := format('%d.%d', [LoByte(p.nVersion), HiByte(p.nVersion)]);
  if (p.nFlags and ZG_CTR_F_2BANKS) <> 0 then
    Item.SubItems[3] := '2'
  else
    Item.SubItems[3] := '1';
  Item.SubItems[4] := IntToStr(p.nMaxKeys);
  Item.SubItems[5] := IntToStr(p.nMaxEvents);
  Item.SubItems[6] := KeyModeStrs[(p.nFlags and ZG_CTR_F_PROXIMITY) <> 0];
end;

procedure TfmConverter.lvCtrsDblClick(Sender: TObject);
begin
  DoOpenSelCtr();
end;

procedure TfmConverter.lvCtrsEdited(Sender: TObject; Item: TListItem;
  var S: string);
var
  p: PZControllerInfo;
  nNewAddr: Integer;
begin
  if not TryStrToInt(S, nNewAddr) then
  begin
    MessageDlg(StrEInvalidCtrAddr, mtError, [mbOk], 0);
    exit;
  end;
  p := @m_aCtrs[Item.Index];
  if p.nSn <> 0 then
  begin
    FCvt.SetCtrAddrBySn(p.nSn, nNewAddr, p.nType);
    p.nAddr := nNewAddr;
  end
  else
  begin
    FCvt.SetCtrAddr(p.nAddr, nNewAddr, p.nType);
    p.nAddr := nNewAddr;
  end;
end;

procedure TfmConverter.miDevTypesAllClick(Sender: TObject);
begin
  SetDevTypes(True, True);
end;

procedure TfmConverter.miDevTypesEurolockClick(Sender: TObject);
begin
  SetDevTypes(False, True);
end;

procedure TfmConverter.miDevTypesGATEClick(Sender: TObject);
begin
  SetDevTypes(True, False);
end;

procedure TfmConverter.miRefreshClick(Sender: TObject);
begin
  UpdateControllers();
end;

procedure TfmConverter.PopupMenu1Popup(Sender: TObject);
begin
  if (not (csoNoGate in FCvt.ScanOptions)) and (not (csoNoEurolock in FCvt.ScanOptions)) then
    miDevTypesAll.Checked := True
  else if not (csoNoGate in FCvt.ScanOptions) then
    miDevTypesGATE.Checked := True
  else if not (csoNoEurolock in FCvt.ScanOptions) then
    miDevTypesEurolock.Checked := True;
end;

function EnumCtrsCB(AInfo: PZG_FIND_CTR_INFO; APos: Integer; AMax: Integer; AUserData: Pointer): LongBool; stdcall;
var
  n, nPct: Integer;
  pForm: TfmConverter;
begin
  pForm := TfmConverter(AUserData);
  n := Length(pForm.m_aCtrs);
  SetLength(pForm.m_aCtrs, n + 1);
  with pForm.m_aCtrs[n] do
  begin
    nType := AInfo.nType;
    nTypeCode := AInfo.nTypeCode;
    nAddr := AInfo.nAddr;
    nSn := AInfo.nSn;
    nVersion := AInfo.nVersion;
    nMaxKeys := AInfo.nMaxKeys;
    nMaxEvents := AInfo.nMaxEvents;
    nFlags := AInfo.nFlags;
  end;

  nPct := (APos * 100) div AMax;
  fmProcess.SetPct(nPct);
  Result := (not fmProcess.m_fCancelled);
end;

procedure TfmConverter.UpdateControllers(AForce: Boolean);
var
  nCurSel: Integer;
  nFlags: Cardinal;
begin
  nCurSel := lvCtrs.ItemIndex;
  lvCtrs.Items.Count := 0;
  SetLength(m_aCtrs, 0);
  Enabled := False;
  try
    fmProcess.Init(Self, StrSearchingCtrs_D);
    if AForce then
      nFlags := ZG_F_UPDATE
    else
      nFlags := 0;
    if csoNoGate in FCvt.ScanOptions then
      nFlags := nFlags or ZG_NF_CVT_NOGATE;
    if csoNoEurolock in FCvt.ScanOptions then
      nFlags := nFlags or ZG_NF_CVT_NOEUROLOCK;
    FCvt.EnumControllers(EnumCtrsCB, Self, nFlags)
  finally
    Enabled := True;
    fmProcess.Hide();
    lvCtrs.Items.Count := Length(m_aCtrs);
    lvCtrs.ItemIndex := min(nCurSel, Length(m_aCtrs)-1);
    lvCtrs.Invalidate();
  end;
end;

procedure TfmConverter.DoOpenSelCtr();
var
  nIdx: Integer;
  p: PZControllerInfo;
  oCtr: TZController;
begin
  nIdx := lvCtrs.ItemIndex;
  if nIdx = -1 then
    exit;
  p := @m_aCtrs[nIdx];
  oCtr := nil;
  FCvt.Notifications := False;
  try
    oCtr := TZController.Create();
    with oCtr do
    begin
      Addr := p.nAddr;
      Sn := p.nSn;
      CvtH := FCvt.Handle;
      CtrType := p.nType;
      Active := True;
    end;
    with fmController do
    begin
      PopupParent := Self;
      FCvt := Self.FCvt;
      FCtr := oCtr;
      ShowModal();
    end;
  finally
    oCtr.Free();
    FCvt.Notifications := True;
  end;
end;

end.
