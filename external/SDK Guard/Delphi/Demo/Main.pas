unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Menus, Contnrs, Types,
  ZGuard, ZPort, ZGClasses, ZPClasses, uConst;

Const
  Main_MinWidth = 630;
  Main_MinHeight = 150;
type
  TZConverterInfo = class
    nPortType       : TZP_PORT_TYPE;  // Тип порта
    sPortName       : String;         // Имя порта
    sFriendly       : String;         // Дружественное имя порта
    nType           : TZG_CVT_TYPE;   // Тип конвертера
    nSn             : Word;           // с/н конвертера
    nVersion        : Cardinal;       // Версия конвертера
    nMode           : TZG_GUARD_MODE; // Режим работы конвертера Guard

    m_pCvt          : TZConverter;
    m_pForm         : TForm;

    destructor Destroy(); override;
  end;

  TfmMain = class(TForm)
    lvCvts: TListView;
    btnOpen: TButton;
    btnUpdateFW: TButton;
    PopupMenu1: TPopupMenu;
    miRefresh: TMenuItem;
    OpenDialog1: TOpenDialog;
    cbSpeed: TComboBox;
    miDevTypes: TMenuItem;
    miDevTypesUsb: TMenuItem;
    miDevTypesIP: TMenuItem;
    miDevTypesAll: TMenuItem;
    miShowUnkDev: TMenuItem;
    edtPort: TEdit;
    labPortName: TLabel;
    cbMode: TComboBox;
    labActCode: TLabel;
    cbCvtSn: TComboBox;
    labCvtSn: TLabel;
    btnSnRefresh: TButton;
    cbActCode: TComboBox;
    chkZ397: TCheckBox;
    StatusBar1: TStatusBar;
    miSearchBackground: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvCvtsData(Sender: TObject; Item: TListItem);
    procedure btnOpenClick(Sender: TObject);
    procedure miRefreshClick(Sender: TObject);
    procedure btnUpdateFWClick(Sender: TObject);
    procedure lvCvtsDblClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure miDevTypesUsbClick(Sender: TObject);
    procedure miDevTypesIPClick(Sender: TObject);
    procedure miDevTypesAllClick(Sender: TObject);
    procedure miShowUnkDevClick(Sender: TObject);
    procedure lvCvtsCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer;
      var Resize: Boolean);
    procedure lvCvtsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure cbModeChange(Sender: TObject);
    procedure btnSnRefreshClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miSearchBackgroundClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    m_fNeedUpdCvts  : Boolean;

    procedure DetectorPortInsert(Sender: TObject; Const AInfo: TZP_DDN_Port_Info);
    procedure DetectorPortRemove(Sender: TObject; Const AInfo: TZP_DDN_Port_Info);
    procedure DetectorDeviceInsert(ASender: TObject; Const AInfo: TZp_DDN_Device_Info);
    procedure DetectorDeviceRemove(ASender: TObject; Const AInfo: TZp_DDN_Device_Info);

    procedure CvtFormClose(Sender: TObject; var Action: TCloseAction);
    procedure AppIdle(Sender: TObject; var Done: Boolean);
  public
    { Public declarations }
    m_oDetector     : TCvtDetector;
    m_oCvts         : TObjectList;
    m_fShowUnkDev   : Boolean;
    m_fBkgndSearch  : Boolean;
    m_fCfgModifyed  : Boolean;

    procedure UpdateConverters(AForce: Boolean=False);
    procedure EnableBkgndSearch(AEnable: Boolean; AUpdate: Boolean=True);

    procedure DoRefresh();
    procedure DoOpen();
    procedure DoCloseSelCvt();
    procedure DoUpdateFWSelCvt();
    procedure SetDevTypes(ATypes: TZDevTypes);
    function FindCvtInfoByForm(AForm: TObject): Integer;
    function FindCvtInfoByPortName(Const APort: String): Integer;
    procedure UpdateProxyCtrlState();
    procedure DoRefreshSNs();
    function GetPortCtrlData(var VType: TZP_Port_Type): String;

    // Загрузка настроек программы
    procedure LoadConfig();
    // Сохранение настроек программы
    procedure SaveConfig();
    function DevTypesToInt(AValue: TZDevTypes): Integer;
    function IntToDevTypes(AValue: Integer): TZDevTypes;
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

uses
  Math, IniFiles,
  uUtils, ConverterDlg, uProcessDlg;


destructor TZConverterInfo.Destroy();
begin
  if m_pForm <> nil then
    m_pForm.Free();
  if m_pCvt <> nil then
    m_pCvt.Free();
  inherited Destroy();
end;

function UpdateFW_CB(APos: Integer; AMax: Integer; AUserData: Pointer): LongBool; stdcall;
var
  nPct: Integer;
begin
  nPct := (APos * 100) div AMax;
  fmProcess.SetPct(nPct);
  Result := (not fmProcess.m_fCancelled);
end;

procedure TfmMain.DoUpdateFWSelCvt();
var
  nIdx: Integer;
  p: TZConverterInfo;
  oData: TMemoryStream;
  rOp: TZG_Cvt_Open_Params;
begin
  nIdx := lvCvts.ItemIndex;
  if nIdx = -1 then
    exit;
  p := m_oCvts[nIdx] as TZConverterInfo;
  if not OpenDialog1.Execute(Handle) then
    exit;
  oData := nil;
  Enabled := False;
  try
    oData := TMemoryStream.Create();
    oData.LoadFromFile(OpenDialog1.FileName);
    fmProcess.Init(Self, StrUpdateCvtFW_D);
    FillChar(rOp, SizeOf(rOp), 0);
    rOp.nType := p.nPortType;
    rOp.pszName := PWideChar(p.sPortName);
    rOp.nSpeed := ZG_SPEED_19200;
    CheckZGError(ZG_UpdateCvtFirmware(@rOp, oData.Memory^, oData.Size, UpdateFW_CB, nil));
  finally
    Enabled := True;
    fmProcess.Hide();
    oData.Free();
  end;
end;

procedure TfmMain.DoRefreshSNs();
var
  sPort, sActCode: String;
  nPortType: TZP_Port_Type;
  aSNs: TWordDynArray;
  nCount, i, nSn: Integer;
begin
  sPort := GetPortCtrlData(nPortType);
  if (sPort = '') or (nPortType <> ZP_Port_IP) then
  begin
    MessageDlg(StrIncorrectPortName, mtWarning, [mbOk], 0);
    Exit;
  end;
  sActCode := cbActCode.Text;
  if sActCode = '' then
  begin
    MessageDlg(StrIncorrectActCode, mtWarning, [mbOk], 0);
    Exit;
  end;
  if (sActCode <> '') and (cbActCode.Items.IndexOf(sActCode) = -1) then
    cbActCode.Items.Add(sActCode);
  nCount := GetProxyConverters(aSNs, sPort, sActCode);
  with cbCvtSn do
  begin
    nSn := StrToIntDef(Text, 0);
    Items.BeginUpdate();
    try
      Items.Clear();
      for i := 0 to nCount - 1 do
        Items.Add(IntToStr(aSNs[i]));
    finally
      Items.EndUpdate();
    end;
    if (nSn = 0) and (nCount > 0) then
      Text := IntToStr(aSNs[0]);
  end;
end;

procedure TfmMain.btnSnRefreshClick(Sender: TObject);
begin
  DoRefreshSNs();
end;

procedure TfmMain.btnUpdateFWClick(Sender: TObject);
begin
  DoUpdateFWSelCvt();
end;

procedure TfmMain.UpdateProxyCtrlState();
var
  f: Boolean;
begin
  f := (cbMode.ItemIndex = 2);
  labActCode.Enabled := f;
  cbActCode.Enabled := f;
  labCvtSn.Enabled := f;
  cbCvtSn.Enabled := f;
  btnSnRefresh.Enabled := f;
end;

procedure TfmMain.cbModeChange(Sender: TObject);
begin
  UpdateProxyCtrlState();
end;

function TfmMain.FindCvtInfoByPortName(Const APort: String): Integer;
var
  p: TZConverterInfo;
begin
  for Result := 0 to m_oCvts.Count - 1 do
  begin
    p := TZConverterInfo(m_oCvts[Result]);
    if (p.sPortName = APort) or (p.sFriendly = APort) then
      Exit;
  end;
  Result := -1;
end;

function TfmMain.FindCvtInfoByForm(AForm: TObject): Integer;
begin
  for Result := 0 to m_oCvts.Count - 1 do
    if TZConverterInfo(m_oCvts[Result]).m_pForm = AForm then
      Exit;
  Result := -1;
end;

procedure TfmMain.CvtFormClose(Sender: TObject; var Action: TCloseAction);
var
  n: Integer;
  pInf: TZConverterInfo;
begin
  n := FindCvtInfoByForm(Sender);
  if n = -1 then
    Exit;
  pInf := m_oCvts[n] as TZConverterInfo;
  if pInf.m_pForm <> nil then
    FreeAndNil(pInf.m_pForm);
  if pInf.m_pCvt <> nil then
    FreeAndNil(pInf.m_pCvt);
  lvCvts.Invalidate();
end;

function TfmMain.GetPortCtrlData(var VType: TZP_Port_Type): String;
begin
  Result := edtPort.Text;
  if Result = '' then
    Exit;
  if SameText(Copy(Result, 1, 3), 'COM') then
  begin
    VType := ZP_PORT_COM;
    Result := UpperCase(Result);
  end
  else if Pos('\', Result) > 0 then
    VType := ZP_PORT_IPS
  else if Pos(':', Result) > 0 then
    VType := ZP_PORT_IP
  else
    VType := ZP_PORT_FT;
end;

procedure TfmMain.DoOpen();
var
  nIdx, nSn: Integer;
  p: TZConverterInfo;
  nSpeed: TZG_Cvt_Speed;
  sPort, sActCode: String;
  nPortType: TZP_Port_Type;
begin
  sPort := GetPortCtrlData(nPortType);
  if sPort = '' then
  begin
    MessageDlg(StrIncorrectPortName, mtWarning, [mbOk], 0);
    Exit;
  end;
  nSn := 0;
  if nPortType = ZP_PORT_IP then
  begin
    case cbMode.ItemIndex of
      1: nPortType := ZP_PORT_IPS;
      2:
      begin
        sActCode := cbActCode.Text;
        nSn := StrToIntDef(cbCvtSn.Text, 0);
        if (sActCode <> '') and (nSn = 0) then
        begin
          MessageDlg(StrIncorrectCvtSn, mtWarning, [mbOk], 0);
          Exit;
        end;
      end;
    end;
  end;
  if (sActCode <> '') and (cbActCode.Items.IndexOf(sActCode) = -1) then
    cbActCode.Items.Add(sActCode);

  nIdx := FindCvtInfoByPortName(sPort);
  if nIdx = -1 then
  begin
    p := TZConverterInfo.Create();
    try
      p.nPortType := nPortType;
      p.sPortName := sPort;
      m_oCvts.Add(p);
    except
      p.Free();
      raise;
    end;
  end
  else
    p := m_oCvts[nIdx] as TZConverterInfo;
  if p.m_pForm <> nil then
  begin
    p.m_pForm.Show();
    exit;
  end;
  try
    if cbSpeed.ItemIndex = 1 then
      nSpeed := ZG_SPEED_57600
    else
      nSpeed := ZG_SPEED_19200;
    p.m_pCvt := TZConverter.Create(sPort, nPortType);
    p.m_pForm := TfmConverter.Create(Self);
    p.m_pCvt.Speed := nSpeed;
    p.m_pCvt.ProxyActCode := sActCode;
    p.m_pCvt.ProxyCvtSn := nSn;
    if chkZ397.Checked then
      p.m_pCvt.CvtType := ZG_CVT_Z397;
    p.m_pCvt.Active := True;
    TfmConverter(p.m_pForm).FCvt := p.m_pCvt;
    p.m_pForm.Show();
    p.m_pForm.OnClose := CvtFormClose;
  except
    if p.m_pForm <> nil then
      FreeAndNil(p.m_pForm);
    if p.m_pCvt <> nil then
      FreeAndNil(p.m_pCvt);
    raise;
  end;
  lvCvts.Invalidate();
end;

procedure TfmMain.DoCloseSelCvt();
var
  nIdx: Integer;
  p: TZConverterInfo;
begin
  nIdx := lvCvts.ItemIndex;
  if nIdx = -1 then
    exit;
  p := m_oCvts[nIdx] as TZConverterInfo;
  if p.m_pForm <> nil then
    p.m_pForm.Close();
end;

procedure TfmMain.btnOpenClick(Sender: TObject);
begin
  DoOpen();
end;

procedure TfmMain.DetectorPortInsert(Sender: TObject; Const AInfo: TZP_DDN_Port_Info);
begin
  StatusBar1.SimpleText := format(StrPortInsert_SSS, [
    TimeToStr(Now()),
    AInfo.rPort.szName,
    AInfo.rPort.szFriendly]);
  m_fNeedUpdCvts := True;
end;

procedure TfmMain.DetectorPortRemove(Sender: TObject; Const AInfo: TZP_DDN_Port_Info);
begin
  StatusBar1.SimpleText := format(StrPortRemove_SSS, [
    TimeToStr(Now()),
    AInfo.rPort.szName,
    AInfo.rPort.szFriendly]);
  m_fNeedUpdCvts := True;
end;

procedure TfmMain.DetectorDeviceInsert(ASender: TObject; Const AInfo: TZp_DDN_Device_Info);
var
  pDevInfo: PZg_Enum_Cvt_Info;
  sPort: String;
begin
  pDevInfo := PZg_Enum_Cvt_Info(AInfo.pInfo);
  if (AInfo.aPorts.nType = Zp_Port_Ft) and (AInfo.aPorts.szFriendly[0] <> #0) then
    sPort := StrPas(AInfo.aPorts.szFriendly)
  else
    sPort := StrPas(AInfo.aPorts.szName);
  StatusBar1.SimpleText := format(StrCvtInsert_SSDS, [
    TimeToStr(Now()),
    CvtTypeStrs[pDevInfo.nType],
    pDevInfo.rBase.nSn,
    sPort]);
  m_fNeedUpdCvts := True;
end;

procedure TfmMain.DetectorDeviceRemove(ASender: TObject; Const AInfo: TZp_DDN_Device_Info);
var
  pDevInfo: PZg_Enum_Cvt_Info;
  sPort: String;
begin
  pDevInfo := PZg_Enum_Cvt_Info(AInfo.pInfo);
  if (AInfo.aPorts.nType = Zp_Port_Ft) and (AInfo.aPorts.szFriendly[0] <> #0) then
    sPort := StrPas(AInfo.aPorts.szFriendly)
  else
    sPort := StrPas(AInfo.aPorts.szName);
  StatusBar1.SimpleText := format(StrCvtRemove_SSDS, [
    TimeToStr(Now()),
    CvtTypeStrs[pDevInfo.nType],
    pDevInfo.rBase.nSn,
    sPort]);
  m_fNeedUpdCvts := True;
end;

procedure TfmMain.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if NewWidth < Main_MinWidth then
    NewWidth := Main_MinWidth;
  if NewHeight < Main_MinHeight then
    NewHeight := Main_MinHeight;
end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if m_fCfgModifyed then
    SaveConfig();
end;

procedure TfmMain.AppIdle(Sender: TObject; var Done: Boolean);
begin
  if m_fNeedUpdCvts and (Application.ModalLevel = 0) then
    UpdateConverters();
end;

function TfmMain.DevTypesToInt(AValue: TZDevTypes): Integer;
begin
  Result := 0;
  if devSerial in AValue then
    Inc(Result, 1);
  if devIP in AValue then
    Inc(Result, 2);
end;

function TfmMain.IntToDevTypes(AValue: Integer): TZDevTypes;
begin
  Result := [];
  if (AValue and 1) <> 0 then
    Result := Result + [devSerial];
  if (AValue and 2) <> 0 then
    Result := Result + [devIP];
end;

Const
  Cfg_Main = 'Main';
  Cfg_DevTypes = 'DevTypes';
  Cfg_ShowUnkDev = 'ShowUnkDev';
  Cfg_SearchBkgnd = 'SearchBkgnd';

procedure TfmMain.LoadConfig();
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    m_fShowUnkDev := ini.ReadBool(Cfg_Main, Cfg_ShowUnkDev, m_fShowUnkDev);
    EnableBkgndSearch(ini.ReadBool(Cfg_Main, Cfg_SearchBkgnd, m_fBkgndSearch), False);
    SetDevTypes(IntToDevTypes(ini.ReadInteger(Cfg_Main, Cfg_DevTypes, DevTypesToInt(m_oDetector.DevTypes))));
  finally
    ini.Free();
  end;
  m_fCfgModifyed := False;
end;

procedure TfmMain.SaveConfig();
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    ini.WriteBool(Cfg_Main, Cfg_ShowUnkDev, m_fShowUnkDev);
    ini.WriteBool(Cfg_Main, Cfg_SearchBkgnd, m_fBkgndSearch);
    ini.WriteInteger(Cfg_Main, Cfg_DevTypes, DevTypesToInt(m_oDetector.DevTypes));
    ini.UpdateFile();
  finally
    ini.Free();
  end;
  m_fCfgModifyed := False;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  // Проверяем версию библиотеки
  if not CheckZGVersion() then
  begin
    MessageDlg(StrWrongSdkVer, mtError, [mbOk], 0);
    Halt;
  end;
  // Инициализируем библиотеку
  ZGInitialize(ZP_IF_LOG or ZG_IF_ERROR_LOG);

  m_oCvts := TObjectList.Create();
  m_oDetector := TCvtDetector.Create();
//  m_oDetector.DevTypes := [devSerial,devIP];
  m_oDetector.DevTypes := [devSerial];
  m_oDetector.OnPortInsert := DetectorPortInsert;
  m_oDetector.OnPortRemove := DetectorPortRemove;

  Application.OnIdle := AppIdle;
  LoadConfig();
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  m_oDetector.Free();
  m_oCvts.Free();
  ZGFinalyze();
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  m_oDetector.Enabled := (m_oDetector.DevTypes <> []);
  // Обновляем список конвертеров
  UpdateConverters();
  UpdateProxyCtrlState();
end;

procedure TfmMain.lvCvtsCustomDrawItem(Sender: TCustomListView; Item: TListItem;
  State: TCustomDrawState; var DefaultDraw: Boolean);
var
  p: TZConverterInfo;
  fs: TFontStyles;
begin
  p := m_oCvts[Item.Index] as TZConverterInfo;
  with Sender.Canvas do
  begin
    fs := Font.Style;
    if p.m_pForm <> nil then
      Font.Style := fs + [fsBold]
    else
      Font.Style := fs - [fsBold];
  end;
end;

procedure TfmMain.lvCvtsData(Sender: TObject; Item: TListItem);
var
  p: TZConverterInfo;
begin
  p := m_oCvts[Item.Index] as TZConverterInfo;
  if p.sFriendly <> '' then
    Item.Caption := format('%s (%s)', [p.sPortName, p.sFriendly])
  else
    Item.Caption := p.sPortName;
  while Item.SubItems.Count < 4 do
    Item.SubItems.Add('');
  Item.SubItems[0] := CvtTypeStrs[p.nType];
  if p.nType >= ZG_CVT_Z397_GUARD then
  begin
    Item.SubItems[1] := IntToStr(p.nSn);
    Item.SubItems[2] := format('%d.%d.%d.%d', [
        p.nVersion and $ff,
        (p.nVersion shr 8) and $ff,
        (p.nVersion shr 16) and $ff,
        (p.nVersion shr 24) and $ff]);
    Item.SubItems[3] := CvtModeStrs[p.nMode];
  end
  else
  begin
    Item.SubItems[1] := '';
    Item.SubItems[2] := '';
    Item.SubItems[3] := '';
  end;
end;

procedure TfmMain.lvCvtsDblClick(Sender: TObject);
var
  nIdx: Integer;
  p: TZConverterInfo;
begin
  nIdx := lvCvts.ItemIndex;
  if nIdx = -1 then
    exit;
  p := m_oCvts[nIdx] as TZConverterInfo;
  if p.m_pForm <> nil then
    DoCloseSelCvt()
  else
    DoOpen();
end;

procedure TfmMain.lvCvtsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  p: TZConverterInfo;
begin
  if Selected then
  begin
    p := m_oCvts[Item.Index] as TZConverterInfo;
    if (p.nPortType = ZP_PORT_FT) and (p.sFriendly <> '') then
      edtPort.Text := p.sFriendly
    else
      edtPort.Text := p.sPortName;
    if p.nPortType = ZP_PORT_IPS then
      cbMode.ItemIndex := 1
    else if (p.nPortType <> ZP_PORT_IP) or (cbMode.ItemIndex = 1) then
      cbMode.ItemIndex := 0;
  end;
end;

procedure TfmMain.SetDevTypes(ATypes: TZDevTypes);
begin
  if m_oDetector.DevTypes = ATypes then
    exit;
  m_oDetector.DevTypes := ATypes;
  m_oDetector.Enabled := (ATypes <> []);
  UpdateConverters();
end;

procedure TfmMain.miDevTypesAllClick(Sender: TObject);
begin
  SetDevTypes([devSerial, devIP]);
  m_fCfgModifyed := True;
end;

procedure TfmMain.miDevTypesIPClick(Sender: TObject);
begin
  SetDevTypes([devIP]);
  m_fCfgModifyed := True;
end;

procedure TfmMain.miDevTypesUsbClick(Sender: TObject);
begin
  SetDevTypes([devSerial]);
  m_fCfgModifyed := True;
end;

procedure TfmMain.DoRefresh();
begin
  UpdateConverters(True);
end;

procedure TfmMain.miRefreshClick(Sender: TObject);
begin
  DoRefresh();
end;

procedure TfmMain.miSearchBackgroundClick(Sender: TObject);
begin
  EnableBkgndSearch(not m_fBkgndSearch);
  m_fCfgModifyed := True;
end;

procedure TfmMain.miShowUnkDevClick(Sender: TObject);
begin
  m_fShowUnkDev := not m_fShowUnkDev;
  m_fCfgModifyed := True;
  UpdateConverters();
end;

procedure TfmMain.PopupMenu1Popup(Sender: TObject);
begin
  if m_oDetector.DevTypes = [devSerial, devIP] then
    miDevTypesAll.Checked := True
  else if devSerial in m_oDetector.DevTypes then
    miDevTypesUsb.Checked := True
  else if devIP in m_oDetector.DevTypes then
    miDevTypesIP.Checked := True;
  miShowUnkDev.Checked := m_fShowUnkDev;
  miSearchBackground.Checked := m_fBkgndSearch;
end;

procedure TfmMain.EnableBkgndSearch(AEnable: Boolean; AUpdate: Boolean);
begin
  m_fBkgndSearch := AEnable;
  if AEnable then
  begin
    m_oDetector.OnDeviceInsert := DetectorDeviceInsert;
    m_oDetector.OnDeviceRemove := DetectorDeviceRemove;
  end
  else
  begin
    m_oDetector.OnDeviceInsert := nil;
    m_oDetector.OnDeviceRemove := nil;
  end;
  if AUpdate then
    UpdateConverters();
end;

procedure TfmMain.UpdateConverters(AForce: Boolean);
var
  nStart: Cardinal;
  nCurSel, i, nIdx: Integer;
  pInf: TZConverterInfo;
  hSearch: THandle;
  rParams: TZp_Search_Params;
  nPortCount: Integer;
  hr: HResult;
  rInfo: TZg_Enum_IpCvt_Info;
  aPI: array[0..1] of TZP_Port_Info;
  pPI: PZp_Port_Info;
  o: TZConverterInfo;
begin
  m_fNeedUpdCvts := False;
  nCurSel := lvCvts.ItemIndex;
  if AForce then
  begin
    StatusBar1.SimpleText := StrSearching;
    Application.ProcessMessages();
  end;
  nStart := GetTickCount();
  try
    lvCvts.Items.Count := 0;
    for i := m_oCvts.Count - 1 downto 0 do
    begin
      pInf := m_oCvts[i] as TZConverterInfo;
      if pInf.m_pCvt = nil then
        m_oCvts.Delete(i);
    end;
//    if devSerial in m_oDetector.DevTypes then
//      EnumConverters(@CvtEnum, nil);
//    if devIP in m_oDetector.DevTypes then
//      EnumIpConverters(@IpCvtEnum, nil);
    FillChar(rParams, SizeOf(rParams), 0);
    if m_oDetector.DevTypes <> [] then
    begin
      if AForce and m_fBkgndSearch then
        m_oDetector.Refresh(INFINITE);

      rParams.nFlags := ZP_SF_IPS;
      if devSerial in m_oDetector.DevTypes then
        rParams.nDevMask := $FFFFFFFF;
      if devIP in m_oDetector.DevTypes then
        rParams.nIpDevMask := $FFFFFFFF;
      if m_fShowUnkDev then
        rParams.nFlags := rParams.nFlags or ZP_SF_UNID;
      if m_fBkgndSearch then
        rParams.nFlags := rParams.nFlags or ZP_SF_DETECTOR;

      CheckZPError(ZP_SearchDevices(hSearch, rParams));
      try
        repeat
          rInfo.rBase.cbSize := SizeOf(rInfo);
          hr := ZP_FindNextDevice(hSearch, @rInfo, @aPI[0], Length(aPI), nPortCount);
          CheckZPError(hr);
          if hr = ZP_S_NOTFOUND then
            break;
          if hr = ZP_S_TIMEOUT then
            break;
          for i := 0 to nPortCount - 1 do
          begin
            pPI := @aPI[i];
            nIdx := FindCvtInfoByPortName(StrPas(pPI.szName));
            if nIdx <> -1 then
              o := m_oCvts[nIdx] as TZConverterInfo
            else
              o := TZConverterInfo.Create();
            try
              with o do
              begin
                nPortType := pPI.nType;
                sPortName := StrPas(pPI.szName);
                sFriendly := StrPas(pPI.szFriendly);
                if (rInfo.nType = ZG_CVT_UNDEF) and
                    (rInfo.rBase.nModel <= Cardinal(ZG_CVT_Z5R_WEB)) then
                  nType := TZG_CVT_TYPE(rInfo.rBase.nModel)
                else
                  nType := rInfo.nType;
                nSn := rInfo.rBase.nSn;
                nVersion := rInfo.rBase.nVersion;
                nMode := rInfo.nMode;
              end;
              if nIdx = -1 then
                m_oCvts.Add(o);
            except
              if nIdx = -1 then
                o.Free();
              raise;
            end;
          end;
        until False;
      finally
        ZP_CloseHandle(hSearch);
      end;
    end;
  finally
    lvCvts.Items.Count := m_oCvts.Count;
    lvCvts.ItemIndex := min(nCurSel, m_oCvts.Count - 1);
    lvCvts.Invalidate();
    if AForce then
      StatusBar1.SimpleText := format(StrSearchDone_D, [GetTickSpan(nStart, GetTickCount())]);
  end;
end;

end.
