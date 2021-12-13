unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Spin, Menus, Contnrs,
  ZPort, ZPClasses, ZRetrConst, uConst;

Const
  Min_Main_Width    = 500;
  Min_Main_Height   = 200;
type
  TDeviceInfo = class
    m_nType         : TZP_Port_Type;  // Тип порта
    m_sName         : String;         // Имя порта
    m_sFriendly     : String;         // Дружественное имя порта
    m_fBusy         : Boolean;        // True, если занят

    m_nDevType      : TDevType;
    m_nDevSn        : Integer;
    m_nDevVersion   : Cardinal;

    m_pPort         : TZPort;
    m_pForm         : TForm;

    destructor Destroy(); override;
  end;

  TfmMain = class(TForm)
    lvPorts: TListView;
    btnRescan: TButton;
    PopupMenu1: TPopupMenu;
    miRefresh: TMenuItem;
    miDevTypes: TMenuItem;
    miDevTypesSerial: TMenuItem;
    miDevTypesIP: TMenuItem;
    miDevTypesAll: TMenuItem;
    btnOpen: TButton;
    Label1: TLabel;
    labPort: TLabel;
    miPortOpen: TMenuItem;
    N1: TMenuItem;
    cbBaud: TComboBox;
    miDevTypesNone: TMenuItem;
    cbPort: TComboBox;
    StatusBar1: TStatusBar;
    miShowUnkDev: TMenuItem;
    miSearchBackground: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnRescanClick(Sender: TObject);
    procedure lvPortsData(Sender: TObject; Item: TListItem);
    procedure miDevTypesAllClick(Sender: TObject);
    procedure miDevTypesIPClick(Sender: TObject);
    procedure miDevTypesSerialClick(Sender: TObject);
    procedure miRefreshClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure lvPortsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure lvPortsDblClick(Sender: TObject);
    procedure FormCanResize(Sender: TObject; var NewWidth, NewHeight: Integer;
      var Resize: Boolean);
    procedure miPortOpenClick(Sender: TObject);
    procedure miDevTypesNoneClick(Sender: TObject);
    procedure cbPortSelect(Sender: TObject);
    procedure miShowUnkDevClick(Sender: TObject);
    procedure miSearchBackgroundClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    m_nPortIdx      : Integer;
    m_fNeedUpdDevs  : Boolean;

    procedure DetectorInsert(Sender: TObject; Const AInfo: TZp_DDN_Port_Info);
    procedure DetectorRemove(Sender: TObject; Const AInfo: TZp_DDN_Port_Info);
    procedure DetectorDevInsert(ASender: TObject; Const AInfo: TZp_DDN_Device_Info);
    procedure DetectorDevRemove(ASender: TObject; Const AInfo: TZp_DDN_Device_Info);
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    procedure PortFormClose(Sender: TObject; var Action: TCloseAction);
  public
    { Public declarations }
    m_oDetector     : TZPDetector;
    m_oDevices      : TObjectList;
    m_fShowUnkDev   : Boolean;
    m_fBkgndSearch  : Boolean;
    m_fCfgModifyed  : Boolean;

    procedure UpdateDevices(AForce: Boolean=False);
    procedure SetDevTypes(ATypes: TZDevTypes; AUpdate: Boolean=True);
    procedure EnableBkgndSearch(AEnable: Boolean; AUpdate: Boolean=True);
    procedure DoOpen();
    procedure DoRefresh();

    function FindPortInfoByName(Const AName: String): Integer;
    function FindPortInfoByForm(AForm: TObject): Integer;

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
  StrUtils, Character, Math, IniFiles,
  uUtils, Port;


destructor TDeviceInfo.Destroy();
begin
  if m_pForm <> nil then
    m_pForm.Free();
  if m_pPort <> nil then
    m_pPort.Free();
  inherited Destroy();
end;

procedure TfmMain.SetDevTypes(ATypes: TZDevTypes; AUpdate: Boolean);
begin
  if (m_oDetector.Enabled = (ATypes <> [])) and
      ((not m_oDetector.Enabled) or (m_oDetector.DevTypes = ATypes)) then
    Exit;
  if ATypes = [] then
    m_oDetector.Enabled := False
  else
  begin
    m_oDetector.DevTypes := ATypes;
    m_oDetector.Enabled := True;
  end;
  if AUpdate then
    UpdateDevices();
end;

function TfmMain.FindPortInfoByForm(AForm: TObject): Integer;
begin
  for Result := 0 to m_oDevices.Count - 1 do
    if TDeviceInfo(m_oDevices[Result]).m_pForm = AForm then
      Exit;
  Result := -1;
end;

procedure TfmMain.PortFormClose(Sender: TObject; var Action: TCloseAction);
var
  n: Integer;
  pInf: TDeviceInfo;
begin
  n := FindPortInfoByForm(Sender);
  if n = -1 then
    Exit;
  pInf := TDeviceInfo(m_oDevices[n]);
  if pInf.m_pForm <> nil then
    FreeAndNil(pInf.m_pForm);
  if pInf.m_pPort <> nil then
    FreeAndNil(pInf.m_pPort);
  lvPorts.Invalidate();
end;

procedure TfmMain.DoOpen();
var
  sPort: String;
  nType: TZP_Port_Type;
  nIdx, nBaud: Integer;
  pInf: TDeviceInfo;
begin
  sPort := cbPort.Text;
  if sPort = '' then
    Exit;

  if SameText(Copy(sPort, 1, 3), 'COM') then
    nType := ZP_PORT_COM
  else if Pos(':', sPort) <> 0 then
    nType := ZP_PORT_IP
  else if Pos('\', sPort) <> 0 then
    nType := ZP_PORT_IPS
  else
    nType := ZP_PORT_FT;
  if not TryStrToInt(cbBaud.Text, nBaud) then
    Exit;

  nIdx := FindPortInfoByName(sPort);
  if nIdx <> -1 then
    pInf := TDeviceInfo(m_oDevices[nIdx])
  else
  begin
    pInf := TDeviceInfo.Create();
    try
      pInf.m_nType := nType;
      pInf.m_sName := sPort;
      m_oDevices.Add(pInf);
    except
      pInf.Free();
      raise;
    end;
  end;
  if pInf.m_pForm <> nil then
  begin
    pInf.m_pForm.Show();
    exit;
  end;
  try
    pInf.m_pPort := TZPort.Create();
    pInf.m_pPort.PortName := sPort;
    pInf.m_pPort.PortType := nType;
    pInf.m_pPort.Baud := nBaud;
    pInf.m_pPort.Opened := True;
    pInf.m_pForm := TfmPort.Create(Self);
    TfmPort(pInf.m_pForm).FPort := pInf.m_pPort;
    pInf.m_pForm.Show();
    pInf.m_pForm.OnClose := PortFormClose;
  except
    if pInf.m_pForm <> nil then
      FreeAndNil(pInf.m_pForm);
    if pInf.m_pPort <> nil then
      FreeAndNil(pInf.m_pPort);
    raise;
  end;
  lvPorts.Invalidate();
end;

procedure TfmMain.btnOpenClick(Sender: TObject);
begin
  DoOpen();
end;

procedure TfmMain.btnRescanClick(Sender: TObject);
begin
  DoRefresh();
end;

procedure TfmMain.cbPortSelect(Sender: TObject);
begin
  if cbPort.ItemIndex <> -1 then
    m_nPortIdx := cbPort.ItemIndex;
end;

procedure TfmMain.DetectorInsert(Sender: TObject; Const AInfo: TZp_DDN_Port_Info);
begin
  StatusBar1.SimpleText := format(StrPortInsert_SSS, [
    TimeToStr(Now()),
    AInfo.rPort.szName,
    AInfo.rPort.szFriendly]);
  m_fNeedUpdDevs := True;
end;

procedure TfmMain.DetectorRemove(Sender: TObject; Const AInfo: TZp_DDN_Port_Info);
begin
  StatusBar1.SimpleText := format(StrPortRemove_SSS, [
    TimeToStr(Now()),
    AInfo.rPort.szName,
    AInfo.rPort.szFriendly]);
  m_fNeedUpdDevs := True;
end;

procedure TfmMain.DetectorDevInsert(ASender: TObject; Const AInfo: TZp_DDN_Device_Info);
var
  pDevInfo: PZp_Device_Info;
  sPort: String;
begin
  pDevInfo := AInfo.pInfo;
  if (AInfo.aPorts.nType = Zp_Port_Ft) and (AInfo.aPorts.szFriendly[0] <> #0) then
    sPort := StrPas(AInfo.aPorts.szFriendly)
  else
    sPort := StrPas(AInfo.aPorts.szName);
  StatusBar1.SimpleText := format(StrDevInsert_SDDS, [
    TimeToStr(Now()),
    pDevInfo.nModel,
    pDevInfo.nSn,
    sPort]);
  m_fNeedUpdDevs := True;
end;

procedure TfmMain.DetectorDevRemove(ASender: TObject; Const AInfo: TZp_DDN_Device_Info);
var
  pDevInfo: PZp_Device_Info;
  sPort: String;
begin
  pDevInfo := AInfo.pInfo;
  if (AInfo.aPorts.nType = Zp_Port_Ft) and (AInfo.aPorts.szFriendly[0] <> #0) then
    sPort := StrPas(AInfo.aPorts.szFriendly)
  else
    sPort := StrPas(AInfo.aPorts.szName);
  StatusBar1.SimpleText := format(StrDevRemove_SDDS, [
    TimeToStr(Now()),
    pDevInfo.nModel,
    pDevInfo.nSn,
    sPort]);
  m_fNeedUpdDevs := True;
end;

procedure TfmMain.EnableBkgndSearch(AEnable: Boolean; AUpdate: Boolean);
begin
  m_fBkgndSearch := AEnable;
  if AEnable then
  begin
    m_oDetector.OnDeviceInsert := DetectorDevInsert;
    m_oDetector.OnDeviceRemove := DetectorDevRemove;
  end
  else
  begin
    m_oDetector.OnDeviceInsert := nil;
    m_oDetector.OnDeviceRemove := nil;
  end;
  if AUpdate then
    UpdateDevices();
end;

procedure TfmMain.AppIdle(Sender: TObject; var Done: Boolean);
begin
  if m_fNeedUpdDevs and (Application.ModalLevel = 0) then
    UpdateDevices();
end;

procedure TfmMain.FormCanResize(Sender: TObject; var NewWidth,
  NewHeight: Integer; var Resize: Boolean);
begin
  if NewWidth < Min_Main_Width then
    NewWidth := Min_Main_Width;
  if NewHeight < Min_Main_Height then
    NewHeight := Min_Main_Height;
end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if m_fCfgModifyed then
    SaveConfig();
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
var
  rSDev: TZp_USB_Device;
  rIpDev: TZp_Ip_Device;
begin
  if not CheckZpVersion() then
  begin
    MessageDlg('Wrong SDK version.', mtError, [mbOk], 0);
    halt;
  end;
  ZPInit();

  FillChar(rSDev, SizeOf(rSDev), 0);
  rSDev.rBase.nTypeId := 0;
  rSDev.pVidPids := @RdVidPids;
  rSDev.nVidPidCount := Length(RdVidPids);
  rSDev.rBase.pReqData := cmdI;
  rSDev.rBase.nReqSize := StrLen(cmdI);
  rSDev.rBase.pfnParse := RdParse;
  rSDev.rBase.nDevInfoSize := SizeOf(TZP_Device_Info);
  rSDev.nBaud := 9600;
  rSDev.chEvent := #10;
  ZP_RegSerialDevice(rSDev);
  rSDev.rBase.nTypeId := 3;
  rSDev.pszBDesc := 'USB IronLogic RFID Adapter';
  ZP_RegSerialDevice(rSDev);
  rSDev.rBase.nTypeId := 4;
  rSDev.pszBDesc := 'USB IL Mifare Adapter';
  ZP_RegSerialDevice(rSDev);

  rSDev.rBase.nTypeId := 1;
  rSDev.pVidPids := @CvtVidPids;
  rSDev.nVidPidCount := Length(CvtVidPids);
  rSDev.rBase.pReqData := cmdI;
  rSDev.rBase.nReqSize := StrLen(cmdI);
  rSDev.rBase.pfnParse := CvtParse;
  rSDev.rBase.nDevInfoSize := SizeOf(TCvtInfo);
  rSDev.nBaud := 230400;
  rSDev.chEvent := #13;
  rSDev.pszBDesc := 'Z397 GUARD Converter';
  ZP_RegSerialDevice(rSDev);

  FillChar(rSDev, SizeOf(rSDev), 0);
  rSDev.rBase.nTypeId := 2;
  rSDev.pVidPids := @CvtVidPids;
  rSDev.nVidPidCount := Length(CvtVidPids);
  rSDev.rBase.nDevInfoSize := SizeOf(TCvtInfo);
  rSDev.nBaud := 230400;
  rSDev.chEvent := #13;
  rSDev.pszBDesc := 'USB <-> RS-485/422';
  ZP_RegSerialDevice(rSDev);

  FillChar(rIpDev, SizeOf(rIpDev), 0);
  rIpDev.rBase.nTypeId := (ZP_MAX_REG_DEV);
  rIpDev.nReqPort := 9000;
  rIpDev.rBase.pReqData := PAnsiChar('SEEK Z397IP');
  rIpDev.rBase.nReqSize := StrLen(PAnsiChar(rIpDev.rBase.pReqData));
  rIpDev.rBase.pfnParse := IpParse;
  rIpDev.rBase.nDevInfoSize := SizeOf(TCvtInfo);
  ZP_RegIpDevice(rIpDev);

  m_oDevices := TObjectList.Create();
  m_oDetector := TZPDetector.Create();
  m_oDetector.DevTypes := [devSerial, devIP];
  m_oDetector.OnPortInsert := DetectorInsert;
  m_oDetector.OnPortRemove := DetectorRemove;
  m_oDetector.Enabled := True;
  cbPort.Clear();

  Application.OnIdle := AppIdle;
  LoadConfig();
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  m_oDetector.Free();
  m_oDevices.Free();
  ZPFinalyze();
end;

function TfmMain.FindPortInfoByName(Const AName: String): Integer;
var
  p: TDeviceInfo;
begin
  for Result := 0 to m_oDevices.Count - 1 do
  begin
    p := TDeviceInfo(m_oDevices[Result]);
    if (p.m_sName = AName) or (p.m_sFriendly = AName) then
      Exit;
  end;
  Result := -1;
end;

procedure TfmMain.UpdateDevices(AForce: Boolean);
var
  nStart: Cardinal;
  nCurSel, i, nIdx: Integer;
  nPortCount: Integer;
  hSearch: THandle;
  rParams: TZp_Search_Params;
  hr: HResult;
  rInfo: TCvtInfo;
  aPI: array[0..1] of TZP_Port_Info;
  pPI: PZp_Port_Info;
  pInf: TDeviceInfo;
begin
  m_fNeedUpdDevs := False;
  lvPorts.Items.Count := 0;
  if AForce then
  begin
    StatusBar1.SimpleText := StrSearching;
    Application.ProcessMessages();
  end;
  nCurSel := lvPorts.ItemIndex;
  nStart := GetTickCount();
  try
    for i := m_oDevices.Count - 1 downto 0 do
    begin
      pInf := m_oDevices[i] as TDeviceInfo;
      if pInf.m_pPort = nil then
        m_oDevices.Delete(i);
    end;
    FillChar(rParams, SizeOf(rParams), 0);
    if m_oDetector.DevTypes <> [] then
    begin
      if AForce and m_fBkgndSearch then
        m_oDetector.Refresh(INFINITE);

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
          for i := 0 to nPortCount - 1 do
          begin
            pPI := @aPI[i];
            nIdx := FindPortInfoByName(StrPas(pPI.szName));
            if nIdx <> -1 then
              pInf := TDeviceInfo(m_oDevices[nIdx])
            else
              pInf := TDeviceInfo.Create();
            try
              with pInf do
              begin
                m_nType := pPI.nType;
                m_sName := StrPas(pPI.szName);
                m_sFriendly := StrPas(pPI.szFriendly);
                m_fBusy := (pPI.nFlags and ZP_PIF_BUSY) <> 0;

                if rInfo.rBase.cbSize > 0 then
                begin
                  m_nDevType := TDevType(rInfo.rBase.nModel);
                  m_nDevSn := rInfo.rBase.nSn;
                  m_nDevVersion := rInfo.rBase.nVersion;
                end;
              end;
              if nIdx = -1 then
                nIdx := m_oDevices.Add(pInf);
            except
              if nIdx = -1 then
                pInf.Free();
              raise;
            end;
          end;
        until False;
      finally
        ZP_CloseHandle(hSearch);
      end;
    end;
  finally
    if AForce then
      StatusBar1.SimpleText := format(StrSearchComplite_D, [GetTickSpan(nStart, GetTickCount())]);
    lvPorts.Items.Count := m_oDevices.Count;
    if m_oDevices.Count > 0 then
    begin
      if nCurSel < 0 then
        nCurSel := 0
      else if nCurSel >= m_oDevices.Count then
        nCurSel := (m_oDevices.Count - 1);
      lvPorts.ItemIndex := nCurSel;
    end;
    lvPorts.Invalidate();
  end;
end;

procedure TfmMain.DoRefresh();
begin
  UpdateDevices(True);
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  UpdateDevices();
end;

procedure TfmMain.lvPortsData(Sender: TObject; Item: TListItem);
var
  nIdx: Integer;
  p: TDeviceInfo;
begin
  nIdx := Item.Index;
  ASSERT((nIdx >= 0) and (nIdx < m_oDevices.Count));
  p := TDeviceInfo(m_oDevices[nIdx]);
  if p.m_sFriendly <> '' then
    Item.Caption := format('%s (%s)', [p.m_sName, p.m_sFriendly])
  else
    Item.Caption := p.m_sName;
  while Item.SubItems.Count < 4 do
    Item.SubItems.Add('');
  Item.SubItems[0] := YesNoStrs[p.m_fBusy];
  Item.SubItems[1] := DevTypeStrs[p.m_nDevType];
  if p.m_nDevType <> dev_Undef then
  begin
    Item.SubItems[2] := IntToStr(p.m_nDevSn);
    Item.SubItems[3] := format('%d.%d.%d.%d', [
        p.m_nDevVersion and $ff,
        (p.m_nDevVersion shr 8) and $ff,
        (p.m_nDevVersion shr 16) and $ff,
        (p.m_nDevVersion shr 24) and $ff]);
  end
  else
  begin
    Item.SubItems[2] := '';
    Item.SubItems[3] := '';
  end;
end;

procedure TfmMain.lvPortsDblClick(Sender: TObject);
begin
  DoOpen();
end;

procedure TfmMain.lvPortsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  n: Integer;
  p: TDeviceInfo;
begin
  if Selected then
  begin
    p := TDeviceInfo(m_oDevices[Item.Index]);
//    edtPort.Text := p.m_sPort;
    with cbPort do
    begin
      Items.BeginUpdate();
      try
        Items.Clear();
        Items.Add(p.m_sName);
        if p.m_sFriendly <> '' then
          Items.Add(p.m_sFriendly);
      finally
        Items.EndUpdate();
      end;
      ItemIndex := min(m_nPortIdx, Items.Count - 1);
    end;
    n := StrToIntDef(cbBaud.Text, 0);
    case p.m_nDevType of
      dev_Z397:
        if (n <> 19200) or (n <> 57600) then
          n := 19200;
      dev_Z397_GUARD,
      dev_Z397_IP:
        n := 230400;
      dev_Z2U..dev_Z2EHR:
        n := 9600;
    end;
    cbBaud.Text := IntToStr(n);
  end;
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

procedure TfmMain.miDevTypesNoneClick(Sender: TObject);
begin
  SetDevTypes([]);
  m_fCfgModifyed := True;
end;

procedure TfmMain.miDevTypesSerialClick(Sender: TObject);
begin
  SetDevTypes([devSerial]);
  m_fCfgModifyed := True;
end;

procedure TfmMain.miPortOpenClick(Sender: TObject);
begin
  DoOpen();
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
  UpdateDevices();
end;

procedure TfmMain.PopupMenu1Popup(Sender: TObject);
begin
  if not m_oDetector.Enabled then
    miDevTypesNone.Checked := True
  else if m_oDetector.DevTypes = [devSerial, devIP] then
    miDevTypesAll.Checked := True
  else if devSerial in m_oDetector.DevTypes then
    miDevTypesSerial.Checked := True
  else if devIP in m_oDetector.DevTypes then
    miDevTypesIP.Checked := True;
  miShowUnkDev.Checked := m_fShowUnkDev;
  miSearchBackground.Checked := m_fBkgndSearch;
end;

end.
