unit CtrScheduleDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ZGuard, ZGClasses, ComCtrls;

Const
  AutoSyncSecMin = 5; // Минимальное расхождение времени контроллера и времени ПК (в секундах)

type
  TATimeZones = array[0..ZG_MAX_TIMEZONES-1] of TZG_Ctr_TimeZone;
  PATimeZones = ^TATimeZones;

  TfmCtrSchedule = class(TForm)
    grTimeZonesOut: TGroupBox;
    grTimeZones: TGroupBox;
    btnTzDefault: TButton;
    grCtrClock: TGroupBox;
    labCtrCurrTime: TLabel;
    labSysCurrTime: TLabel;
    labCurCtrTime: TLabel;
    labCurSysTime: TLabel;
    btnTimeSync: TButton;
    chkClockAutoSync: TCheckBox;
    lbTzs: TListBox;
    lbTzsOut: TListBox;
    btnTzOutDefault: TButton;
    btnChangeTZ: TButton;
    btnChangeTZOut: TButton;
    Timer1: TTimer;
    procedure FormShow(Sender: TObject);
    procedure btnTzDefaultClick(Sender: TObject);
    procedure btnTzOutDefaultClick(Sender: TObject);
    procedure btnChangeTZClick(Sender: TObject);
    procedure btnChangeTZOutClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure btnTimeSyncClick(Sender: TObject);
    procedure lbTzsDblClick(Sender: TObject);
    procedure lbTzsOutDblClick(Sender: TObject);
  private
    { Private declarations }
    m_aTimeZones    : TATimeZones;  // Список временных зон
    m_aTimeZonesOut : TATimeZones;  // Список временных зон для выхода
    m_fClockValid   : Boolean;      // True, часы настроены корректно
    m_nCtrTimeSpan  : Int64;        // Величина расхождения часов контроллера с часами ПК (в сек)
    m_nCheckTimeN   : Integer;      // Номер обновления меток (Label) времени
    m_fDualZone     : Boolean;      // True, поддерживается второй набор вр.зон
    m_nB0TzCount    : Integer;      // Количество вр.зон в первом банке

    procedure DoTzDefault(AOut: Boolean);
    procedure DoChangeSelTz(AOut: Boolean);

    procedure UpdateTzLbItem(ALb: TListBox; AIdx: Integer;
        Const ATz: TZG_Ctr_TimeZone; ARedraw: Boolean=True);
    procedure UpdateTzLb(ALb: TListBox; Const ATzs: TATimeZones; ACount: Integer);
    procedure DoCheckTime();
    procedure UpdateTimes();
    procedure SyncCtrTime();
  public
    { Public declarations }
    FCtr            : TZController;
  end;

var
  fmCtrSchedule: TfmCtrSchedule;

implementation

{$R *.dfm}

uses
  DateUtils, uUtils, uConst, CtrTimeZoneDlg;


procedure TfmCtrSchedule.UpdateTimes();
Const
  _TimeFmt    = 'dd.mm.yyyy hh:nn:ss';
var
  dtCurrTime, dtCtrTime: TDateTime;
begin
  dtCurrTime := Now();
  if m_fClockValid then
  begin
    dtCtrTime := IncSecond(dtCurrTime, m_nCtrTimeSpan);
    labCurCtrTime.Caption := FormatDateTime(_TimeFmt, dtCtrTime);
  end
  else
    labCurCtrTime.Caption := '???';
  labCurSysTime.Caption := FormatDateTime(_TimeFmt, dtCurrTime);
  // Выделяем жирным если время рассинхронизировалось
  if (m_nCtrTimeSpan <> 0) <> (fsBold in labCurCtrTime.Font.Style) then
  begin
    if m_nCtrTimeSpan <> 0 then
      labCurCtrTime.Font.Style := labCurCtrTime.Font.Style + [fsBold]
    else
      labCurCtrTime.Font.Style := labCurCtrTime.Font.Style - [fsBold];
  end;
end;

procedure TfmCtrSchedule.DoChangeSelTz(AOut: Boolean);
var
  nIdx: Integer;
  lb: TListBox;
  pArr: PATimeZones;
begin
  if AOut then
  begin
    lb := lbTzsOut;
    pArr := @m_aTimeZonesOut;
  end
  else
  begin
    lb := lbTzs;
    pArr := @m_aTimeZones;
  end;
  nIdx := lb.ItemIndex;
  if nIdx = -1 then
    exit;
  fmCtrTimeZone.m_rTz := pArr^[nIdx];
  fmCtrTimeZone.m_nTimeZoneIdx := nIdx;
  if fmCtrTimeZone.ShowModal() <> mrOk then
    exit;
  if not AOut then
    FCtr.WriteTimeZones(nIdx, @fmCtrTimeZone.m_rTz, 1, nil, nil)
  else if m_fDualZone then
    FCtr.WriteTimeZones(ZG_DUAL_ZONE_TZ0 + nIdx, @fmCtrTimeZone.m_rTz, 1, nil, nil)
  else
    FCtr.WriteTimeZones(nIdx, @fmCtrTimeZone.m_rTz, 1, nil, nil, 1);
  pArr^[nIdx] := fmCtrTimeZone.m_rTz;
  UpdateTzLbItem(lb, nIdx, pArr^[nIdx]);
end;

procedure TfmCtrSchedule.btnChangeTZClick(Sender: TObject);
begin
  DoChangeSelTz(False);
end;

procedure TfmCtrSchedule.btnChangeTZOutClick(Sender: TObject);
begin
  DoChangeSelTz(True);
end;

procedure TfmCtrSchedule.SyncCtrTime();
var
  rClock: TZG_Ctr_Clock;
  rCurrTime: TSystemTime;
begin
  GetLocalTime(rCurrTime);
  FillChar(rClock, SizeOf(rClock), 0);
  with rClock do
  begin
    nYear := rCurrTime.wYear;
    nMonth := rCurrTime.wMonth;
    nDay := rCurrTime.wDay;
    nHour := rCurrTime.wHour;
    nMinute := rCurrTime.wMinute;
    nSecond := rCurrTime.wSecond;
  end;
  FCtr.SetClock(rClock);
  m_nCtrTimeSpan := 0;
  m_fClockValid := True;
  UpdateTimes();
end;

procedure TfmCtrSchedule.btnTimeSyncClick(Sender: TObject);
begin
  SyncCtrTime();
end;

procedure TfmCtrSchedule.DoTzDefault(AOut: Boolean);
var
  i: Integer;
  aNewTZs: TATimeZones;
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
  if not AOut then
    FCtr.WriteTimeZones(0, @aNewTZs[0], m_nB0TzCount, nil, nil)
  else if m_fDualZone then
    FCtr.WriteTimeZones(ZG_DUAL_ZONE_TZ0, @aNewTZs[0], Length(aNewTZs), nil, nil)
  else
    FCtr.WriteTimeZones(0, @aNewTZs[0], Length(aNewTZs), nil, nil, 1);

  if AOut then
  begin
    m_aTimeZonesOut := aNewTZs;
    UpdateTzLb(lbTZsOut, m_aTimeZonesOut, ZG_MAX_TIMEZONES);
  end
  else
  begin
    m_aTimeZones := aNewTZs;
    UpdateTzLb(lbTZs, m_aTimeZones, m_nB0TzCount);
  end;
end;

procedure TfmCtrSchedule.btnTzDefaultClick(Sender: TObject);
begin
  if MessageDlg(StrReally, mtConfirmation, [mbYes, mbNo, mbCancel], 0) <> mrYes then
    exit;
  DoTzDefault(False);
end;

procedure TfmCtrSchedule.btnTzOutDefaultClick(Sender: TObject);
begin
  if MessageDlg(StrReally, mtConfirmation, [mbYes, mbNo, mbCancel], 0) <> mrYes then
    exit;
  DoTzDefault(True);
end;

procedure TfmCtrSchedule.FormHide(Sender: TObject);
begin
  Timer1.Enabled := False;
end;

procedure TfmCtrSchedule.FormShow(Sender: TObject);
var
  rInfo: TZG_Ctr_Info;
  rClock: TZG_Ctr_Clock;
  dt: TDateTime;
  f2Banks: Boolean;
begin
  FillChar(rInfo, SizeOf(rInfo), 0);
  FCtr.GetInformation(rInfo);

  f2Banks := (rInfo.nFlags and ZG_CTR_F_2BANKS) <> 0;
  m_fDualZone := (not f2Banks) and ((rInfo.nFlags and ZG_CTR_F_DUAL_ZONE) <> 0);
  if (rInfo.nFlags and ZG_CTR_F_ELECTRO) <> 0 then
    m_nB0TzCount := (ZG_MAX_TIMEZONES - 1)
  else
    m_nB0TzCount := ZG_MAX_TIMEZONES;
  FCtr.ReadTimeZones(0, @m_aTimeZones[0], m_nB0TzCount, nil, nil);
  UpdateTzLb(lbTZs, m_aTimeZones, m_nB0TzCount);
  grTimeZonesOut.Visible := f2Banks or m_fDualZone;
  if f2Banks or m_fDualZone then
  begin
    if f2Banks then
      FCtr.ReadTimeZones(0, @m_aTimeZonesOut[0], Length(m_aTimeZonesOut), nil, nil, 1)
    else
      FCtr.ReadTimeZones(ZG_DUAL_ZONE_TZ0, @m_aTimeZonesOut[0], Length(m_aTimeZonesOut), nil, nil);
    UpdateTzLb(lbTZsOut, m_aTimeZonesOut, ZG_MAX_TIMEZONES);
  end;

  FCtr.GetClock(rClock);
  m_fClockValid := TryEncodeDateTime(rClock.nYear, rClock.nMonth, rClock.nDay,
      rClock.nHour, rClock.nMinute, rClock.nSecond, 0, dt);
  if m_fClockValid then
    m_nCtrTimeSpan := SecondsBetween(Now(), dt);
  UpdateTimes();
  m_nCheckTimeN := 0;
  Timer1.Enabled := True;
end;

procedure TfmCtrSchedule.lbTzsDblClick(Sender: TObject);
begin
  DoChangeSelTz(False);
end;

procedure TfmCtrSchedule.lbTzsOutDblClick(Sender: TObject);
begin
  DoChangeSelTz(True);
end;

procedure TfmCtrSchedule.DoCheckTime();
var
  rClock: TZG_Ctr_Clock;
  dt: TDateTime;
begin
  Inc(m_nCheckTimeN);
  if m_nCheckTimeN = 5 then
  begin
    m_nCheckTimeN := 0;
    FCtr.GetClock(rClock);
    m_fClockValid := TryEncodeDateTime(rClock.nYear, rClock.nMonth, rClock.nDay,
        rClock.nHour, rClock.nMinute, rClock.nSecond, 0, dt);
    if m_fClockValid then
      m_nCtrTimeSpan := SecondsBetween(Now(), dt);
  end;
  if (m_nCtrTimeSpan >= AutoSyncSecMin) and chkClockAutoSync.Checked then
    SyncCtrTime()
  else
    UpdateTimes();
end;

procedure TfmCtrSchedule.Timer1Timer(Sender: TObject);
begin
  try
    DoCheckTime();
  except
    Timer1.Enabled := False;
    raise;
  end;
end;

procedure TfmCtrSchedule.UpdateTzLbItem(ALb: TListBox; AIdx: Integer;
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
  ALb.Items[AIdx] := format('%d  %s  %.2d:%.2d-%.2d:%.2d', [
      AIdx + 1,
      s,
      ATz.nBegHour, ATz.nBegMinute,
      ATz.nEndHour, ATz.nEndMinute]);

  if ARedraw then
    ALb.Invalidate();
end;

procedure TfmCtrSchedule.UpdateTzLb(ALb: TListBox; Const ATzs: TATimeZones; ACount: Integer);
var
  i: Integer;
begin
  ALb.Items.BeginUpdate();
  try
    while ALb.Items.Count > ACount do
      ALb.Items.Delete(ALb.Items.Count - 1);
    while ALb.Items.Count < ACount do
      ALb.Items.Add('');
    for i := 0 to ACount - 1 do
      UpdateTzLbItem(ALb, i, ATzs[i], False);
  finally
    ALb.Items.EndUpdate();
  end;
  ALb.Invalidate();
end;

end.
