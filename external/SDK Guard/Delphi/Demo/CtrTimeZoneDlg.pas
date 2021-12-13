unit CtrTimeZoneDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, ComCtrls, ZGuard, ZGClasses, CheckLst;

type
  TfmCtrTimeZone = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    grDaysOfWeek: TGroupBox;
    dtpFrom: TDateTimePicker;
    dtpTo: TDateTimePicker;
    lbDows: TCheckListBox;
    grCtrMode: TGroupBox;
    cbMode: TComboBox;
    procedure FormShow(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    m_rTz           : TZG_Ctr_TimeZone;
    m_nTimeZoneIdx  : Integer;
  end;

var
  fmCtrTimeZone: TfmCtrTimeZone;

implementation

{$R *.dfm}

uses
  DateUtils, uUtils;


procedure TfmCtrTimeZone.btnOkClick(Sender: TObject);
var
  dt: TDateTime;
  i: Integer;
begin
  ModalResult := mrNone;
  dt := dtpFrom.Time;
  m_rTz.nBegHour := HourOf(dt);
  m_rTz.nBegMinute := MinuteOf(dt);
  dt := dtpTo.Time;
  m_rTz.nEndHour := HourOf(dt);
  m_rTz.nEndMinute := MinuteOf(dt);
  m_rTz.nDayOfWeeks := 0;
  for i := 0 to 6 do
    if lbDows.Checked[i] then
      Inc(m_rTz.nDayOfWeeks, 1 shl i);
  if m_rTz.nMode <> ZG_MODE_UNDEF then
    m_rTz.nMode := TZG_Ctr_Mode(cbMode.ItemIndex + 1);
  ModalResult := mrOk;
end;

procedure TfmCtrTimeZone.FormShow(Sender: TObject);
var
  i: Integer;
begin
  dtpFrom.Time := EncodeTime(m_rTz.nBegHour, m_rTz.nBegMinute, 0, 0);
  dtpTo.Time := EncodeTime(m_rTz.nEndHour, m_rTz.nEndMinute, 0, 0);
  for i := 0 to 6 do
    lbDows.Checked[i] := GetBit(m_rTz.nDayOfWeeks, i);
  if m_rTz.nMode <> ZG_MODE_UNDEF then
  begin
    cbMode.ItemIndex := Integer(m_rTz.nMode) - 1;
    grCtrMode.Visible := True;
  end
  else
    grCtrMode.Visible := False;
end;

end.
