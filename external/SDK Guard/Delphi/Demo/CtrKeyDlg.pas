unit CtrKeyDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CheckLst, ExtCtrls, ZBase, ZGuard, CtrScheduleDlg;

type
  TfmCtrKey = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    rgKeyType: TRadioGroup;
    grAccess: TGroupBox;
    roAccessEver: TRadioButton;
    roAccessNever: TRadioButton;
    roAccessSchedule: TRadioButton;
    cbTZs: TCheckListBox;
    grKeyNumber: TGroupBox;
    edtNum: TEdit;
    chkShortNum: TCheckBox;
    chkFunctional: TCheckBox;
    chkDual: TCheckBox;
    edtCode1: TEdit;
    edtCode2: TEdit;
    edtNum2: TEdit;
    procedure FormShow(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure chkDualClick(Sender: TObject);
    procedure edtCode1Change(Sender: TObject);
    procedure edtNumChange(Sender: TObject);
    procedure edtCode2Change(Sender: TObject);
    procedure edtNum2Change(Sender: TObject);
  private
    { Private declarations }
    m_nCtrlUpdate   : Integer;
    procedure UpdateCtrlsState();
  public
    { Public declarations }
    m_nMaxBanks     : Integer;
    m_fProximity    : Boolean;
    m_aSchedule     : TATimeZones;
    m_aScheduleOut  : TATimeZones;

    m_rNum          : TZ_KeyNum;
    m_nType         : TZG_Ctr_Key_Type;
    m_nAccess       : Cardinal;
    m_nFlags        : Cardinal;
    m_nKeyIdx       : Integer;
  end;

var
  fmCtrKey: TfmCtrKey;

implementation

{$R *.dfm}

uses
  StrUtils, uUtils, uConst;


procedure TfmCtrKey.btnOkClick(Sender: TObject);
var
  s: String;
  i: Integer;
begin
  ModalResult := mrNone;
  m_nType := TZG_CTR_KEY_TYPE(rgKeyType.ItemIndex + 1);
  if roAccessEver.Checked then
    m_nAccess := $FF
  else if roAccessNever.Checked then
    m_nAccess := 0
  else
  begin
    m_nAccess := 0;
    for i := 0 to cbTZs.Count - 1 do
      if cbTZs.Checked[i] then
        Inc(m_nAccess, (1 shl i));
  end;
  m_nFlags := 0;
  if chkShortNum.Checked then
    Inc(m_nFlags, ZG_KF_SHORTNUM);
  if chkFunctional.Checked then
    Inc(m_nFlags, ZG_KF_FUNCTIONAL);
  FillChar(m_rNum, SizeOf(m_rNum), 0);
  if chkDual.Checked then
  begin
    Inc(m_nFlags, ZG_KF_DUAL);
    s := ReplaceStr(edtNum.Text, ' ', '');
    if not ParseKeyNum(m_rNum, s, 0) then
    begin
      MessageDlg(StrEInvalidKeyNum, mtError, [mbOk], 0);
      edtNum.SetFocus();
      exit;
    end;
    s := ReplaceStr(edtNum2.Text, ' ', '');
    if not ParseKeyNum(m_rNum, s, 1) then
    begin
      MessageDlg(StrEInvalidKeyNum, mtError, [mbOk], 0);
      edtNum2.SetFocus();
      exit;
    end;
  end
  else
  begin
    s := ReplaceStr(edtNum.Text, ' ', '');
    if not ParseKeyNum(m_rNum, s) then
    begin
      MessageDlg(StrEInvalidKeyNum, mtError, [mbOk], 0);
      exit;
    end;
  end;
  ModalResult := mrOk;
end;

procedure TfmCtrKey.chkDualClick(Sender: TObject);
var
  s: String;
begin
  if m_nCtrlUpdate > 0 then
    Exit;
  if (m_nFlags and ZG_KF_DUAL) <> 0 then
  begin
    s := ReplaceStr(edtNum.Text, ' ', '');
    ParseKeyNum(m_rNum, s, 0);
    s := ReplaceStr(edtNum2.Text, ' ', '');
    ParseKeyNum(m_rNum, s, 1);
  end;
  if chkDual.Checked then
    m_nFlags := m_nFlags or ZG_KF_DUAL
  else
    m_nFlags := m_nFlags and not ZG_KF_DUAL;
  Inc(m_nCtrlUpdate);
  if (m_nFlags and ZG_KF_DUAL) <> 0 then
  begin
    edtNum.Text := KeyNumToStr(m_rNum, m_fProximity, 0);
    edtNum2.Text := KeyNumToStr(m_rNum, m_fProximity, 1);
    edtCode1.Text := ZKeyCodeToStr(PInteger(@m_rNum[1])^ and $FFFFFF, #0);
    edtCode2.Text := ZKeyCodeToStr(PInteger(@m_rNum[4])^ and $FFFFFF, #0);
  end
  else
    edtNum.Text := KeyNumToStr(m_rNum, m_fProximity);
  Dec(m_nCtrlUpdate);
  UpdateCtrlsState();
end;

procedure TfmCtrKey.edtCode1Change(Sender: TObject);
var
  n: Integer;
begin
  if m_nCtrlUpdate > 0 then
    Exit;
  n := ZKeyStrToCode(edtCode1.Text);
  Move(n, m_rNum[1], 3);
  Inc(m_nCtrlUpdate);
  edtNum.Text := KeyNumToStr(m_rNum, m_fProximity, 0);
  Dec(m_nCtrlUpdate);
end;

procedure TfmCtrKey.edtCode2Change(Sender: TObject);
var
  n: Integer;
begin
  if m_nCtrlUpdate > 0 then
    Exit;
  n := ZKeyStrToCode(edtCode2.Text);
  Move(n, m_rNum[4], 3);
  Inc(m_nCtrlUpdate);
  edtNum2.Text := KeyNumToStr(m_rNum, m_fProximity, 1);
  Dec(m_nCtrlUpdate);
end;

procedure TfmCtrKey.edtNum2Change(Sender: TObject);
var
  s: String;
begin
  if m_nCtrlUpdate > 0 then
    Exit;
  s := ReplaceStr(edtNum2.Text, ' ', '');
  if not ParseKeyNum(m_rNum, s, 1) then
    exit;
  Inc(m_nCtrlUpdate);
  edtCode2.Text := ZKeyCodeToStr(PInteger(@m_rNum[4])^ and $FFFFFF, #0);
  Dec(m_nCtrlUpdate);
end;

procedure TfmCtrKey.edtNumChange(Sender: TObject);
var
  s: String;
begin
  if m_nCtrlUpdate > 0 then
    Exit;
  if (m_nFlags and ZG_KF_DUAL) <> 0 then
  begin
    s := ReplaceStr(edtNum.Text, ' ', '');
    if not ParseKeyNum(m_rNum, s, 0) then
      exit;
    Inc(m_nCtrlUpdate);
    edtCode1.Text := ZKeyCodeToStr(PInteger(@m_rNum[1])^ and $FFFFFF, #0);
    Dec(m_nCtrlUpdate);
  end;
end;

procedure TfmCtrKey.UpdateCtrlsState();
var
  f: Boolean;
begin
  f := ((m_nFlags and ZG_KF_DUAL) <> 0);
  edtNum2.Enabled := f;
  edtCode1.Enabled := f;
  edtCode2.Enabled := f;
end;

procedure TfmCtrKey.FormShow(Sender: TObject);
var
  i, j: Integer;
  pTz, pTzOut: PZG_Ctr_TimeZone;
  s, sOut: String;
begin
  Caption := format('Key #%d', [m_nKeyIdx + 1]);
  Inc(m_nCtrlUpdate);
  rgKeyType.ItemIndex := Integer(m_nType) - 1;
  if m_nMaxBanks = 2 then
  begin
    for i := 0 to cbTZs.Count - 1 do
    begin
      pTz := @m_aSchedule[i];
      pTzOut := @m_aScheduleOut[i];
      s := 'MTWTFSS';
      for j := 0 to 6 do
        if not GetBit(pTz.nDayOfWeeks, j) then
          s[j + 1] := '-';
      sOut := 'MTWTFSS';
      for j := 0 to 6 do
        if not GetBit(pTzOut.nDayOfWeeks, j) then
          sOut[j + 1] := '-';
      cbTZs.Items[i] := format(StrLbTz2_DSDDDDSDDDD, [
          i + 1,
          s,
          pTz.nBegHour, pTz.nBegMinute, pTz.nEndHour, pTz.nEndMinute,
          sOut,
          pTzOut.nBegHour, pTzOut.nBegMinute, pTzOut.nEndHour, pTzOut.nEndMinute]);
    end;
  end
  else
  begin
    for i := 0 to cbTZs.Count - 1 do
    begin
      pTz := @m_aSchedule[i];
      s := 'MTWTFSS';
      for j := 0 to 6 do
        if not GetBit(pTz.nDayOfWeeks, j) then
          s[j + 1] := '-';
      cbTZs.Items[i] := format(StrLbTz1_DSDDDD, [
          i + 1,
          s,
          pTz.nBegHour, pTz.nBegMinute, pTz.nEndHour, pTz.nEndMinute]);
    end;
  end;

  if m_nAccess = $FF then
    roAccessEver.Checked := True
  else if m_nAccess = 0 then
    roAccessNever.Checked := True
  else
  begin
    roAccessSchedule.Checked := True;
    for i := 0 to cbTZs.Count - 1 do
      cbTZs.Checked[i] := GetBit(m_nAccess, i);
  end;
  chkShortNum.Checked := (m_nFlags and ZG_KF_SHORTNUM) <> 0;
  chkFunctional.Checked := (m_nFlags and ZG_KF_FUNCTIONAL) <> 0;
  chkDual.Checked := (m_nFlags and ZG_KF_DUAL) <> 0;
  if (m_nFlags and ZG_KF_DUAL) <> 0 then
  begin
    edtNum.Text := KeyNumToStr(m_rNum, m_fProximity, 0);
    edtNum2.Text := KeyNumToStr(m_rNum, m_fProximity, 1);
    edtCode1.Text := ZKeyCodeToStr(PInteger(@m_rNum[1])^ and $FFFFFF, #0);
    edtCode2.Text := ZKeyCodeToStr(PInteger(@m_rNum[4])^ and $FFFFFF, #0);
  end
  else
    edtNum.Text := KeyNumToStr(m_rNum, m_fProximity);
  Dec(m_nCtrlUpdate);
  UpdateCtrlsState();
end;

end.
