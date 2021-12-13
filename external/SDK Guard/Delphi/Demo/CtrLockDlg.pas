unit CtrLockDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, ZGuard, ZGClasses;

type
  TfmCtrLock = class(TForm)
    grLockOut: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    edtLockMaxTimeout: TSpinEdit;
    edtLockLetTimeout: TSpinEdit;
    edtLockOpenTimeout: TSpinEdit;
    btnLockTimes2Defaullt: TButton;
    grLockTimes: TGroupBox;
    labLockMaxTime: TLabel;
    labLockLetTime: TLabel;
    labLockOpenTime: TLabel;
    edtLockMaxTime: TSpinEdit;
    edtLockLetTime: TSpinEdit;
    edtLockOpenTime: TSpinEdit;
    btnLockTimesDefaullt: TButton;
    btnOk: TButton;
    btnCancel: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnLockTimesDefaulltClick(Sender: TObject);
    procedure btnLockTimes2DefaulltClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FCtr            : TZController;
  end;

var
  fmCtrLock: TfmCtrLock;

implementation

{$R *.dfm}

procedure TfmCtrLock.btnLockTimes2DefaulltClick(Sender: TObject);
begin
  edtLockOpenTimeout.Value := 3000;
  edtLockLetTimeout.Value := 0;
  edtLockMaxTimeout.Value := 0;
end;

procedure TfmCtrLock.btnLockTimesDefaulltClick(Sender: TObject);
begin
  edtLockOpenTime.Value := 3000;
  edtLockLetTime.Value := 0;
  edtLockMaxTime.Value := 0;
end;

procedure TfmCtrLock.btnOkClick(Sender: TObject);
var
  nOpenTime, nLetTime, nMaxTime: Cardinal;
begin
  ModalResult := mrNone;
  nOpenTime := edtLockOpenTime.Value;
  nLetTime := edtLockLetTime.Value;
  nMaxTime := edtLockMaxTime.Value;
  FCtr.WriteLockTimes($7, nOpenTime, nLetTime, nMaxTime);
  if grLockOut.Visible then
  begin
    nOpenTime := edtLockOpenTimeout.Value;
    nLetTime := edtLockLetTimeout.Value;
    nMaxTime := edtLockMaxTimeout.Value;
    FCtr.WriteLockTimes($7, nOpenTime, nLetTime, nMaxTime, 1);
  end;
  ModalResult := mrOk;
end;

procedure TfmCtrLock.FormShow(Sender: TObject);
var
  rInfo: TZG_Ctr_Info;
  nOpenTime, nLetTime, nMaxTime: Cardinal;
  f2Banks: Boolean;
begin
  FillChar(rInfo, SizeOf(rInfo), 0);
  FCtr.GetInformation(rInfo);
  FCtr.ReadLockTimes(@nOpenTime, @nLetTime, @nMaxTime);
  f2Banks := (rInfo.nFlags and ZG_CTR_F_2BANKS) <> 0;
  edtLockOpenTime.Value := nOpenTime;
  edtLockLetTime.Value := nLetTime;
  edtLockMaxTime.Value := nMaxTime;
  grLockOut.Visible := f2Banks;
  if f2Banks then
  begin
    FCtr.ReadLockTimes(@nOpenTime, @nLetTime, @nMaxTime, 1);
    edtLockOpenTimeout.Value := nOpenTime;
    edtLockLetTimeout.Value := nLetTime;
    edtLockMaxTimeout.Value := nMaxTime;
  end;
end;

end.
