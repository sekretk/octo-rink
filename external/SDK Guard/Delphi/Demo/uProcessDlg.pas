unit uProcessDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfmProcess = class(TForm)
    labDescription: TLabel;
    ProgressBar1: TProgressBar;
    btnCancel: TButton;
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FPctStr         : String;
    m_fCancelled    : Boolean;

    procedure Init(AOwner: TCustomForm; Const APctStr: String; APos: Integer=0);
    procedure SetPct(AValue: Integer);
  end;

var
  fmProcess: TfmProcess;

implementation

{$R *.dfm}

procedure TfmProcess.btnCancelClick(Sender: TObject);
begin
  m_fCancelled := True;
end;

procedure TfmProcess.Init(AOwner: TCustomForm; Const APctStr: String; APos: Integer);
begin
  FPctStr := APctStr;
  m_fCancelled := False;
  PopupParent := AOwner;
  labDescription.Caption := format(FPctStr, [APos]);
  ProgressBar1.Position := APos;
  Show();
  Application.ProcessMessages();
end;

procedure TfmProcess.SetPct(AValue: Integer);
begin
  labDescription.Caption := format(FPctStr, [AValue]);
  ProgressBar1.Position := AValue;
  Application.ProcessMessages();
end;

end.
