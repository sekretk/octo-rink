unit Port;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ZPClasses, ExtCtrls, ZPort;

type
  TfmPort = class(TForm)
    Memo1: TMemo;
    btnSend: TButton;
    btnClear: TButton;
    cbCmd: TComboBox;
    procedure btnSendClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    m_nWaitCounter  : Integer;
    m_fAnswer       : Boolean;

    procedure PortRxChar(Sender: TObject);
    procedure PortStatusChange(Sender: TObject);

    procedure Send(Const AData; ACount: Integer);
  public
    { Public declarations }
    FPort           : TZPort;
  end;

var
  fmPort: TfmPort;

implementation

{$R *.dfm}

uses
  uConst;

procedure TfmPort.Send(Const AData; ACount: Integer);
begin
  FPort.Write(AData, ACount);
  m_nWaitCounter := 30;
  m_fAnswer := False;
end;

procedure TfmPort.PortRxChar(Sender: TObject);
var
  n: Integer;
  s: AnsiString;
begin
  n := FPort.GetInCount();
  if n > 0 then
  begin
    m_fAnswer := True;
    SetLength(s, n);
    SetLength(s, FPort.Read(s[1], n));
    Memo1.Text := Memo1.Text + String(s);
    SendMessage(Memo1.Handle, EM_LINESCROLL, 0, Memo1.Lines.Count-1);
  end;
  if m_nWaitCounter > 0 then
  begin
    Dec(m_nWaitCounter);
    if (m_nWaitCounter = 0) and (not m_fAnswer) then
      MessageDlg('No answer.', mtError, [mbOk], 0);
  end;
end;

procedure TfmPort.PortStatusChange(Sender: TObject);
begin
  Memo1.Lines.Add(format('Status changed: %s', [
      ConnectionStatusStrs[FPort.GetConnectionStatus()]]));
end;

procedure TfmPort.btnClearClick(Sender: TObject);
begin
  Memo1.Clear();
end;

procedure TfmPort.btnSendClick(Sender: TObject);
var
  s: AnsiString;
begin
  s := AnsiString(cbCmd.Text) + #13#10;
  Send(s[1], Length(s));
end;

procedure TfmPort.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FPort.OnRxChar := nil;
  FPort.OnStatusChange := nil;
end;

procedure TfmPort.FormShow(Sender: TObject);
begin
  Caption := format('Port "%s", speed: %d', [
    FPort.PortName,
    FPort.Baud]);
  Memo1.Clear();
  m_nWaitCounter := 0;
  FPort.OnRxChar := PortRxChar;
  FPort.OnStatusChange := PortStatusChange;
end;

end.
