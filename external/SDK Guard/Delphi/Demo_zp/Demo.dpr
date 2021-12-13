program Demo;

uses
  Forms,
  Main in 'Main.pas' {fmMain},
  ZPort in '..\ZPort.pas',
  ZPClasses in '..\ZPClasses.pas',
  uConst in 'uConst.pas',
  ZRetrConst in '..\ZRetrConst.pas',
  Port in 'Port.pas' {fmPort},
  uUtils in 'uUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
