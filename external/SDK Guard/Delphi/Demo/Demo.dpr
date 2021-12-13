program Demo;

uses
  Forms,
  Main in 'Main.pas' {fmMain},
  ConverterDlg in 'ConverterDlg.pas' {fmConverter},
  ControllerDlg in 'ControllerDlg.pas' {fmController},
  uConst in 'uConst.pas',
  uProcessDlg in 'uProcessDlg.pas' {fmProcess},
  CtrLockDlg in 'CtrLockDlg.pas' {fmCtrLock},
  CtrScheduleDlg in 'CtrScheduleDlg.pas' {fmCtrSchedule},
  CtrKeysDlg in 'CtrKeysDlg.pas' {fmCtrKeys},
  CtrEventsDlg in 'CtrEventsDlg.pas' {fmCtrEvents},
  CtrEvMonitorDlg in 'CtrEvMonitorDlg.pas' {fmCtrEvMonitor},
  uUtils in 'uUtils.pas',
  CtrTimeZoneDlg in 'CtrTimeZoneDlg.pas' {fmCtrTimeZone},
  CtrKeyDlg in 'CtrKeyDlg.pas' {fmCtrKey},
  CtrElectroDlg in 'CtrElectroDlg.pas' {fmCtrElectro},
  CtrModesDlg in 'CtrModesDlg.pas' {fmCtrModes};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmController, fmController);
  Application.CreateForm(TfmProcess, fmProcess);
  Application.CreateForm(TfmCtrLock, fmCtrLock);
  Application.CreateForm(TfmCtrSchedule, fmCtrSchedule);
  Application.CreateForm(TfmCtrKeys, fmCtrKeys);
  Application.CreateForm(TfmCtrEvents, fmCtrEvents);
  Application.CreateForm(TfmCtrEvMonitor, fmCtrEvMonitor);
  Application.CreateForm(TfmCtrTimeZone, fmCtrTimeZone);
  Application.CreateForm(TfmCtrKey, fmCtrKey);
  Application.CreateForm(TfmCtrElectro, fmCtrElectro);
  Application.CreateForm(TfmCtrModes, fmCtrModes);
  Application.Run;
end.
