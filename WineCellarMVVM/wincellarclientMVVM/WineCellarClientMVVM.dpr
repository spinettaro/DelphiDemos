program WineCellarClientMVVM;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainFMX in 'Views\MainFMX.pas' {TabbedForm},
  WinesBO in '..\winecellarserver\WinesBO.pas',
  ViewModel.Main in 'ViewModels\ViewModel.Main.pas',
  RESTServicesU in 'Models\RESTServicesU.pas',
  System.Constant in 'System.Constant.pas',
  JobQueue in 'SupportCode\JobQueue.pas',
  EventsU in 'Events\EventsU.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.CreateForm(TTabbedForm, TabbedForm);
  Application.Run;

end.
