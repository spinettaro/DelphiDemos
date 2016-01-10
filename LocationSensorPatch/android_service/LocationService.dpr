program LocationService;

uses
  System.Android.ServiceApplication,
  LocationServiceU in 'LocationServiceU.pas' {AndroidServiceDM: TAndroidService},
  System.Android.SensorsDD in '..\patch\System.Android.SensorsDD.pas',
  System.SensorsDD in '..\patch\System.SensorsDD.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAndroidServiceDM, AndroidServiceDM);
  Application.Run;
end.
