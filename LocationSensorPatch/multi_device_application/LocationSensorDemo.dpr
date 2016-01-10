program LocationSensorDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainU in 'MainU.pas' {Form4},
  LocationServiceU in '..\android_service\LocationServiceU.pas' {AndroidServiceDM: TAndroidService},
  System.Android.SensorsDD in '..\patch\System.Android.SensorsDD.pas',
  System.SensorsDD in '..\patch\System.SensorsDD.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
