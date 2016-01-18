program SaveState;

uses
  System.StartUpCopy,
  FMX.Forms,
  SaveStateFRM in 'SaveStateFRM.pas' {Form1} ,
  SaveStateHelper in 'SaveStateHelper.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
