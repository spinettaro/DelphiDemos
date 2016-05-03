program MarvelAPI;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  MarvelAPIFMX in 'MarvelAPIFMX.pas' {HeaderFooterForm},
  ImgLoadThreadU in 'ImgLoadThreadU.pas',
  ConstantU in 'ConstantU.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(THeaderFooterForm, HeaderFooterForm);
  Application.Run;
end.
