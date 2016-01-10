unit MainU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, System.Android.Service;

type
  TForm4 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FLocationServiceConn: TLocalServiceConnection;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.fmx}

procedure TForm4.Button1Click(Sender: TObject);
begin
  FLocationServiceConn := TLocalServiceConnection.Create; // -- android_service
  FLocationServiceConn.StartService('LocationService'); // -- android_service
end;

end.
