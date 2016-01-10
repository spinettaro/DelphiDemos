unit LocationServiceU;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText,
  AndroidApi.JNI.Os, System.Android.SensorsDD, System.SensorsDD;

type
  TAndroidServiceDM = class(TAndroidService)
    function AndroidServiceStartCommand(const Sender: TObject;
      const Intent: JIntent; Flags, StartId: Integer): Integer;
  private
    { Private declarations }
    FSensor: TCustomLocationSensor;
    procedure StartLocationSensor;
  protected
    procedure OnLocationChange(Sender: TObject;
      const OldLocation, NewLocation: TLocationCoord2D);
  public
    { Public declarations }
  end;

var
  AndroidServiceDM: TAndroidServiceDM;

implementation

{ %CLASSGROUP 'FMX.Controls.TControl' }

uses AndroidApi.Log, AndroidApi.JNI.App;

{$R *.dfm}

procedure Log(const Fmt: string; const Params: array of const);
var
  Msg: string;
  M: TMarshaller;
begin
  Msg := Format(Fmt, Params);
  LOGI(M.AsUtf8(Msg).ToPointer);
end;

function TAndroidServiceDM.AndroidServiceStartCommand(const Sender: TObject;
  const Intent: JIntent; Flags, StartId: Integer): Integer;
begin
  Log('Location Service StartCommand', []);
  StartLocationSensor;
  Result := TJService.JavaClass.START_STICKY; // Keeps service running
end;

procedure TAndroidServiceDM.OnLocationChange(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
begin
  Log('New location available -> Latitude: %2.6f, Longitude: %2.6f ',
    [NewLocation.Latitude, NewLocation.Longitude]);
end;

procedure TAndroidServiceDM.StartLocationSensor;
var
  FSensors: TSensorArray;
  Sensor: TCustomSensor;
begin
  TSensorManager.Current.Active := true;
  FSensors := TSensorManager.Current.GetSensorsByCategory
    (TSensorCategory.Location);

  FSensor := nil;
  for Sensor in FSensors do
  begin
    Log('Sensor ORD: ' + (ord(TCustomLocationSensor(Sensor).SensorType)
      .ToString), []);
    if TCustomLocationSensor(Sensor).SensorType = TLocationSensorType.GPS then
    begin
      FSensor := TCustomLocationSensor(Sensor);
      Break;
    end;
  end;

  if not Assigned(FSensor) then
    Exit; { no location sensor is available }

  { start the sensor if it is not started }
  if not FSensor.Started then
  begin
    FSensor.Start;
  end;

  FSensor.OnLocationChanged := OnLocationChange;
end;

end.
