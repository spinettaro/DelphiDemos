{ ******************************************************* }
{ }
{ CodeGear Delphi Runtime Library }
{ Copyright(c) 2013-2015 Embarcadero Technologies, Inc. }
{ }
{ ******************************************************* }

unit System.Android.SensorsDD;

interface

uses
  // -- patch dd
  // use the modified System.SensorsDD
  System.SensorsDD;

type

  TPlatformSensorManager = class(TSensorManager)
  protected
    class function GetSensorManager: TSensorManager; override;
  end;

  TPlatformGeocoder = class(TGeocoder)
  protected
    class function GetGeocoderImplementer: TGeocoderClass; override;
  end;

  TPlatformGpsStatus = class(TGpsStatus)
  protected
    class function GetGpsStatusImplementer: TGpsStatusClass; override;
  end;

implementation

uses
  System.SysUtils, System.Generics.Collections,
  Androidapi.Sensor, Androidapi.AppGlue, Androidapi.Looper, System.Math,
  Androidapi.Jni,
  Androidapi.JNIBridge, Androidapi.Jni.Location, Androidapi.Jni.JavaTypes,
  Androidapi.Jni.Os, Androidapi.Jni.App, Androidapi.NativeActivity,
  Androidapi.Jni.GraphicsContentViewText, Androidapi.Helpers, System.DateUtils;

{ Permissions manager }
type
  TPermission = class
  private
    class var FPermissions: TJavaObjectArray<JString>;
  private
    class constructor Create;
    class destructor Destroy;
  public
    class function IsPermitted(const APermission: JString): Boolean;
    class function GetList: TJavaObjectArray<JString>;
  end;

  { TPermission }

class function TPermission.IsPermitted(const APermission: JString): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FPermissions.Length - 1 do
    if FPermissions[I].equalsIgnoreCase(APermission) then
    begin
      Result := True;
      Break;
    end;
end;

class constructor TPermission.Create;
var
  PackageInfo: JPackageInfo;
  PackageManager: JPackageManager;
  Activity: JActivity;
  LContext: JContext;
begin
  // -- patch dd
  // Activity := TJNativeActivity.Wrap
  // (PANativeActivity(System.DelphiActivity)^.clazz)
  LContext := TJContextWrapper.Wrap(System.JavaContext);
  PackageManager := LContext.getPackageManager();
  PackageInfo := PackageManager.getPackageInfo
    (LContext.getApplicationContext.getPackageName,
    TJPackageManager.JavaClass.GET_PERMISSIONS);
  FPermissions := PackageInfo.requestedPermissions;
end;

class destructor TPermission.Destroy;
begin

end;

class function TPermission.GetList: TJavaObjectArray<JString>;
begin
  Result := FPermissions;
end;

type
  TAndroidGeocoder = class(TGeocoder)
  private type
    TGeocoderRunnable = class(TJavaLocal, JRunnable)
    private
      FCoord: TLocationCoord2D;
      FLGeocoder: JGeocoder;
    public
      constructor Create(ACoord: TLocationCoord2D; AGeocoder: JGeocoder);
      procedure run; cdecl;
    end;
  private
  class var
    FGeocoder: JGeocoder;
    // FActivity: JActivity; // -- patch dd
    FActivity: JContextWrapper; // -- patch dd
  private
    class constructor Create;
    class destructor Destroy;
  protected
    class function GetGeocoderImplementer: TGeocoderClass; override;
    class procedure GeocodeRequest(const AAddress: TCivicAddress); override;
    class procedure GeocodeReverseRequest(const Coords
      : TLocationCoord2D); override;
  public
    class function Supported: Boolean; override;
    class function Authorized: TAuthorizationType; override;
    class procedure Cancel; override;
  end;

type
  TUIAndroidLocationSensor = class(TCustomLocationSensor)
  private
    FPermitted: Boolean;
    // FActivity: JNativeActivity; // -- patch dd
    FActivity: JContext; // -- patch dd
    FLastValue: JLocation;
    FLocationManager: JLocationManager;
    FAccuracy: TLocationAccuracy;
    FDistance: TLocationDistance;

  type
    TLocationListener = class(TJavaLocal, JLocationListener)
    private
      FLocationSensor: TUIAndroidLocationSensor;
    public
      constructor Create(ALocationSensor: TUIAndroidLocationSensor);
      procedure onLocationChanged(P1: JLocation); cdecl;
      procedure onStatusChanged(P1: JString; P2: Integer; P3: JBundle); cdecl;
      procedure onProviderEnabled(P1: JString); cdecl;
      procedure onProviderDisabled(P1: JString); cdecl;
    end;

    TLocationRunnable = class(TJavaLocal, JRunnable)
    private
      FLocationManager: JLocationManager;
      FListener: TLocationListener;
      FProvider: JString;
    public
      constructor Create(ALocationManager: JLocationManager;
        AListener: TLocationListener; AProvider: JString);
      procedure run; cdecl;
    end;
  private
    FGPSListener: TLocationListener;
    FGPSRunner: TLocationRunnable;
    FNetworkListener: TLocationListener;
    FNetworkRunner: TLocationRunnable;
    FPassiveListener: TLocationListener;
    FPassiveRunner: TLocationRunnable;
  protected
    function DoStart: Boolean; override;
    procedure DoStop; override;
    function GetLocationSensorType: TLocationSensorType; override;
    function GetAvailableProperties: TCustomLocationSensor.TProperties;
      override;
    function GetDoubleProperty(Prop: TCustomLocationSensor.TProperty)
      : Double; override;
    function GetStringProperty(Prop: TCustomLocationSensor.TProperty)
      : string; override;
    function GetSensorCategory: TSensorCategory; override;
    function GetState: TSensorState; override;
    function GetTimeStamp: TDateTime; override;
    procedure DoOptimize; override;
    function GetAuthorized: TAuthorizationType; override;
    function GetAccuracy: TLocationAccuracy; override;
    function GetDistance: TLocationDistance; override;
    function GetPowerConsumption: TPowerConsumption; override;
    procedure SetAccuracy(const Value: TLocationAccuracy); override;
    procedure SetDistance(const Value: TLocationDistance); override;
    procedure DoLocationChangeType; override;
  public
    constructor Create(AManager: TSensorManager); override;
    function Supported: Boolean;
  end;

  TNativeSensor = class
  strict private
    FSensorType: Integer;
    FSensorManager: PASensorManager;
    FNativeSensor: PASensor;
    FNativeEventQueue: PASensorEventQueue;
    FLastSensorEvent: ASensorEvent;
    FUpdateInterval: Double;
    function GetInterval: Double;
    procedure SetInterval(const Value: Double);
    class function NativeCallBack(FileDescriptor, Events: Integer;
      Data: Pointer): Integer; cdecl; static;
  public
    constructor Create(SensorType: Integer);
    function Supported: Boolean;
    function LastValue: ASensorEvent;
    property UpdateInterval: Double read GetInterval write SetInterval;
    function DoStart: Boolean;
    procedure DoStop;
    function TimeStamp: Double;
  end;
  {
    TAndroidNativeSensor = class(TCustomSensor)
    strict private
    FNativeSensor: TNativeSensor;
    protected
    function GetSensorCategory: TSensorCategory; override;
    function GetState: TSensorState; override;
    function GetTimeStamp: TDateTime; override;
    function GetDoubleProperty(Prop: TProperty): Double; override;
    public
    constructor Create(AManager: TSensorManager); override;
    function Supported: Boolean;
    end;
  }

  TAndroidNativeGravitySensor = class(TCustomMotionSensor)
  strict private
    FNativeSensor: TNativeSensor;
  protected
    function GetMotionSensorType: TMotionSensorType; override;
    function GetAvailableProperties: TCustomMotionSensor.TProperties; override;
    function DoStart: Boolean; override;
    procedure DoStop; override;
    function GetSensorCategory: TSensorCategory; override;
    function GetState: TSensorState; override;
    function GetTimeStamp: TDateTime; override;
    function GetDoubleProperty(Prop: TCustomMotionSensor.TProperty)
      : Double; override;
    function GetUpdateInterval: Double; override;
    procedure SetUpdateInterval(AInterval: Double); override;
  public
    constructor Create(AManager: TSensorManager); override;
    function Supported: Boolean;
  end;

  TAndroidNativeLinearAccelerometrSensor = class(TCustomMotionSensor)
  strict private
    FNativeSensor: TNativeSensor;
  protected
    function GetMotionSensorType: TMotionSensorType; override;
    function GetAvailableProperties: TCustomMotionSensor.TProperties; override;
    function DoStart: Boolean; override;
    procedure DoStop; override;
    function GetSensorCategory: TSensorCategory; override;
    function GetState: TSensorState; override;
    function GetTimeStamp: TDateTime; override;
    function GetDoubleProperty(Prop: TCustomMotionSensor.TProperty)
      : Double; override;
    function GetUpdateInterval: Double; override;
    procedure SetUpdateInterval(AInterval: Double); override;
  public
    constructor Create(AManager: TSensorManager); override;
    function Supported: Boolean;
  end;

  TAndroidNativeHumiditySensor = class(TCustomEnvironmentalSensor)
  strict private
    FNativeSensor: TNativeSensor;
  protected
    function GetSensorCategory: TSensorCategory; override;
    function GetState: TSensorState; override;
    function GetTimeStamp: TDateTime; override;
    function GetDoubleProperty(Prop: TCustomEnvironmentalSensor.TProperty)
      : Double; override;
    function GetEnvironmentalSensorType: TEnvironmentalSensorType; override;
    function GetAvailableProperties
      : TCustomEnvironmentalSensor.TProperties; override;
    function DoStart: Boolean; override;
    procedure DoStop; override;
  public
    constructor Create(AManager: TSensorManager); override;
    function Supported: Boolean;
  end;

  TAndroidNativeTemperatureSensor = class(TCustomEnvironmentalSensor)
  strict private
    FNativeSensor: TNativeSensor;
  protected
    function GetSensorCategory: TSensorCategory; override;
    function GetState: TSensorState; override;
    function GetTimeStamp: TDateTime; override;
    function GetDoubleProperty(Prop: TCustomEnvironmentalSensor.TProperty)
      : Double; override;
    function GetEnvironmentalSensorType: TEnvironmentalSensorType; override;
    function GetAvailableProperties
      : TCustomEnvironmentalSensor.TProperties; override;
    function DoStart: Boolean; override;
    procedure DoStop; override;
  public
    constructor Create(AManager: TSensorManager); override;
    function Supported: Boolean;
  end;

  TAndroidNativeProximitySensor = class(TCustomBiometricSensor)
  strict private
    FNativeSensor: TNativeSensor;
  protected
    function GetBiometricSensorType: TBiometricSensorType; override;
    function GetSensorCategory: TSensorCategory; override;
    function GetState: TSensorState; override;
    function GetTimeStamp: TDateTime; override;
    function GetAvailableProperties
      : TCustomBiometricSensor.TProperties; override;
    function GetDoubleProperty(Prop: TCustomBiometricSensor.TProperty)
      : Double; override;
    function DoStart: Boolean; override;
    procedure DoStop; override;
  public
    constructor Create(AManager: TSensorManager); override;
    function Supported: Boolean;
  end;

  TAndroidNativeMagneticSensor = class(TCustomOrientationSensor)
  strict private
    FNativeSensor: TNativeSensor;
  protected
    function GetUpdateInterval: Double; override;
    procedure SetUpdateInterval(AInterval: Double); override;
    function GetOrientationSensorType: TOrientationSensorType; override;
    function GetSensorCategory: TSensorCategory; override;
    function GetState: TSensorState; override;
    function GetTimeStamp: TDateTime; override;
    function GetAvailableProperties
      : TCustomOrientationSensor.TProperties; override;
    function GetDoubleProperty(Prop: TCustomOrientationSensor.TProperty)
      : Double; override;
    function DoStart: Boolean; override;
    procedure DoStop; override;
  public
    constructor Create(AManager: TSensorManager); override;
    function Supported: Boolean;
  end;

  TAndroidNativePressureSensor = class(TCustomEnvironmentalSensor)
  strict private
    FNativeSensor: TNativeSensor;
  protected
    function GetSensorCategory: TSensorCategory; override;
    function GetState: TSensorState; override;
    function GetTimeStamp: TDateTime; override;
    function GetEnvironmentalSensorType: TEnvironmentalSensorType; override;
    function GetAvailableProperties
      : TCustomEnvironmentalSensor.TProperties; override;
    function GetDoubleProperty(Prop: TCustomEnvironmentalSensor.TProperty)
      : Double; override;
    function DoStart: Boolean; override;
    procedure DoStop; override;
  public
    constructor Create(AManager: TSensorManager); override;
    function Supported: Boolean;
  end;

  TAndroidNativeLightSensor = class(TCustomLightSensor)
  strict private
    FNativeSensor: TNativeSensor;
  protected
    function GetLightSensorType: TLightSensorType; override;
    function GetAvailableProperties: TCustomLightSensor.TProperties; override;
    function GetDoubleProperty(Prop: TCustomLightSensor.TProperty)
      : Double; override;
    function GetSensorCategory: TSensorCategory; override;
    function GetState: TSensorState; override;
    function GetTimeStamp: TDateTime; override;
    function DoStart: Boolean; override;
    procedure DoStop; override;
  public
    constructor Create(AManager: TSensorManager); override;
    function Supported: Boolean;
  end;

  TAndroidNativeAccelerometrSensor = class(TCustomMotionSensor)
  strict private
    FNativeSensor: TNativeSensor;
  protected
    function GetMotionSensorType: TMotionSensorType; override;
    function GetAvailableProperties: TCustomMotionSensor.TProperties; override;
    function DoStart: Boolean; override;
    procedure DoStop; override;
    function GetSensorCategory: TSensorCategory; override;
    function GetState: TSensorState; override;
    function GetTimeStamp: TDateTime; override;
    function GetDoubleProperty(Prop: TCustomMotionSensor.TProperty)
      : Double; override;
    function GetUpdateInterval: Double; override;
    procedure SetUpdateInterval(AInterval: Double); override;
  public
    constructor Create(AManager: TSensorManager); override;
    function Supported: Boolean;
  end;

  TAndroidNativeRotationSensor = class(TCustomOrientationSensor)
  private
    FNativeSensor: TNativeSensor;
  protected
    constructor Create(AManager: TSensorManager); override;
  public
    function Supported: Boolean;
    function GetOrientationSensorType: TOrientationSensorType; override;
    function GetAvailableProperties
      : TCustomOrientationSensor.TProperties; override;
    function GetDoubleProperty(Prop: TCustomOrientationSensor.TProperty)
      : Double; override;
    function GetSensorCategory: TSensorCategory; override;
    function GetUpdateInterval: Double; override;
    procedure SetUpdateInterval(AInterval: Double); override;
    function DoStart: Boolean; override;
    procedure DoStop; override;
    function GetState: TSensorState; override;
    function GetTimeStamp: TDateTime; override;
  end;

  TAndroidNativeGyroscopeSensor = class(TCustomMotionSensor)
  private
    FNativeSensor: TNativeSensor;
  protected
    constructor Create(AManager: TSensorManager); override;
  public
    function Supported: Boolean;
    function GetMotionSensorType: TMotionSensorType; override;
    function GetAvailableProperties: TCustomMotionSensor.TProperties; override;
    function GetDoubleProperty(Prop: TCustomMotionSensor.TProperty)
      : Double; override;
    function GetSensorCategory: TSensorCategory; override;
    function GetUpdateInterval: Double; override;
    procedure SetUpdateInterval(AInterval: Double); override;
    function DoStart: Boolean; override;
    procedure DoStop; override;
    function GetState: TSensorState; override;
    function GetTimeStamp: TDateTime; override;
  end;

type
  TAndroidSensorManager = class(TPlatformSensorManager)
  private
    FActive: Boolean;
    FSensorManager: PASensorManager;
  protected
    function GetCanActivate: Boolean; override;
    function GetActive: Boolean; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Activate; override;
    procedure Deactivate; override;
  end;

  { TAndroidSensorManager }

procedure TAndroidSensorManager.Activate;
var
  Accelerator: TAndroidNativeAccelerometrSensor;
  Orientation: TAndroidNativeGyroscopeSensor;
  Light: TAndroidNativeLightSensor;
  Pressure: TAndroidNativePressureSensor;
  MagneticField: TAndroidNativeMagneticSensor;
  Proximity: TAndroidNativeProximitySensor;
  Rotation: TAndroidNativeRotationSensor;
  Temperature: TAndroidNativeTemperatureSensor;
  Humidity: TAndroidNativeHumiditySensor;
  Gravity: TAndroidNativeGravitySensor;
  LinearAcceleration: TAndroidNativeLinearAccelerometrSensor;
  Location: TUIAndroidLocationSensor;
begin
  if not Active then
  begin
    FActive := True;

    Accelerator := TAndroidNativeAccelerometrSensor.Create(Self);
    if not Accelerator.Supported then
      RemoveSensor(Accelerator);

    Orientation := TAndroidNativeGyroscopeSensor.Create(Self);
    if not Orientation.Supported then
      RemoveSensor(Orientation);

    Light := TAndroidNativeLightSensor.Create(Self);
    if not Light.Supported then
      RemoveSensor(Light);

    Pressure := TAndroidNativePressureSensor.Create(Self);
    if not Pressure.Supported then
      RemoveSensor(Pressure);

    MagneticField := TAndroidNativeMagneticSensor.Create(Self);
    if not MagneticField.Supported then
      RemoveSensor(MagneticField);

    Proximity := TAndroidNativeProximitySensor.Create(Self);
    if not Proximity.Supported then
      RemoveSensor(Proximity);

    Rotation := TAndroidNativeRotationSensor.Create(Self);
    if not Rotation.Supported then
      RemoveSensor(Rotation);

    Temperature := TAndroidNativeTemperatureSensor.Create(Self);
    if not Temperature.Supported then
      RemoveSensor(Temperature);

    Humidity := TAndroidNativeHumiditySensor.Create(Self);
    if not Humidity.Supported then
      RemoveSensor(Humidity);

    Gravity := TAndroidNativeGravitySensor.Create(Self);
    if not Gravity.Supported then
      RemoveSensor(Gravity);

    LinearAcceleration := TAndroidNativeLinearAccelerometrSensor.Create(Self);
    if not LinearAcceleration.Supported then
      RemoveSensor(LinearAcceleration);

    Location := TUIAndroidLocationSensor.Create(Self);
    if not Location.Supported then
      RemoveSensor(Location);
  end;
end;

constructor TAndroidSensorManager.Create;
begin
  inherited;
  FSensorManager := ASensorManager_getInstance;
  FActive := False;
end;

procedure TAndroidSensorManager.Deactivate;
var
  I: Integer;
begin
  FActive := False;
  for I := Count - 1 downto 0 do
    RemoveSensor(Sensors[I]);
end;

destructor TAndroidSensorManager.Destroy;
begin
  inherited;
end;

function TAndroidSensorManager.GetActive: Boolean;
begin
  Result := FActive;
end;

function TAndroidSensorManager.GetCanActivate: Boolean;
begin
  Result := Assigned(FSensorManager);
end;

{ TAndroidCustomSensor }

constructor TAndroidNativeAccelerometrSensor.Create(AManager: TSensorManager);
begin
  inherited;
  FNativeSensor := TNativeSensor.Create(ASENSOR_TYPE_ACCELEROMETER);
end;

function TAndroidNativeAccelerometrSensor.DoStart: Boolean;
begin
  Result := FNativeSensor.DoStart;
end;

procedure TAndroidNativeAccelerometrSensor.DoStop;
begin
  inherited;
  FNativeSensor.DoStop;
end;

function TAndroidNativeAccelerometrSensor.GetAvailableProperties
  : TCustomMotionSensor.TProperties;
begin
  Result := [TCustomMotionSensor.TProperty.AccelerationX,
    TCustomMotionSensor.TProperty.AccelerationY,
    TCustomMotionSensor.TProperty.AccelerationZ]
end;

function TAndroidNativeAccelerometrSensor.GetDoubleProperty
  (Prop: TCustomMotionSensor.TProperty): Double;
begin
  case Prop of
    TCustomMotionSensor.TProperty.AccelerationX:
      Result := -1 * FNativeSensor.LastValue.acceleration.x /
        ASENSOR_STANDARD_GRAVITY;
    TCustomMotionSensor.TProperty.AccelerationY:
      Result := -1 * FNativeSensor.LastValue.acceleration.y /
        ASENSOR_STANDARD_GRAVITY;
    TCustomMotionSensor.TProperty.AccelerationZ:
      Result := -1 * FNativeSensor.LastValue.acceleration.z /
        ASENSOR_STANDARD_GRAVITY;
  else
    Result := NaN;
  end;
end;

function TAndroidNativeAccelerometrSensor.GetMotionSensorType
  : TMotionSensorType;
begin
  Result := TMotionSensorType.Accelerometer3D;
end;

function TAndroidNativeAccelerometrSensor.GetSensorCategory: TSensorCategory;
begin
  Result := TSensorCategory.Motion;
end;

function TAndroidNativeAccelerometrSensor.GetState: TSensorState;
begin
  if Supported then
    Result := TSensorState.Ready
  else
    Result := TSensorState.NoData;
end;

function TAndroidNativeAccelerometrSensor.GetTimeStamp: TDateTime;
begin
  Result := FNativeSensor.TimeStamp;
end;

function TAndroidNativeAccelerometrSensor.GetUpdateInterval: Double;
begin
  Result := FNativeSensor.UpdateInterval;
end;

procedure TAndroidNativeAccelerometrSensor.SetUpdateInterval(AInterval: Double);
begin
  if Supported then
    FNativeSensor.UpdateInterval := AInterval;
end;

function TAndroidNativeAccelerometrSensor.Supported: Boolean;
begin
  Result := FNativeSensor.Supported;
end;

{ TPlatformSensorManager }

class function TPlatformSensorManager.GetSensorManager: TSensorManager;
begin
  Result := TAndroidSensorManager.Create;
end;

{ TPlatformGeocoder }

class function TPlatformGeocoder.GetGeocoderImplementer: TGeocoderClass;
begin
  Result := TAndroidGeocoder;
end;

{ TPlatformGpsStatus }

class function TPlatformGpsStatus.GetGpsStatusImplementer: TGpsStatusClass;
begin

  Result := nil;
end;

constructor TAndroidNativeGyroscopeSensor.Create(AManager: TSensorManager);
begin
  inherited;
  FNativeSensor := TNativeSensor.Create(ASENSOR_TYPE_GYROSCOPE);
end;

function TAndroidNativeGyroscopeSensor.DoStart: Boolean;
begin
  Result := FNativeSensor.DoStart;
end;

procedure TAndroidNativeGyroscopeSensor.DoStop;
begin
  inherited;
  FNativeSensor.DoStop;
end;

function TAndroidNativeGyroscopeSensor.GetAvailableProperties
  : TCustomMotionSensor.TProperties;
begin
  Result := [TCustomMotionSensor.TProperty.AngleAccelX,
    TCustomMotionSensor.TProperty.AngleAccelY,
    TCustomMotionSensor.TProperty.AngleAccelZ];
end;

function TAndroidNativeGyroscopeSensor.GetDoubleProperty
  (Prop: TCustomMotionSensor.TProperty): Double;
begin
  case Prop of
    TCustomMotionSensor.TProperty.AngleAccelX:
      Result := FNativeSensor.LastValue.vector.x;
    TCustomMotionSensor.TProperty.AngleAccelY:
      Result := FNativeSensor.LastValue.vector.y;
    TCustomMotionSensor.TProperty.AngleAccelZ:
      Result := FNativeSensor.LastValue.vector.z;
  else
    Result := NaN;
  end;
end;

function TAndroidNativeGyroscopeSensor.GetMotionSensorType: TMotionSensorType;
begin
  Result := TMotionSensorType.Gyrometer3D;
end;

function TAndroidNativeGyroscopeSensor.GetSensorCategory: TSensorCategory;
begin
  Result := TSensorCategory.Motion;
end;

function TAndroidNativeGyroscopeSensor.GetState: TSensorState;
begin
  if FNativeSensor.Supported then
    Result := TSensorState.Ready
  else
    Result := TSensorState.NoData;
end;

function TAndroidNativeGyroscopeSensor.GetTimeStamp: TDateTime;
begin
  Result := FNativeSensor.TimeStamp;
end;

function TAndroidNativeGyroscopeSensor.GetUpdateInterval: Double;
begin
  Result := FNativeSensor.UpdateInterval;
end;

procedure TAndroidNativeGyroscopeSensor.SetUpdateInterval(AInterval: Double);
begin
  inherited;
  FNativeSensor.UpdateInterval := AInterval;
end;

function TAndroidNativeGyroscopeSensor.Supported: Boolean;
begin
  Result := FNativeSensor.Supported;
end;

{ TNativeSensor }

constructor TNativeSensor.Create(SensorType: Integer);
begin
  FSensorType := SensorType;
  FSensorManager := ASensorManager_getInstance;
  FNativeSensor := ASensorManager_getDefaultSensor(FSensorManager, SensorType);
  FNativeEventQueue := ASensorManager_createEventQueue(FSensorManager,
    ALooper_forThread, LOOPER_ID_USER, nil, nil);
  SetInterval(1000);
end;

function TNativeSensor.DoStart: Boolean;
begin
  Result := True;
  if Supported then
    ASensorEventQueue_enableSensor(FNativeEventQueue, FNativeSensor);
end;

procedure TNativeSensor.DoStop;
begin
  ASensorEventQueue_disableSensor(FNativeEventQueue, FNativeSensor);
end;

function TNativeSensor.GetInterval: Double;
begin
  Result := FUpdateInterval;
end;

function TNativeSensor.LastValue: ASensorEvent;
var
  SensorEvent: ASensorEvent;
begin
  while ASensorEventQueue_getEvents(FNativeEventQueue, @SensorEvent, 1) > 0 do
    FLastSensorEvent := SensorEvent;
  Result := FLastSensorEvent;
end;

class function TNativeSensor.NativeCallBack(FileDescriptor, Events: Integer;
  Data: Pointer): Integer;
const
  UnregisteredFromTheLooper = 0;
  ContinueReceivingCallbacks = 1;
begin
  Result := ContinueReceivingCallbacks;
end;

procedure TNativeSensor.SetInterval(const Value: Double);
begin
  if Supported then
  begin
    FUpdateInterval := Value;
    ASensorEventQueue_setEventRate(FNativeEventQueue, FNativeSensor,
      Round(FUpdateInterval));
  end;
end;

function TNativeSensor.Supported: Boolean;
begin
  Result := Assigned(FNativeSensor) and Assigned(FNativeEventQueue);
end;

function TNativeSensor.TimeStamp: Double;
const
  TimeScale = 1000000;
begin
  Result := NaN;
  if Supported then
    Result := IncMilliSecond(UnixDateDelta, LastValue.TimeStamp div TimeScale);
end;

{ TAndroidNativeLightSensor }

constructor TAndroidNativeLightSensor.Create(AManager: TSensorManager);
begin
  inherited;
  FNativeSensor := TNativeSensor.Create(ASENSOR_TYPE_LIGHT);
end;

function TAndroidNativeLightSensor.DoStart: Boolean;
begin
  Result := FNativeSensor.DoStart;
end;

procedure TAndroidNativeLightSensor.DoStop;
begin
  inherited;
  FNativeSensor.DoStop;
end;

function TAndroidNativeLightSensor.GetAvailableProperties
  : TCustomLightSensor.TProperties;
begin
  Result := [TCustomLightSensor.TProperty.Lux];
end;

function TAndroidNativeLightSensor.GetDoubleProperty
  (Prop: TCustomLightSensor.TProperty): Double;
begin
  case Prop of
    TCustomLightSensor.TProperty.Lux:
      Result := FNativeSensor.LastValue.Light;
  else
    Result := NaN;
  end;
end;

function TAndroidNativeLightSensor.GetLightSensorType: TLightSensorType;
begin
  Result := TLightSensorType.AmbientLight;
end;

function TAndroidNativeLightSensor.GetSensorCategory: TSensorCategory;
begin
  Result := TSensorCategory.Light;
end;

function TAndroidNativeLightSensor.GetState: TSensorState;
begin
  if FNativeSensor.Supported then
    Result := TSensorState.Ready
  else
    Result := TSensorState.NoData;
end;

function TAndroidNativeLightSensor.GetTimeStamp: TDateTime;
begin
  Result := FNativeSensor.TimeStamp;
end;

function TAndroidNativeLightSensor.Supported: Boolean;
begin
  Result := FNativeSensor.Supported;
end;

{ TAndroidNativePressureSensor }

constructor TAndroidNativePressureSensor.Create(AManager: TSensorManager);
begin
  inherited;
  FNativeSensor := TNativeSensor.Create(ASENSOR_TYPE_PRESSURE);
end;

function TAndroidNativePressureSensor.DoStart: Boolean;
begin
  Result := FNativeSensor.DoStart;
end;

procedure TAndroidNativePressureSensor.DoStop;
begin
  inherited;
  FNativeSensor.DoStop;
end;

function TAndroidNativePressureSensor.GetAvailableProperties
  : TCustomEnvironmentalSensor.TProperties;
begin
  Result := [TCustomEnvironmentalSensor.TProperty.Pressure];
end;

function TAndroidNativePressureSensor.GetDoubleProperty
  (Prop: TCustomEnvironmentalSensor.TProperty): Double;
begin
  case Prop of
    // Atmospheric pressure in hPa (millibar)
    TCustomEnvironmentalSensor.TProperty.Pressure:
      Result := FNativeSensor.LastValue.Pressure;
  else
    Result := NaN;
  end;
end;

function TAndroidNativePressureSensor.GetEnvironmentalSensorType
  : TEnvironmentalSensorType;
begin
  Result := TEnvironmentalSensorType.AtmosphericPressure;
end;

function TAndroidNativePressureSensor.GetSensorCategory: TSensorCategory;
begin
  Result := TSensorCategory.Environmental;
end;

function TAndroidNativePressureSensor.GetState: TSensorState;
begin
  if Supported then
    Result := TSensorState.Ready
  else
    Result := TSensorState.NoData;
end;

function TAndroidNativePressureSensor.GetTimeStamp: TDateTime;
begin
  Result := FNativeSensor.TimeStamp;
end;

function TAndroidNativePressureSensor.Supported: Boolean;
begin
  Result := FNativeSensor.Supported;
end;

{ TAndroidNativeMagneticSensor }

constructor TAndroidNativeMagneticSensor.Create(AManager: TSensorManager);
begin
  inherited;
  FNativeSensor := TNativeSensor.Create(ASENSOR_TYPE_MAGNETIC_FIELD);
end;

function TAndroidNativeMagneticSensor.DoStart: Boolean;
begin
  Result := FNativeSensor.DoStart;
end;

procedure TAndroidNativeMagneticSensor.DoStop;
begin
  inherited;
  FNativeSensor.DoStop;
end;

function TAndroidNativeMagneticSensor.GetAvailableProperties
  : TCustomOrientationSensor.TProperties;
begin
  Result := [TCustomOrientationSensor.TProperty.HeadingX,
    TCustomOrientationSensor.TProperty.HeadingY,
    TCustomOrientationSensor.TProperty.HeadingZ];
end;

function TAndroidNativeMagneticSensor.GetDoubleProperty
  (Prop: TCustomOrientationSensor.TProperty): Double;
begin
  case Prop of
    // All values are in micro-Tesla (uT) and measure the ambient magnetic field in the X, Y and Z axis.
    TCustomOrientationSensor.TProperty.HeadingX:
      Result := FNativeSensor.LastValue.magnetic.x;
    TCustomOrientationSensor.TProperty.HeadingY:
      Result := FNativeSensor.LastValue.magnetic.y;
    TCustomOrientationSensor.TProperty.HeadingZ:
      Result := FNativeSensor.LastValue.magnetic.z;
  else
    Result := NaN;
  end;
end;

function TAndroidNativeMagneticSensor.GetOrientationSensorType
  : TOrientationSensorType;
begin
  Result := TOrientationSensorType.Compass3D;
end;

function TAndroidNativeMagneticSensor.GetSensorCategory: TSensorCategory;
begin
  Result := TSensorCategory.Orientation;
end;

function TAndroidNativeMagneticSensor.GetState: TSensorState;
begin
  if Supported then
    Result := TSensorState.Ready
  else
    Result := TSensorState.NoData;
end;

function TAndroidNativeMagneticSensor.GetTimeStamp: TDateTime;
begin
  Result := FNativeSensor.TimeStamp;
end;

function TAndroidNativeMagneticSensor.GetUpdateInterval: Double;
begin
  Result := FNativeSensor.UpdateInterval;
end;

procedure TAndroidNativeMagneticSensor.SetUpdateInterval(AInterval: Double);
begin
  inherited;
  FNativeSensor.UpdateInterval := AInterval;
end;

function TAndroidNativeMagneticSensor.Supported: Boolean;
begin
  Result := FNativeSensor.Supported;
end;

{ TAndroidNativeProximitySensor }

constructor TAndroidNativeProximitySensor.Create(AManager: TSensorManager);
begin
  inherited;
  FNativeSensor := TNativeSensor.Create(ASENSOR_TYPE_PROXIMITY);
end;

function TAndroidNativeProximitySensor.DoStart: Boolean;
begin
  Result := FNativeSensor.DoStart;
end;

procedure TAndroidNativeProximitySensor.DoStop;
begin
  inherited;
  FNativeSensor.DoStop;
end;

function TAndroidNativeProximitySensor.GetAvailableProperties
  : TCustomBiometricSensor.TProperties;
begin
  Result := [TCustomBiometricSensor.TProperty.HumanProximity];
end;

function TAndroidNativeProximitySensor.GetBiometricSensorType
  : TBiometricSensorType;
begin
  Result := TBiometricSensorType.HumanProximity;
end;

function TAndroidNativeProximitySensor.GetDoubleProperty
  (Prop: TCustomBiometricSensor.TProperty): Double;
begin
  case Prop of
    // Proximity sensor distance measured in centimeters
    TCustomBiometricSensor.TProperty.HumanProximity:
      begin
        Result := FNativeSensor.LastValue.Proximity;
      end;
  else
    Result := NaN;
  end;
end;

function TAndroidNativeProximitySensor.GetSensorCategory: TSensorCategory;
begin
  Result := TSensorCategory.Biometric;
end;

function TAndroidNativeProximitySensor.GetState: TSensorState;
begin
  if Supported then
    Result := TSensorState.Ready
  else
    Result := TSensorState.NoData;
end;

function TAndroidNativeProximitySensor.GetTimeStamp: TDateTime;
begin
  Result := FNativeSensor.TimeStamp;
end;

function TAndroidNativeProximitySensor.Supported: Boolean;
begin
  Result := FNativeSensor.Supported;
end;

{ TAndroidNativeRotationSensor }

constructor TAndroidNativeRotationSensor.Create(AManager: TSensorManager);
begin
  inherited;
  FNativeSensor := TNativeSensor.Create(ASENSOR_TYPE_ROTATION_VECTOR);
end;

function TAndroidNativeRotationSensor.DoStart: Boolean;
begin
  Result := FNativeSensor.DoStart;
end;

procedure TAndroidNativeRotationSensor.DoStop;
begin
  inherited;
  FNativeSensor.DoStop;
end;

function TAndroidNativeRotationSensor.GetAvailableProperties
  : TCustomOrientationSensor.TProperties;
begin
  Result := [TCustomOrientationSensor.TProperty.TiltX,
    TCustomOrientationSensor.TProperty.TiltY,
    TCustomOrientationSensor.TProperty.TiltZ];
end;

function TAndroidNativeRotationSensor.GetDoubleProperty
  (Prop: TCustomOrientationSensor.TProperty): Double;
var
  Tilts: ASensorVector;

  function VectorToAngles(const RotationVector: ASensorVector): ASensorVector;
  var
    RM: array [0 .. 8] of Double;
    Len: Double;
    sqX, sqY, sqZ, qXY, qZL, qXZ, qYL, qYZ, qXL: Double;
  begin
    sqX := RotationVector.x * RotationVector.x;
    sqY := RotationVector.y * RotationVector.y;
    sqZ := RotationVector.z * RotationVector.z;
    Len := 1 - sqX - sqY - sqZ;
    if Len > 0 then
      Len := Sqrt(Len)
    else
      Len := 0;
    sqX := 2 * sqX;
    sqY := 2 * sqY;
    sqZ := 2 * sqZ;
    qXY := 2 * RotationVector.x * RotationVector.y;
    qZL := 2 * RotationVector.z * Len;
    qXZ := 2 * RotationVector.x * RotationVector.z;
    qYL := 2 * RotationVector.y * Len;
    qYZ := 2 * RotationVector.y * RotationVector.z;
    qXL := 2 * RotationVector.x * Len;

    RM[0] := 1 - sqY - sqZ;
    RM[1] := qXY - qZL;
    RM[2] := qXZ + qYL;

    RM[3] := qXY + qZL;
    RM[4] := 1 - sqX - sqZ;
    RM[5] := qYZ - qXL;

    RM[6] := qXZ - qYL;
    RM[7] := qYZ + qXL;
    RM[8] := 1 - sqX - sqY;

    Result.azimuth := RadToDeg(ArcTan2(RM[1], RM[4]));
    Result.pitch := RadToDeg(ArcCos(-RM[7]) - Pi / 2);
    Result.roll := RadToDeg(ArcTan2(-RM[6], RM[8]));
  end;

begin
  Tilts := VectorToAngles(FNativeSensor.LastValue.vector);
  case Prop of
    TCustomOrientationSensor.TProperty.TiltX:
      Result := Tilts.roll;
    TCustomOrientationSensor.TProperty.TiltY:
      Result := Tilts.pitch;
    TCustomOrientationSensor.TProperty.TiltZ:
      Result := Tilts.azimuth;
  else
    Result := NaN;
  end;
end;

function TAndroidNativeRotationSensor.GetOrientationSensorType
  : TOrientationSensorType;
begin
  Result := TOrientationSensorType.Inclinometer3D;
end;

function TAndroidNativeRotationSensor.GetSensorCategory: TSensorCategory;
begin
  Result := TSensorCategory.Orientation;
end;

function TAndroidNativeRotationSensor.GetState: TSensorState;
begin
  if FNativeSensor.Supported then
    Result := TSensorState.Ready
  else
    Result := TSensorState.NoData;
end;

function TAndroidNativeRotationSensor.GetTimeStamp: TDateTime;
begin
  Result := FNativeSensor.TimeStamp;
end;

function TAndroidNativeRotationSensor.GetUpdateInterval: Double;
begin
  Result := FNativeSensor.UpdateInterval;
end;

procedure TAndroidNativeRotationSensor.SetUpdateInterval(AInterval: Double);
begin
  inherited;
  FNativeSensor.UpdateInterval := AInterval;
end;

function TAndroidNativeRotationSensor.Supported: Boolean;
begin
  Result := FNativeSensor.Supported;
end;

{ TAndroidNativeTemperatureSensor }

constructor TAndroidNativeTemperatureSensor.Create(AManager: TSensorManager);
begin
  inherited;
  FNativeSensor := TNativeSensor.Create(ASENSOR_TYPE_AMBIENT_TEMPERATURE);
end;

function TAndroidNativeTemperatureSensor.DoStart: Boolean;
begin
  Result := FNativeSensor.DoStart;
end;

procedure TAndroidNativeTemperatureSensor.DoStop;
begin
  inherited;
  FNativeSensor.DoStop;
end;

function TAndroidNativeTemperatureSensor.GetAvailableProperties
  : TCustomEnvironmentalSensor.TProperties;
begin
  Result := [TCustomEnvironmentalSensor.TProperty.Temperature];
end;

function TAndroidNativeTemperatureSensor.GetDoubleProperty
  (Prop: TCustomEnvironmentalSensor.TProperty): Double;
begin
  case Prop of
    // ambient (room) temperature in degree Celsius
    TCustomEnvironmentalSensor.TProperty.Temperature:
      Result := FNativeSensor.LastValue.Temperature;
  else
    Result := NaN;
  end;
end;

function TAndroidNativeTemperatureSensor.GetEnvironmentalSensorType
  : TEnvironmentalSensorType;
begin
  Result := TEnvironmentalSensorType.Temperature;
end;

function TAndroidNativeTemperatureSensor.GetSensorCategory: TSensorCategory;
begin
  Result := TSensorCategory.Environmental;
end;

function TAndroidNativeTemperatureSensor.GetState: TSensorState;
begin
  if Supported then
    Result := TSensorState.Ready
  else
    Result := TSensorState.NoData;
end;

function TAndroidNativeTemperatureSensor.GetTimeStamp: TDateTime;
begin
  Result := FNativeSensor.TimeStamp;
end;

function TAndroidNativeTemperatureSensor.Supported: Boolean;
begin
  Result := FNativeSensor.Supported;
end;

{ TAndroidNativeSensor }

constructor TAndroidNativeHumiditySensor.Create(AManager: TSensorManager);
begin
  inherited;
  FNativeSensor := TNativeSensor.Create(ASENSOR_TYPE_RELATIVE_HUMIDITY);
end;

function TAndroidNativeHumiditySensor.DoStart: Boolean;
begin
  Result := FNativeSensor.DoStart;
end;

procedure TAndroidNativeHumiditySensor.DoStop;
begin
  inherited;
  FNativeSensor.DoStop;
end;

function TAndroidNativeHumiditySensor.GetAvailableProperties
  : TCustomEnvironmentalSensor.TProperties;
begin
  Result := [TCustomEnvironmentalSensor.TProperty.Humidity];
end;

function TAndroidNativeHumiditySensor.GetDoubleProperty
  (Prop: TCustomEnvironmentalSensor.TProperty): Double;
begin
  case Prop of
    // Relative ambient air humidity in percent
    TCustomEnvironmentalSensor.TProperty.Humidity:
      Result := FNativeSensor.LastValue.vector.v[0];
  else
    Result := NaN;
  end;
end;

function TAndroidNativeHumiditySensor.GetEnvironmentalSensorType
  : TEnvironmentalSensorType;
begin
  Result := TEnvironmentalSensorType.Humidity;
end;

function TAndroidNativeHumiditySensor.GetSensorCategory: TSensorCategory;
begin
  Result := TSensorCategory.Environmental;
end;

function TAndroidNativeHumiditySensor.GetState: TSensorState;
begin
  if Supported then
    Result := TSensorState.Ready
  else
    Result := TSensorState.NoData;
end;

function TAndroidNativeHumiditySensor.GetTimeStamp: TDateTime;
begin
  Result := FNativeSensor.TimeStamp;
end;

function TAndroidNativeHumiditySensor.Supported: Boolean;
begin
  Result := FNativeSensor.Supported;
end;

{ TAndroidNativeGravitySensor }

constructor TAndroidNativeGravitySensor.Create(AManager: TSensorManager);
begin
  inherited;
  FNativeSensor := TNativeSensor.Create(ASENSOR_TYPE_GRAVITY);
end;

function TAndroidNativeGravitySensor.DoStart: Boolean;
begin
  Result := FNativeSensor.DoStart;
end;

procedure TAndroidNativeGravitySensor.DoStop;
begin
  inherited;
  FNativeSensor.DoStop;
end;

function TAndroidNativeGravitySensor.GetAvailableProperties
  : TCustomMotionSensor.TProperties;
begin
  Result := [TCustomMotionSensor.TProperty.AccelerationX,
    TCustomMotionSensor.TProperty.AccelerationY,
    TCustomMotionSensor.TProperty.AccelerationZ]
end;

function TAndroidNativeGravitySensor.GetDoubleProperty
  (Prop: TCustomMotionSensor.TProperty): Double;
begin
  case Prop of
    TCustomMotionSensor.TProperty.AccelerationX:
      Result := -1 * FNativeSensor.LastValue.acceleration.x /
        ASENSOR_STANDARD_GRAVITY;
    TCustomMotionSensor.TProperty.AccelerationY:
      Result := -1 * FNativeSensor.LastValue.acceleration.y /
        ASENSOR_STANDARD_GRAVITY;
    TCustomMotionSensor.TProperty.AccelerationZ:
      Result := -1 * FNativeSensor.LastValue.acceleration.z /
        ASENSOR_STANDARD_GRAVITY;
  else
    Result := NaN;
  end;
end;

function TAndroidNativeGravitySensor.GetMotionSensorType: TMotionSensorType;
begin
  Result := TMotionSensorType.GravityAccelerometer3D;
end;

function TAndroidNativeGravitySensor.GetSensorCategory: TSensorCategory;
begin
  Result := TSensorCategory.Motion;
end;

function TAndroidNativeGravitySensor.GetState: TSensorState;
begin
  if Supported then
    Result := TSensorState.Ready
  else
    Result := TSensorState.NoData;
end;

function TAndroidNativeGravitySensor.GetTimeStamp: TDateTime;
begin
  Result := FNativeSensor.TimeStamp;
end;

function TAndroidNativeGravitySensor.GetUpdateInterval: Double;
begin
  Result := FNativeSensor.UpdateInterval;
end;

procedure TAndroidNativeGravitySensor.SetUpdateInterval(AInterval: Double);
begin
  inherited;
  FNativeSensor.UpdateInterval := AInterval;
end;

function TAndroidNativeGravitySensor.Supported: Boolean;
begin
  Result := FNativeSensor.Supported;
end;

{ TAndroidNativeLinearAccelerometrSensor }

constructor TAndroidNativeLinearAccelerometrSensor.Create
  (AManager: TSensorManager);
begin
  inherited;
  FNativeSensor := TNativeSensor.Create(ASENSOR_TYPE_LINEAR_ACCELERATION);
end;

function TAndroidNativeLinearAccelerometrSensor.DoStart: Boolean;
begin
  Result := FNativeSensor.DoStart;
end;

procedure TAndroidNativeLinearAccelerometrSensor.DoStop;
begin
  inherited;
  DoStop;
end;

function TAndroidNativeLinearAccelerometrSensor.GetAvailableProperties
  : TCustomMotionSensor.TProperties;
begin
  Result := [TCustomMotionSensor.TProperty.AccelerationX,
    TCustomMotionSensor.TProperty.AccelerationY,
    TCustomMotionSensor.TProperty.AccelerationZ]
end;

function TAndroidNativeLinearAccelerometrSensor.GetDoubleProperty
  (Prop: TCustomMotionSensor.TProperty): Double;
begin
  case Prop of
    TCustomMotionSensor.TProperty.AccelerationX:
      Result := -1 * FNativeSensor.LastValue.acceleration.x /
        ASENSOR_STANDARD_GRAVITY;
    TCustomMotionSensor.TProperty.AccelerationY:
      Result := -1 * FNativeSensor.LastValue.acceleration.y /
        ASENSOR_STANDARD_GRAVITY;
    TCustomMotionSensor.TProperty.AccelerationZ:
      Result := -1 * FNativeSensor.LastValue.acceleration.z /
        ASENSOR_STANDARD_GRAVITY;
  else
    Result := NaN;
  end;
end;

function TAndroidNativeLinearAccelerometrSensor.GetMotionSensorType
  : TMotionSensorType;
begin
  Result := TMotionSensorType.LinearAccelerometer3D;
end;

function TAndroidNativeLinearAccelerometrSensor.GetSensorCategory
  : TSensorCategory;
begin
  Result := TSensorCategory.Motion;
end;

function TAndroidNativeLinearAccelerometrSensor.GetState: TSensorState;
begin
  if Supported then
    Result := TSensorState.Ready
  else
    Result := TSensorState.NoData;
end;

function TAndroidNativeLinearAccelerometrSensor.GetTimeStamp: TDateTime;
begin
  Result := FNativeSensor.TimeStamp;
end;

function TAndroidNativeLinearAccelerometrSensor.GetUpdateInterval: Double;
begin
  Result := FNativeSensor.UpdateInterval;
end;

procedure TAndroidNativeLinearAccelerometrSensor.SetUpdateInterval
  (AInterval: Double);
begin
  inherited;
  FNativeSensor.UpdateInterval := AInterval;
end;

function TAndroidNativeLinearAccelerometrSensor.Supported: Boolean;
begin
  Result := FNativeSensor.Supported;
end;

{ TUIAndroidLocationSensor.TLocationListener }

constructor TUIAndroidLocationSensor.TLocationListener.Create(ALocationSensor
  : TUIAndroidLocationSensor);
begin
  inherited Create;
  FLocationSensor := ALocationSensor;
end;

procedure TUIAndroidLocationSensor.TLocationListener.onLocationChanged
  (P1: JLocation);
var
  OldLocation, CurrentLocation: TLocationCoord2D;
  Heading: THeading;

begin
  if Assigned(FLocationSensor.FLastValue) then
    OldLocation.Create(FLocationSensor.FLastValue.getLatitude,
      FLocationSensor.FLastValue.getLongitude)
  else
    OldLocation.Create(NaN, NaN);
  CurrentLocation.Create(P1.getLatitude, P1.getLongitude);
  FLocationSensor.FLastValue := P1;
  FLocationSensor.DoLocationChanged(OldLocation, CurrentLocation);
  if P1.hasBearing then
  begin
    Heading.azimuth := P1.getBearing;
    FLocationSensor.DoHeadingChanged(Heading);
  end;
end;

procedure TUIAndroidLocationSensor.TLocationListener.onProviderDisabled
  (P1: JString);
begin;
end;

procedure TUIAndroidLocationSensor.TLocationListener.onProviderEnabled
  (P1: JString);
begin;
end;

procedure TUIAndroidLocationSensor.TLocationListener.onStatusChanged
  (P1: JString; P2: Integer; P3: JBundle);
begin;
end;

{ TUIAndroidLocationSensor.TLocationRunnable }

constructor TUIAndroidLocationSensor.TLocationRunnable.Create(ALocationManager
  : JLocationManager; AListener: TLocationListener; AProvider: JString);
begin
  Inherited Create;
  FLocationManager := ALocationManager;
  FListener := AListener;
  FProvider := AProvider;
end;

procedure TUIAndroidLocationSensor.TLocationRunnable.run;
const
  cMinTime = 100;
  cMinDistance = 10;
begin
  FLocationManager.requestLocationUpdates(FProvider, cMinTime, cMinDistance,
    FListener);
end;

{ TUIAndroidLocationSensor }

constructor TUIAndroidLocationSensor.Create(AManager: TSensorManager);
var
  LocationService: JObject;
begin
  inherited;
  // FActivity := TJNativeActivity.Wrap
  // (PANativeActivity(System.DelphiActivity)^.clazz); // -- patch dd
  FActivity := TJContext.Wrap(System.JavaContext); // -- patch dd
  LocationService := FActivity.getSystemService
    (TJContext.JavaClass.LOCATION_SERVICE);
  if Assigned(LocationService) then
    FLocationManager := TJLocationManager.Wrap((LocationService as ILocalObject)
      .GetObjectID);
end;

procedure TUIAndroidLocationSensor.DoLocationChangeType;
begin
  inherited;
end;

procedure TUIAndroidLocationSensor.DoOptimize;
begin

end;

function TUIAndroidLocationSensor.DoStart: Boolean;

  function RunIfPossible(var ARunnable: TLocationRunnable;
    var AListener: TLocationListener; AProviderName: JString): Boolean;
  var
    Provider: JLocationProvider;
    LHandler: JHandler;
  begin
    Result := False;
    if FLocationManager.isProviderEnabled(AProviderName) then
    begin
      if AListener = nil then
        AListener := TLocationListener.Create(Self);
      Provider := FLocationManager.getProvider(AProviderName);
      if Provider <> nil then
      begin
        ARunnable := TLocationRunnable.Create(FLocationManager, AListener,
          AProviderName);
        // FActivity.runOnUiThread(ARunnable); // --patch dd
        // -- patch dd
        // You can use post method of Handler instead runOnUiThread in this case.
        // more info here: http://developer.android.com/guide/topics/fundamentals/processes-and-threads.html
        LHandler := TJHandler.JavaClass.init;
        LHandler.post(ARunnable);
        Result := True;
      end;
    end;
  end;

  function RunTheBestProvider(var ARunnable: TLocationRunnable;
    var AListener: TLocationListener): Boolean;
  var
    Criteria: JCriteria;
    ProviderName: JString;
  begin
    Result := False;
    Criteria := TJCriteria.JavaClass.init;
    case Round(FAccuracy) of
      0 .. 100:
        Criteria.setHorizontalAccuracy(TJCriteria.JavaClass.ACCURACY_HIGH);
      101 .. 500:
        Criteria.setHorizontalAccuracy(TJCriteria.JavaClass.ACCURACY_MEDIUM);
    else
      Criteria.setHorizontalAccuracy(TJCriteria.JavaClass.ACCURACY_LOW);
    end;

    ProviderName := FLocationManager.getBestProvider(Criteria, True);

    if ProviderName <> nil then
      Result := RunIfPossible(ARunnable, AListener, ProviderName);
  end;

var
  GPSStarted, NetworkStarted, PassiveStarted: Boolean;

begin
  Result := False;
  FPermitted := TPermission.IsPermitted
    (StringToJString('android.permission.ACCESS_FINE_LOCATION'));
  if FPermitted then
  begin
    if FAccuracy > 0 then
      Result := RunTheBestProvider(FPassiveRunner, FPassiveListener)
    else
    begin
      GPSStarted := RunIfPossible(FGPSRunner, FGPSListener,
        TJLocationManager.JavaClass.GPS_PROVIDER);
      NetworkStarted := RunIfPossible(FNetworkRunner, FNetworkListener,
        TJLocationManager.JavaClass.NETWORK_PROVIDER);
      PassiveStarted := RunIfPossible(FPassiveRunner, FPassiveListener,
        TJLocationManager.JavaClass.PASSIVE_PROVIDER);
      Result := GPSStarted or NetworkStarted or PassiveStarted;
    end;
  end;
end;

procedure TUIAndroidLocationSensor.DoStop;
begin
  inherited;
  if FPassiveListener <> nil then
    FLocationManager.removeUpdates(FPassiveListener);
  if FNetworkListener <> nil then
    FLocationManager.removeUpdates(FNetworkListener);
  if FGPSListener <> nil then
    FLocationManager.removeUpdates(FGPSListener);
end;

function TUIAndroidLocationSensor.GetAccuracy: TLocationAccuracy;
begin
  Result := FAccuracy;
end;

function TUIAndroidLocationSensor.GetAuthorized: TAuthorizationType;
begin
  Result := TAuthorizationType.atNotSpecified;
end;

function TUIAndroidLocationSensor.GetAvailableProperties
  : TCustomLocationSensor.TProperties;
begin
  Result := [TCustomLocationSensor.TProperty.Latitude,
    TCustomLocationSensor.TProperty.Longitude,
    TCustomLocationSensor.TProperty.Altitude,
    TCustomLocationSensor.TProperty.Speed,
    TCustomLocationSensor.TProperty.TrueHeading];

end;

function TUIAndroidLocationSensor.GetDistance: TLocationDistance;
begin
  Result := FDistance;
end;

function TUIAndroidLocationSensor.GetDoubleProperty
  (Prop: TCustomLocationSensor.TProperty): Double;
begin
  Result := NaN;
  if Assigned(FLastValue) then
    case Prop of
      TCustomLocationSensor.TProperty.Latitude:
        Result := FLastValue.getLatitude;
      TCustomLocationSensor.TProperty.Longitude:
        Result := FLastValue.getLongitude;
      TCustomLocationSensor.TProperty.Altitude:
        if FLastValue.hasAltitude then
          Result := FLastValue.getAltitude;
      TCustomLocationSensor.TProperty.Speed:
        if FLastValue.hasSpeed then
          Result := FLastValue.getSpeed;
      TCustomLocationSensor.TProperty.TrueHeading:
        if FLastValue.hasBearing then
          Result := FLastValue.getBearing;
    else
      Result := NaN;
    end;
end;

function TUIAndroidLocationSensor.GetLocationSensorType: TLocationSensorType;
begin
  Result := TLocationSensorType.GPS;
end;

function TUIAndroidLocationSensor.GetPowerConsumption: TPowerConsumption;
begin
  Result := TPowerConsumption.pcNotSpecified;
end;

function TUIAndroidLocationSensor.GetSensorCategory: TSensorCategory;
begin
  Result := TSensorCategory.Location;
end;

function TUIAndroidLocationSensor.GetState: TSensorState;
begin
  if Supported then
    Result := TSensorState.Ready
  else
    Result := TSensorState.NoData;
end;

function TUIAndroidLocationSensor.GetStringProperty
  (Prop: TCustomLocationSensor.TProperty): string;
begin
  Result := '';
end;

function TUIAndroidLocationSensor.GetTimeStamp: TDateTime;
begin
  if Assigned(FLastValue) then
    Result := FLastValue.getTime
  else
    Result := 0;
end;

procedure TUIAndroidLocationSensor.SetAccuracy(const Value: TLocationAccuracy);
begin
  inherited;
  FAccuracy := Max(0, Value);
end;

procedure TUIAndroidLocationSensor.SetDistance(const Value: TLocationDistance);
begin
  inherited;
  FDistance := Value;
end;

function TUIAndroidLocationSensor.Supported: Boolean;
begin
  Result := Assigned(FLocationManager);
end;

{ TAndroidGeocoder }

class function TAndroidGeocoder.Authorized: TAuthorizationType;
begin
  Result := TAuthorizationType.atNotSpecified;
end;

class procedure TAndroidGeocoder.Cancel;
begin;
end;

class constructor TAndroidGeocoder.Create;
begin
  // -- patch dd
  // FActivity := TJNativeActivity.Wrap
  // (PANativeActivity(System.DelphiActivity)^.clazz);
  FActivity := TJContextWrapper.Wrap(System.JavaContext);
  FGeocoder := TJGeocoder.JavaClass.init(FActivity);
end;

class destructor TAndroidGeocoder.Destroy;
begin

end;

class procedure TAndroidGeocoder.GeocodeRequest(const AAddress: TCivicAddress);
var
  I: Integer;
  List: JList;
  LAddress: JAddress;
  JO: JObject;
begin
  List := FGeocoder.getFromLocationName(StringToJString(AAddress.ToString), 10);
  SetLength(FGeoFwdCoords, List.size);
  for I := 0 to List.size - 1 do
  begin
    JO := List.get(I);
    LAddress := TJAddress.Wrap((JO as ILocalObject).GetObjectID);
    FGeoFwdCoords[I] := TLocationCoord2D.Create(LAddress.getLatitude,
      LAddress.getLongitude);
  end;
  DoGeocode(FGeoFwdCoords);
end;

class procedure TAndroidGeocoder.GeocodeReverseRequest
  (const Coords: TLocationCoord2D);
var
  List: JList;
  LAddress: JAddress;
  Addr: TCivicAddress;
  FLActivity: JActivity;
  JO: JObject;
  I: Integer;
begin
  FLActivity := TJNativeActivity.Wrap
    (PANativeActivity(System.DelphiActivity)^.clazz);
  List := FGeocoder.getFromLocation(Coords.Latitude, Coords.Longitude, 1);
  if List.size = 0 then
    Addr := nil
  else
  begin
    for I := 0 to List.size - 1 do
    begin
      JO := List.get(I);
      LAddress := TJAddress.Wrap((JO as ILocalObject).GetObjectID);
      Addr := FGeoRevAddress;
      Addr.AdminArea := JStringToString(LAddress.getAdminArea);
      Addr.CountryName := JStringToString(LAddress.getCountryName);
      Addr.CountryCode := JStringToString(LAddress.getCountryCode);
      Addr.Locality := JStringToString(LAddress.getLocality);
      Addr.FeatureName := JStringToString(LAddress.getFeatureName);
      Addr.PostalCode := JStringToString(LAddress.getPostalCode);
      Addr.SubAdminArea := JStringToString(LAddress.getAdminArea);
      Addr.SubLocality := JStringToString(LAddress.getSubLocality);
      Addr.SubThoroughfare := JStringToString(LAddress.getSubThoroughfare);
      Addr.Thoroughfare := JStringToString(LAddress.getThoroughfare);
    end;
  end;
  DoGeocodeReverse(Addr);
end;

class function TAndroidGeocoder.GetGeocoderImplementer: TGeocoderClass;
begin
  Result := Self;
end;

class function TAndroidGeocoder.Supported: Boolean;
begin
  Result := False;
  if Assigned(FGeocoder) then
    Result := TJGeocoder.JavaClass.isPresent;
end;

{ TAndroidGeocoder.TGeocoderRunnable }

constructor TAndroidGeocoder.TGeocoderRunnable.Create(ACoord: TLocationCoord2D;
  AGeocoder: JGeocoder);
begin
  inherited Create;
  FCoord := ACoord;
  FLGeocoder := AGeocoder;
end;

procedure TAndroidGeocoder.TGeocoderRunnable.run;
var
  List: JList;
  Address: JAddress;
  Addr: TCivicAddress;
  Activity: JActivity;
  JO: JObject;
  I: Integer;
begin
  Activity := TJNativeActivity.Wrap
    (PANativeActivity(System.DelphiActivity)^.clazz);
  FLGeocoder := TJGeocoder.JavaClass.init(Activity);
  List := FLGeocoder.getFromLocation(FCoord.Latitude, FCoord.Longitude, 10);
  if List.size = 0 then
    Addr := nil
  else
  begin
    for I := 0 to List.size - 1 do
    begin
      JO := List.get(I);
      Address := TJAddress.Wrap((JO as ILocalObject).GetObjectID);
      Addr := FGeoRevAddress;
      Addr.AdminArea := JStringToString(Address.getAdminArea);
      Addr.CountryName := JStringToString(Address.getCountryName);
      Addr.CountryCode := JStringToString(Address.getCountryCode);
      Addr.Locality := JStringToString(Address.getLocality);
      Addr.FeatureName := JStringToString(Address.getFeatureName);
      Addr.PostalCode := JStringToString(Address.getPostalCode);
      Addr.SubAdminArea := JStringToString(Address.getAdminArea);
      Addr.SubLocality := JStringToString(Address.getSubLocality);
      Addr.SubThoroughfare := JStringToString(Address.getSubThoroughfare);
      Addr.Thoroughfare := JStringToString(Address.getThoroughfare);
    end;
  end;
  DoGeocodeReverse(Addr);
end;

initialization

end.
