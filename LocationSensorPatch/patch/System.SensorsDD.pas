{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{ Copyright(c) 2012-2015 Embarcadero Technologies, Inc. }
{                                                       }
{*******************************************************}

unit System.SensorsDD;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.RTLConsts;

type
  TCivicAddress = class;
  TGeocoder = class;
  TGpsStatus = class;

  TGeocoderClass = class of TGeocoder;
  TGpsStatusClass = class of TGpsStatus;

{$SCOPEDENUMS ON}
  // describes the position type
  TLocationDegrees = Double;
  // describes the distance type
  TLocationDistance = Double;
  // describes the location accuracy
  TLocationAccuracy = Double;
  // describes the battery consumption
  TPowerConsumption = (pcNotSpecified, pcLow, pcMedium, pcHigh);

  // describes the authorization mode for using a specific sensor
  TAuthorizationType = (atNotSpecified, atUnauthorized, atAuthorized);
  // describes the location change types for triggering notifications
  TLocationChangeType = (lctSmall, lctLarge);

  TSensorCategory = (Location, Environmental, Motion, Orientation, Mechanical, Electrical, Biometric, Light, Scanner);
  TSensorCategories = set of TSensorCategory;
  TLocationSensorType = (GPS, Static, Lookup, Triangulation, Broadcast, DeadReckoning, Other);
  TEnvironmentalSensorType = (Temperature, AtmosphericPressure, Humidity, WindSpeed, WindDirection);
  TMotionSensorType = (Accelerometer1D, Accelerometer2D, Accelerometer3D, MotionDetector, Gyrometer1D, Gyrometer2D, Gyrometer3D, Speedometer,
    LinearAccelerometer3D, GravityAccelerometer3D);
  TOrientationSensorType = (Compass1D, Compass2D, Compass3D, Inclinometer1D, Inclinometer2D, Inclinometer3D, Distance1D, Distance2D, Distance3D);
  TElectricalSensorType = (Voltage, Current, Capacitance, Resistance, Inductance, ElectricalPower, Potentiometer);
  TMechanicalSensorType = (BooleanSwitch, BooleanSwitchArray, MultiValueSwitch, Force, Scale, Pressure, Strain);
  TBiometricSensorType = (HumanPresence, HumanProximity, Touch);
  TLightSensorType = (AmbientLight);
  TScannerSensorType = (RFID, Barcode);

  TSensorState = (Added, Removed, Initializing, Ready, NoData, AccessDenied, Error);

  // structure describing geographical 2D coordinates
  TLocationCoord2D = record
    Latitude: TLocationDegrees;
    Longitude: TLocationDegrees;

    constructor Create(ALatitude, ALongitude: TLocationDegrees);
  end;

  // structure describing azimuth
  THeading = record
    Azimuth: Double;
  end;

  // structure describing a geographical region
  TLocationRegion = record
  private
    FID: String;
    FCenter: TLocationCoord2D;
    FRadius: TLocationDistance;
  public
    // creates a new location region with a new generated ID
    constructor Create(const ACenter: TLocationCoord2D; ARadius: TLocationDistance); overload;
    // creates a new location region with a given ID
    constructor Create(const ACenter: TLocationCoord2D; ARadius: TLocationDistance;
      const AID: String); overload;

    // unique ID identifing the region
    property ID: String read FID;
    property Center: TLocationCoord2D read FCenter;
                                                                                                        
// region monitoring will not work
    property Radius: TLocationDistance read FRadius;
    class function Empty: TLocationRegion; inline; static;
  end;

  // stores information about the status of a sattelite
  TGpsSatellite = record
  private
    FElevation: Double;
    FSnr: Double;
    FPrn: Integer;
    FUsedInFix: Boolean;
    FHasAlmanac: Boolean;
    FAzimuth: Double;
    FHasEphemeris: Boolean;
  public
    property Azimuth: Double read FAzimuth;
    property Elevation: Double read FElevation;
    property Prn: Integer read FPrn;
    property Snr: Double read FSnr;

    property HasAlmanac: Boolean read FHasAlmanac;
    property HasEphemeris: Boolean read FHasEphemeris;
    property UsedInFix: Boolean read FUsedInFix;

    constructor Create(Azimuth, Elevation: Double; Prn: Integer; Snr: Double;
      HasAlmanac, HasEphemeris, UsedInFix: Boolean);
  end;

  // describes the event that lets the user convert a whole civic address to a string value
  TConvertAddressEvent = procedure (const Address: TCivicAddress; out Value: String; var Handled: Boolean);

  // retains a civic address
  TCivicAddress = class(TPersistent)
  private
    FCoord: TLocationCoord2D;
    FAddress: String;
    FAdminArea: String;
    FCountryCode: String;
    FCountryName: String;
    FFeatureName: String;
    FLocale: String;
    FLocality: String;
    FPhone: String;
    FPostalCode: String;
    FPremises: String;
    FSubAdminArea: String;
    FSubLocality: String;
    FSubThoroughfare: String;
    FThoroughfare: String;
    FURL: String;
    FOnConvertAddress: TConvertAddressEvent;
  protected
    procedure AssignTo(Dest: TPersistent); override;

    function ConcatAddrStrings(const Strings: array of String): String;
  public
    property Coord: TLocationCoord2D read FCoord write FCoord;
    property Address: String read FAddress write FAddress;
    property AdminArea: String read FAdminArea write FAdminArea;
    property CountryCode: String read FCountryCode write FCountryCode;
    property CountryName: String read FCountryName write FCountryName;
    property FeatureName: String read FFeatureName write FFeatureName;
    property Locale: String read FLocale write FLocale;
    property Locality: String read FLocality write FLocality;
    property Phone: String read FPhone write FPhone;
    property PostalCode: String read FPostalCode write FPostalCode;
    property Premises: String read FPremises write FPremises;
    property SubAdminArea: String read FSubAdminArea write FSubAdminArea;
    property SubLocality: String read FSubLocality write FSubLocality;
    property SubThoroughfare: String read FSubThoroughfare write FSubThoroughfare;
    property Thoroughfare: String read FThoroughfare write FThoroughfare;
    property URL: String read FURL write FURL;

    // converts the civic address details to a single string
    function ToString: String; override;

    // lets the user generate the civic address string instead of using the default converter
    property OnConvertAddress: TConvertAddressEvent read FOnConvertAddress write FOnConvertAddress;
  end;

  // general sensor manager exception
  ESensorManagerException = class(Exception);
  // general sensor exception
  ESensorException = class(Exception);
  // used for location sensor errors
  ELocationSensorException = class(ESensorException);
  // metaclass for sensor exceptions
  ESensorExceptionClass = class of ESensorException;
  // general geocoder exception
  EGeocoderException = class(Exception);
  // general GPS status exception
  EGpsStatusException = class(Exception);

  TSensorManager = class;

  TCustomSensor = class abstract
  public type
    TProperty = (UniqueID, Manufacturer, Model, SerialNo, Name, Description);
  private
    FOnDataChanged: TNotifyEvent;
    FOnSensorRemoved: TNotifyEvent;
    FOnStateChanged: TNotifyEvent;
    FManager: TSensorManager;
    FStarted: Boolean;
  protected
    // establishes the error class for the sensor
    class function ErrorClass: ESensorExceptionClass; virtual;
    // raises a location sensor error
    class procedure SensorError(const Msg: String; Params: array of const); overload;
    class procedure SensorError(const Msg: String); overload;

    procedure DataChanged; virtual;
    function GetCustomData(const Data): Variant; virtual;
    function GetCustomProperty(const Prop): Variant; virtual;
    function GetHasCustomProperty(const Prop): Boolean; virtual;
    function GetHasCustomData(const Data): Boolean; virtual;
    function GetSensorCategory: TSensorCategory; virtual; abstract;
    function GetSensorProperty(Prop: TProperty): string; virtual;
    function GetState: TSensorState; virtual; abstract;
    function GetTimeStamp: TDateTime; virtual; abstract;
    procedure RemoveSensor;
    procedure SensorRemoved; virtual;
    procedure StateChanged; virtual;
    procedure SetCustomProperty(const Prop; const Value: Variant); virtual;
    constructor Create(AManager: TSensorManager); virtual;
    /// <summary>
    /// Method for overriding in heritor for managing Start method
    /// </summary>
    function DoStart: Boolean; virtual;
    /// <summary>
    /// Method for overriding in heritor for managing Stop method
    /// </summary>
    procedure DoStop; virtual;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    /// <summary>
    /// Activates sensor for work
    /// </summary>
    procedure Start;
    /// <summary>
    /// Stops sensor. After it, sensor can be activated by Start
    /// </summary>
    procedure Stop;
    /// <summary>
    /// Indicates if the sensor is started and ready to using
    /// </summary>
    property Started: Boolean read FStarted;
    property Category: TSensorCategory read GetSensorCategory;
    property CustomData[const Data]: Variant read GetCustomData;
    property CustomProperty[const Prop]: Variant read GetCustomProperty write SetCustomProperty;
    property Description: string index TProperty.Description read GetSensorProperty;
    property HasCustomData[const Data]: Boolean read GetHasCustomData;
    property HasCustomProperty[const Prop]: Boolean read GetHasCustomProperty;
    property Manager: TSensorManager read FManager;
    property Manufacturer: string index TProperty.Manufacturer read GetSensorProperty;
    property Model: string index TProperty.Model read GetSensorProperty;
    property Name: string index TProperty.Name read GetSensorProperty;
    property SerialNo: string index TProperty.SerialNo read GetSensorProperty;
    property State: TSensorState read GetState;
    property TimeStamp: TDateTime read GetTimeStamp;
    property UniqueID: string index TProperty.UniqueID read GetSensorProperty;
    property OnDataChanged: TNotifyEvent read FOnDataChanged write FOnDataChanged;
    property OnSensorRemoved: TNotifyEvent read FOnSensorRemoved write FOnSensorRemoved;
    property OnStateChanged: TNotifyEvent read FOnStateChanged write FOnStateChanged;
  end;

  // describes the location change notification event
  TLocationChangedEvent = procedure (Sender: TObject; const OldLocation, NewLocation: TLocationCoord2D) of object;
  // describes the heading change notification event
  THeadingChangedEvent = procedure (Sender: TObject; const AHeading: THeading) of object;
  // describes the region in proximity event
  TRegionProximityEvent = procedure (Sender: TObject; const Region: TLocationRegion) of object;

  TCustomLocationSensor = class abstract(TCustomSensor)
  public type
    TProperty = (Latitude, Longitude, ErrorRadius, Altitude, Speed, TrueHeading, MagneticHeading,
      Address1, Address2, City, StateProvince, PostalCode, CountryRegion);
    TProperties = set of TProperty;

    // describes a list of regions
    TRegionList = TList<TLocationRegion>;
  private
    FRegions: TRegionList;
    FLocationChange: TLocationChangeType;
    FOptimize: Boolean;
    FOnLocationChanged: TLocationChangedEvent;
    FOnHeadingChanged : THeadingChangedEvent;
    FOnEnterRegion: TRegionProximityEvent;
    FOnExitRegion: TRegionProximityEvent;
  protected
    function GetLocationSensorType: TLocationSensorType; virtual; abstract;
    function GetAvailableProperties: TProperties; virtual; abstract;
    function GetDoubleProperty(Prop: TProperty): Double; virtual;
    function GetStringProperty(Prop: TProperty): string; virtual;
    function GetSensorCategory: TSensorCategory; override;

    function GetAuthorized: TAuthorizationType; virtual; abstract;
    function GetAccuracy: TLocationAccuracy; virtual; abstract;
    function GetDistance: TLocationDistance; virtual; abstract;
    function GetPowerConsumption: TPowerConsumption; virtual; abstract;
    procedure SetAccuracy(const Value: TLocationAccuracy); virtual; abstract;
    procedure SetDistance(const Value: TLocationDistance); virtual; abstract;
    procedure SetLocationChange(Value: TLocationChangeType);        
    procedure SetOptimize(const Value: Boolean);        

    // issued after the location change type has changed
    procedure DoLocationChangeType; virtual; abstract;
    // called after Optimized property is changed in order to enable/disable
    // location notifications optimization
    procedure DoOptimize; virtual; abstract;
    // triggers the location change event handler
    procedure DoLocationChanged(const OldLocation, NewLocation: TLocationCoord2D); virtual;
    // triggers the heading change event handler
    procedure DoHeadingChanged(const AHeading: THeading); virtual;
    // triggers the region entering event
    procedure DoEnterRegion(const Region: TLocationRegion); virtual;
    // triggers the region exiting event
    procedure DoExitRegion(const Region: TLocationRegion); virtual;
    // updates the underlying regions to reflect the ones in Regions property
    procedure RegionNotify(Sender: TObject; const Item: TLocationRegion;
      Action: TCollectionNotification);
    procedure RegionAdded(const Item: TLocationRegion); virtual;
    procedure RegionRemoved(const Item: TLocationRegion); virtual;

    constructor Create(AManager: TSensorManager); override;
  public
    destructor Destroy; override;

    property SensorType: TLocationSensorType read GetLocationSensorType;
    property AvailableProperties: TProperties read GetAvailableProperties;

    // determines the if the application is authorized to use location services
    property Authorized: TAuthorizationType read GetAuthorized;
    // level of accuracy
    property Accuracy: TLocationAccuracy read GetAccuracy write SetAccuracy;
    // minimum distance that triggers new notifications
    property Distance: TLocationDistance read GetDistance write SetDistance;
    // when True, it instantiates more underlying sensors and tries to get the best location values;
    // changing the value while the sensor is started will raise an error
    property Optimize: Boolean read FOptimize write SetOptimize;
    // how much battery the sensor consumes
    property PowerConsumption: TPowerConsumption read GetPowerConsumption;
    // the type of location change that triggers new notifications;
    // changing the value while the sensor is started will raise an error
    property LocationChange: TLocationChangeType read FLocationChange write SetLocationChange;
    // manages a list of regions that are tested for location proximity
    property Regions: TRegionList read FRegions;

    // Latitude in degrees. North is +
    property Latitude: Double index TProperty.Latitude read GetDoubleProperty;
    // Latitude in degrees. East is +
    property Longitude: Double index TProperty.Longitude read GetDoubleProperty;
    // Accuracy radius of Latitude/Longitude values in meters
    property ErrorRadius: Double index TProperty.ErrorRadius read GetDoubleProperty;
    // Altitude in Meters relative to sea level
    property Altitude: Double index TProperty.Altitude read GetDoubleProperty;
    // Speed in Knots
    property Speed: Double index TProperty.Speed read GetDoubleProperty;
    // Heading relative to True North in degrees
    property TrueHeading: Double index TProperty.TrueHeading read GetDoubleProperty;
    // Heading relative to Magnetic North in degrees
    property MagneticHeading: Double index TProperty.MagneticHeading read GetDoubleProperty;
    // Physical Location Address Line 1
    property Address1: string index TProperty.Address1 read GetStringProperty;
    // Physical Location Address Line 2
    property Address2: string index TProperty.Address2 read GetStringProperty;
    // Physical Location City
    property City: string index TProperty.City read GetStringProperty;
    // Physical Location State or Province
    property StateProvince: string index TProperty.StateProvince read GetStringProperty;
    // Physical Location Postal/Zip Code
    property PostalCode: string index TProperty.PostalCode read GetStringProperty;
    // Physical Location Country or Region
    property CountryRegion: string index TProperty.CountryRegion read GetStringProperty;
    // triggered when a location change notification is received
    property OnLocationChanged: TLocationChangedEvent read FOnLocationChanged write FOnLocationChanged;
    // triggered when a heading change notification is received
    property OnHeadingChanged: THeadingChangedEvent read FOnHeadingChanged write FOnHeadingChanged;
    // triggered when a notification is received because the device entered
    // a region registered in the Regions list
    property OnEnterRegion: TRegionProximityEvent read FOnEnterRegion write FOnEnterRegion;
    // triggered when the device exists a registered region
    property OnExitRegion: TRegionProximityEvent read FOnExitRegion write FOnExitRegion;
  end;

  TCustomEnvironmentalSensor = class(TCustomSensor)
  public type
    TProperty = (Temperature, Pressure, Humidity, WindDirection, WindSpeed);
    TProperties = set of TProperty;
  protected
    function GetEnvironmentalSensorType: TEnvironmentalSensorType; virtual; abstract;
    function GetAvailableProperties: TProperties; virtual; abstract;
    function GetDoubleProperty(Prop: TProperty): Double; virtual;
    function GetSensorCategory: TSensorCategory; override;
  public
    property SensorType: TEnvironmentalSensorType read GetEnvironmentalSensorType;
    property AvailableProperties: TProperties read GetAvailableProperties;
    // Temperature in Celcius
    property Temperature: Double index TProperty.Temperature read GetDoubleProperty;
    // Atmospheric pressure in in Bars
    property Pressure: Double index TProperty.Pressure read GetDoubleProperty;
    // Relative humidity in percentage
    property Humidity: Double index TProperty.Humidity read GetDoubleProperty;
    // Wind direction relative to magnetic north in degrees counter-clockwise
    property WindDirection: Double index TProperty.WindDirection read GetDoubleProperty;
    // Wind speed in meters per second
    property WindSpeed: Double index TProperty.WindSpeed read GetDoubleProperty;
  end;

  TCustomMotionSensor = class(TCustomSensor)
  public type
    TProperty = (AccelerationX, AccelerationY, AccelerationZ, AngleAccelX, AngleAccelY, AngleAccelZ, Motion, Speed);
    TProperties = set of TProperty;
  protected
    function GetMotionSensorType: TMotionSensorType; virtual; abstract;
    function GetAvailableProperties: TProperties; virtual; abstract;
    function GetDoubleProperty(Prop: TProperty): Double; virtual;
    function GetSensorCategory: TSensorCategory; override;
    function GetUpdateInterval: Double; virtual; abstract;
    procedure SetUpdateInterval(AInterval: Double); virtual; abstract;
    constructor Create(AManager: TSensorManager); override;
  public
    property SensorType: TMotionSensorType read GetMotionSensorType;
    property AvailableProperties: TProperties read GetAvailableProperties;
    // X-axis Acceleration in G's (Gravity)
    property AccelerationX: Double index TProperty.AccelerationX read GetDoubleProperty;
    // Y-axis Acceleration in G's (Gravity)
    property AccelerationY: Double index TProperty.AccelerationY read GetDoubleProperty;
    // Z-axis Acceleration in G's (Gravity)
    property AccelerationZ: Double index TProperty.AccelerationZ read GetDoubleProperty;
    // X-axis Angular Acceleration in Degrees/Second^2
    property AngleAccelX: Double index TProperty.AngleAccelX read GetDoubleProperty;
    // Y-axis Angular Acceleration in Degrees/Second^2
    property AngleAccelY: Double index TProperty.AngleAccelY read GetDoubleProperty;
    // Z-axis Angular Acceleration in Degrees/Second^2
    property AngleAccelZ: Double index TProperty.AngleAccelZ read GetDoubleProperty;
    // Motion State
    property Motion: Double index TProperty.Motion read GetDoubleProperty;
    // Speed in Meters/Second
    property Speed: Double index TProperty.Speed read GetDoubleProperty;
    // Determines how often motion data is updated
    property UpdateInterval: Double read GetUpdateInterval write SetUpdateInterval;
  end;

  TCustomOrientationSensor = class(TCustomSensor)
  public type
    TProperty = (TiltX, TiltY, TiltZ, DistanceX, DistanceY, DistanceZ, HeadingX, HeadingY, HeadingZ, MagHeading,
      TrueHeading, CompMagHeading, CompTrueHeading);
    TProperties = set of TProperty;
  protected
    function GetOrientationSensorType: TOrientationSensorType; virtual; abstract;
    function GetAvailableProperties: TProperties; virtual; abstract;
    function GetDoubleProperty(Prop: TProperty): Double; virtual;
    function GetSensorCategory: TSensorCategory; override;
    function GetUpdateInterval: Double; virtual; abstract;
    procedure SetUpdateInterval(AInterval: Double); virtual; abstract;
    constructor Create(AManager: TSensorManager); override;
  public
    // indicates if the sensor is started, periodically receiving updates
    property SensorType: TOrientationSensorType read GetOrientationSensorType;
    property AvailableProperties: TProperties read GetAvailableProperties;
    // Inclinometer x-axis angle in degrees
    property TiltX: Double index TProperty.TiltX read GetDoubleProperty;
    // Inclinometer y-axis angle in degrees
    property TiltY: Double index TProperty.TiltY read GetDoubleProperty;
    // Inclinometer z-axis angle in degrees
    property TiltZ: Double index TProperty.TiltZ read GetDoubleProperty;
    // Distance x-axis in meters
    property DistanceX: Double index TProperty.DistanceX read GetDoubleProperty;
    // Distance y-axis in meters
    property DistanceY: Double index TProperty.DistanceY read GetDoubleProperty;
    // Distance z-axis in meters
    property DistanceZ: Double index TProperty.DistanceZ read GetDoubleProperty;
    // Compass heading x-axis in degrees
    property HeadingX: Double index TProperty.HeadingX read GetDoubleProperty;
    // Compass heading y-axis in degrees
    property HeadingY: Double index TProperty.HeadingY read GetDoubleProperty;
    // Compass heading z-axis in degrees
    property HeadingZ: Double index TProperty.HeadingZ read GetDoubleProperty;
    // Compass heading relative to magnetic north (uncompensated)
    property MagHeading: Double index TProperty.MagHeading read GetDoubleProperty;
    // Compass heading relative to true north (uncompensated)
    property TrueHeading: Double index TProperty.TrueHeading read GetDoubleProperty;
    // Compass heading relative to magnetic north (compensated)
    property CompMagHeading: Double index TProperty.CompMagHeading read GetDoubleProperty;
    // Compass heading relative to true north (compensated)
    property CompTrueHeading: Double index TProperty.CompTrueHeading read GetDoubleProperty;
    // Determines how often motion data is updated
    property UpdateInterval: Double read GetUpdateInterval write SetUpdateInterval;
  end;

  TCustomElectricalSensor = class(TCustomSensor)
  public type
    TProperty = (Capacitance, Resistance, Inductance, Current, Voltage, Power);
    TProperties = set of TProperty;
  protected
    function GetElectricalSensorType: TElectricalSensorType; virtual; abstract;
    function GetAvailableProperties: TProperties; virtual; abstract;
    function GetDoubleProperty(Prop: TProperty): Double; virtual;
    function GetSensorCategory: TSensorCategory; override;
  public
    property SensorType: TElectricalSensorType read GetElectricalSensorType;
    property AvailableProperties: TProperties read GetAvailableProperties;
    // Capacitance in Farads
    property Capacitance: Double index TProperty.Capacitance read GetDoubleProperty;
    // Resistance in Ohms
    property Resistance: Double index TProperty.Resistance read GetDoubleProperty;
    // Inductance in Henries
    property Inductance: Double index TProperty.Inductance read GetDoubleProperty;
    // Current in Amperes
    property Current: Double index TProperty.Current read GetDoubleProperty;
    // Electrical potential in Volts
    property Voltage: Double index TProperty.Voltage read GetDoubleProperty;
    // Power in Watts
    property Power: Double index TProperty.Power read GetDoubleProperty;
  end;

  TCustomMechanicalSensor = class(TCustomSensor)
  public type
    TProperty = (SwitchState, SwitchArrayState, MultiValueState, Force, AbsPressure, GaugePressure, Strain, Weight);
    TProperties = set of TProperty;
  protected
    function GetMechanicalSensorType: TMechanicalSensorType; virtual; abstract;
    function GetAvailableProperties: TProperties; virtual; abstract;
    function GetDoubleProperty(Prop: TProperty): Double; virtual;
    function GetBooleanProperty(Prop: TProperty): Boolean; virtual;
    function GetStateProperty(Prop: TProperty): Cardinal; virtual;
    function GetSensorCategory: TSensorCategory; override;
  public
    property SensorType: TMechanicalSensorType read GetMechanicalSensorType;
    property AvailableProperties: TProperties read GetAvailableProperties;
    // Boolean Switch State
    property SwitchState: Boolean index TProperty.SwitchState read GetBooleanProperty;
    // Boolean Array Switch State
    property SwitchArrayState: Cardinal index TProperty.SwitchArrayState read GetStateProperty;
    // Multi Switch State
    property MultiValueState: Double index TProperty.MultiValueState read GetDoubleProperty;
    // Mechanical Force in Newtons
    property Force: Double index TProperty.Force read GetDoubleProperty;
    // Absolure Pressure in Pascals
    property AbsPressure: Double index TProperty.AbsPressure read GetDoubleProperty;
    // Gauge Pressure Force in Pascals
    property GaugePressure: Double index TProperty.GaugePressure read GetDoubleProperty;
    // Strain
    property Strain: Double index TProperty.Strain read GetDoubleProperty;
    // Weight in Kilograms
    property Weight: Double index TProperty.Weight read GetDoubleProperty;
  end;

  TCustomBiometricSensor = class(TCustomSensor)
  public type
    TProperty = (HumanPresence, HumanProximity, Touch);
    TProperties = set of TProperty;
  protected
    function GetBiometricSensorType: TBiometricSensorType; virtual; abstract;
    function GetAvailableProperties: TProperties; virtual; abstract;
    function GetDoubleProperty(Prop: TProperty): Double; virtual;
    function GetBooleanProperty(Prop: TProperty): Boolean; virtual;
    function GetSensorCategory: TSensorCategory; override;
  public
    property SensorType: TBiometricSensorType read GetBiometricSensorType;
    property AvailableProperties: TProperties read GetAvailableProperties;
    // Human presence - True when human at computer
    property HumanPresence: Boolean index TProperty.HumanPresence read GetBooleanProperty;
    // Human proximity - distance in meters between computer and human
    property HumanProximity: Double index TProperty.HumanProximity read GetDoubleProperty;
    // Touch - True if sensor is being touched
    property Touch: Boolean index TProperty.Touch read GetBooleanProperty;
  end;

  TCustomLightSensor = class(TCustomSensor)
  public type
    TProperty = (Lux, Temperature, Chromacity);
    TProperties = set of TProperty;
  protected
    function GetLightSensorType: TLightSensorType; virtual; abstract;
    function GetAvailableProperties: TProperties; virtual; abstract;
    function GetDoubleProperty(Prop: TProperty): Double; virtual;
    function GetSensorCategory: TSensorCategory; override;
  public
    property SensorType: TLightSensorType read GetLightSensorType;
    property AvailableProperties: TProperties read GetAvailableProperties;
    // Ambient Light in LUX, Lumens per Square Meter
    property Lux: Double index TProperty.Lux read GetDoubleProperty;
    // Light temperature in Degrees Kelvin
    property Temperature: Double index TProperty.Temperature read GetDoubleProperty;
    // Ligth Chromacity
    property Chromacity: Double index TProperty.Chromacity read GetDoubleProperty;
  end;

  TCustomScannerSensor = class(TCustomSensor)
  public type
    TProperty = (RFIDTag, BarcodeData);
    TProperties = set of TProperty;
  protected
    function GetScannerSensorType: TScannerSensorType; virtual; abstract;
    function GetAvailableProperties: TProperties; virtual; abstract;
    function GetUInt64Property(Prop: TProperty): UInt64; virtual;
    function GetStringProperty(Prop: TProperty): string; virtual;
    function GetSensorCategory: TSensorCategory; override;
  public
    property SensorType: TScannerSensorType read GetScannerSensorType;
    property AvailableProperties: TProperties read GetAvailableProperties;
    // 40 bit RFID tag value
    property RFIDTag: UInt64 index TProperty.RFIDTag read GetUInt64Property;
    // Scanned barcode data
    property BarcodeData: string index TProperty.BarcodeData read GetStringProperty;
  end;

  TSensorManagerEvent = procedure (Sender: TObject; Sensor: TCustomSensor; State: TSensorState) of object;
  TSensorArray = array of TCustomSensor;

  // represents a sensor filter type; Sensor is the sensor to be filtered;
  // return True if Sensor is the one you are interested in
  TSensorFilter = reference to function (Sensor: TCustomSensor): Boolean;

  TSensorManager = class abstract
  public type
    TFilters = TDictionary<String, TSensorFilter>;
  private class var
    FCurrentManager: TSensorManager;
    class function InternalGetSensorManager: TSensorManager; static;
    class constructor Create;
    class destructor Destroy;
  protected type
    TSensorManagerType = class of TSensorManager;
  protected class var
    FSensorManagerType: TSensorManagerType;
  private
    FSensorManagerEvents: TList<TSensorManagerEvent>;
    FSensors: TObjectList<TCustomSensor>;
    FFilters: TFilters;
    function GetCount: Integer; inline;
    function GetSensor(Index: Integer): TCustomSensor; overload; inline;
  protected
    class function GetSensorManager: TSensorManager; virtual; abstract;
    procedure AddSensor(Sensor: TCustomSensor);
    procedure SensorManagerEvent(Sensor: TCustomSensor; State: TSensorState);
    function GetCanActivate: Boolean; virtual; abstract;
    function GetActive: Boolean; virtual; abstract;
    procedure SetActive(Value: Boolean); virtual;
    procedure RemoveSensor(Sensor: TCustomSensor);
    constructor Create;
  public
    destructor Destroy; override;
    procedure Activate; virtual; abstract;
    procedure AddSensorManagerEvent(Event: TSensorManagerEvent);
    procedure Deactivate; virtual; abstract;
    // returns an array of sensors that fall in the requested Category
    function GetSensorsByCategory(Category: TSensorCategory): TSensorArray;
    // returns an array of sensors that satifisfy the given filter
    function GetSensorsByFilter(Filter: TSensorFilter): TSensorArray;
    procedure RemoveSensorManagerEvent(Event: TSensorManagerEvent);

    property Active: Boolean read GetActive write SetActive;
    property CanActivate: Boolean read GetCanActivate;
    property Count: Integer read GetCount;
    property Sensors[Index: Integer]: TCustomSensor read GetSensor; default;
    // manages a list of filters which can later be reused to filter out and query sensors;
    // each filter has a unique name to be easier to be identified
    property Filters: TFilters read FFilters;
    class property Current: TSensorManager read InternalGetSensorManager;
  end;

  // describes the event receiving forward geocoding results
  TGeocodeEvent = procedure (const Coords: TArray<TLocationCoord2D>) of object;
  // describes the event receiving reverse geocoding results
  TGeocodeReverseEvent = procedure (const Address: TCivicAddress) of object;

  TGeocoder = class abstract(TObject)
  private class var
    // the current geocoder implementer
    FCurrent: TGeocoderClass;
    FGeocoding: Boolean;
    FOnGeocode: TGeocodeEvent;
    FOnGeocodeReverse: TGeocodeReverseEvent;

    class function GetCurrent: TGeocoderClass; static; inline;
  protected class var
    // stores the forward geocoding results; it must be populated by descendants
    // when they obtain the notification from the device
    FGeoFwdCoords: TArray<TLocationCoord2D>;
    // stores the reverse geocoding result; it must be populated by descendants
    // when they obtain the notification from the device
    FGeoRevAddress: TCivicAddress;
  private
    class function GeocoderImplementer: TGeocoderClass;
  protected
    // raises a geocoding exception
    class procedure GeocodeError(const Msg: String);

    // descendants return here the class that implements the geocoder functionality
    class function GetGeocoderImplementer: TGeocoderClass; virtual; abstract;
    // does the actual forward geocoding request
    class procedure GeocodeRequest(const Address: TCivicAddress); virtual; abstract;
    // does the actual reverse geocoding request
    class procedure GeocodeReverseRequest(const Coords: TLocationCoord2D); virtual; abstract;
    // triggers the event handler for OnGeocode
    class procedure DoGeocode(const Coords: TArray<TLocationCoord2D>); virtual;
    // triggers the event handler for OnGeocodeReverse
    class procedure DoGeocodeReverse(const Address: TCivicAddress); virtual;

    // issued when the class is first used through the Current property; if
    // some finalization must be done, descendants can use the class destructor
    class procedure Initialize; virtual;
  public
    // determines whether the geocoding can be realized
    class function Supported: Boolean; virtual; abstract;
    // determines whether the application is authorized to use the service
    class function Authorized: TAuthorizationType; virtual; abstract;
    // cancels the current geocoding proces
    class procedure Cancel; virtual; abstract;
    // returns whether the application is currently processing a geocoding request
    class function Geocoding: Boolean; inline;
    // forward geocoding and triggers OnGeocode when done
    class procedure Geocode(const Address: TCivicAddress);
    // reverse geocoding and triggers OnGeocodeReverse when Done
    class procedure GeocodeReverse(const Coords: TLocationCoord2D);

    // the current geocoder implementer
    class property Current: TGeocoderClass read GetCurrent;
    // event triggered when the forward geocoding notification has arrived;
    // if the array of coordinates is empty, it means that it could not
    // determine any corresponding coordinates
    class property OnGeocode: TGeocodeEvent read FOnGeocode write FOnGeocode;
    // event triggerd when the reverse geocoding notification has arrived;
    // if the Address is nil, it means that the Address could not be determined
    class property OnGeocodeReverse: TGeocodeReverseEvent read FOnGeocodeReverse write FOnGeocodeReverse;
  end;

  TGpsStatus = class abstract(TObject)
  private class var
    FCurrent: TGpsStatusClass;

    class function GetCurrent: TGpsStatusClass; static; inline;
  protected
    // raises a GPS exception
    class procedure GpsStatusError(const Msg: String);

    // descendants return the class the implements the Gps status functionality
    class function GetGpsStatusImplementer: TGpsStatusClass; virtual; abstract;
    // issued when the class is first used through the Current property; if
    // some finalization must be done, descendants can use the class destructor
    class procedure Initialize; virtual;
  public
    // determines whether the device supports querying the Gps status
    class function Supported: Boolean; virtual; abstract;
    // determines whether the application is authorized to use the service
    class function Authorized: TAuthorizationType; virtual; abstract;
    // access to the state of each GPS sattelite
    class function Satellites(Index: Integer): TGpsSatellite; virtual; abstract;
    // the number of sattelites
    class function SatelliteCount: Integer; virtual; abstract;
    // the time required to receive the first fix since the most recent restart of the GPS engine
    class function SatelliteFirstFixTime: Integer; virtual; abstract;

    // the current Gps status implementer
    class property Current: TGpsStatusClass read GetCurrent;
  end;

implementation

uses
  System.Variants, System.Math, System.Character,
{$IFDEF ANDROID}
  // -- patch dd
  // use the modified System.Android.Sensors
  System.Android.SensorsDD;
{$ENDIF ANDROID}
{$IFDEF IOS}
  System.iOS.Sensors;
{$ELSE}
{$IFDEF MACOS}
  System.Mac.Sensors;
{$ENDIF MACOS}
{$ENDIF IOS}
{$IFDEF MSWINDOWS}
  System.Win.Sensors;
{$ENDIF}


{ TGpsSattelite }

constructor TGpsSatellite.Create(Azimuth, Elevation: Double; Prn: Integer;
  Snr: Double; HasAlmanac, HasEphemeris, UsedInFix: Boolean);
begin
  FAzimuth := Azimuth;
  FElevation := Elevation;
  FPrn := Prn;
  FSnr := Snr;
  FHasAlmanac := HasAlmanac;
  FHasEphemeris := HasEphemeris;
  FUsedInFix := UsedInFix;
end;

{ TGeoCoord2D }

constructor TLocationCoord2D.Create(ALatitude, ALongitude: TLocationDegrees);
begin
  Latitude := ALatitude;
  Longitude := ALongitude;
end;

{ TGeoRegion }

constructor TLocationRegion.Create(const ACenter: TLocationCoord2D;
  ARadius: TLocationDistance);
var
  GUID: TGUID;
begin
  // create the unique ID
  CreateGUID(GUID);
  Create(ACenter, ARadius, GUIDToString(GUID));
end;

{ TSensorManager }

class constructor TSensorManager.Create;
begin
  FSensorManagerType := TPlatformSensorManager;
end;

class destructor TSensorManager.Destroy;
begin
  FreeAndNil(FCurrentManager);
end;

destructor TSensorManager.Destroy;
begin
  Deactivate;
  FSensorManagerEvents.Free;
  FreeAndNil(FSensors);
  FFilters.Free;
  inherited;
end;

function TSensorManager.GetCount: Integer;
begin
  Result := FSensors.Count;
end;

function TSensorManager.GetSensor(Index: Integer): TCustomSensor;
begin
  Result := FSensors[Index];
end;

function TSensorManager.GetSensorsByFilter(Filter: TSensorFilter): TSensorArray;
var
  Sensor: TCustomSensor;
  i: Integer;
begin
  Result := nil;
  if Assigned(Filter) then
    for i := 0 to Count - 1 do
    begin
      Sensor := Sensors[i];
      if Filter(Sensor) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[High(Result)] := Sensor;
      end;
    end;
end;

function TSensorManager.GetSensorsByCategory(Category: TSensorCategory): TSensorArray;
var
  I: Integer;
  Sensor: TCustomSensor;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    Sensor := Sensors[I];
    if Sensor.Category = Category then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := Sensor;
    end;
  end;
end;

class function TSensorManager.InternalGetSensorManager: TSensorManager;
begin
  if FCurrentManager = nil then
    if FSensorManagerType <> nil then
      FCurrentManager := FSensorManagerType.GetSensorManager
    else
      raise ESensorManagerException.Create('No SensorManager implementation found');
  Result := FCurrentManager;
end;

procedure TSensorManager.AddSensor(Sensor: TCustomSensor);
begin
  if (FSensors <> nil) and (FSensors.IndexOf(Sensor) < 0) then
  begin
    FSensors.Add(Sensor);
    SensorManagerEvent(Sensor, TSensorState.Added);
  end;
end;

procedure TSensorManager.AddSensorManagerEvent(Event: TSensorManagerEvent);
begin
  FSensorManagerEvents.Add(Event);
end;

procedure TSensorManager.RemoveSensor(Sensor: TCustomSensor);
begin
  if (FSensors <> nil) and (Sensor <> nil) and (Sensor.FManager <> nil) then
  begin
    try
      SensorManagerEvent(Sensor, TSensorState.Removed);
    finally
      Sensor.FManager := nil;
      FSensors.Remove(Sensor);
    end;
  end;
end;

procedure TSensorManager.RemoveSensorManagerEvent(Event: TSensorManagerEvent);
begin
  FSensorManagerEvents.Remove(Event);
end;

constructor TSensorManager.Create;
begin
  inherited Create;
  FSensorManagerEvents := TList<TSensorManagerEvent>.Create;
  FSensors := TObjectList<TCustomSensor>.Create;
  FFilters := TFilters.Create;
end;

procedure TSensorManager.SensorManagerEvent(Sensor: TCustomSensor; State: TSensorState);
var
  Event: TSensorManagerEvent;
begin
  Event := nil;
  for Event in FSensorManagerEvents do
    if Assigned(Event) then
      Event(Self, Sensor, State);
end;

procedure TSensorManager.SetActive(Value: Boolean);
begin
  if Active <> Value then
    if Value then
      Activate
    else
      Deactivate;
end;

{ TSensor }

procedure TCustomSensor.AfterConstruction;
begin
  inherited AfterConstruction;
  if FManager <> nil then
    FManager.AddSensor(Self);
end;

constructor TCustomSensor.Create(AManager: TSensorManager);
begin
  inherited Create;
  FManager := AManager;
  FStarted := False;
end;

procedure TCustomSensor.DataChanged;
begin
  if Assigned(FOnDataChanged) then
    FOnDataChanged(Self);
end;

destructor TCustomSensor.Destroy;
begin
  if FManager <> nil then
    FManager.RemoveSensor(Self);
  inherited Destroy;
end;

function TCustomSensor.DoStart: Boolean;
begin
  Result := True;
end;

procedure TCustomSensor.DoStop;
begin

end;

class function TCustomSensor.ErrorClass: ESensorExceptionClass;
begin
  Result := ESensorException;
end;

function TCustomSensor.GetCustomData(const Data): Variant;
begin
  Result := Null;
end;

function TCustomSensor.GetCustomProperty(const Prop): Variant;
begin
  Result := Null;
end;

function TCustomSensor.GetHasCustomData(const Data): Boolean;
begin
  Result := False;
end;

function TCustomSensor.GetHasCustomProperty(const Prop): Boolean;
begin
  Result := False;
end;

function TCustomSensor.GetSensorProperty(Prop: TProperty): string;
begin
  Result := '';
end;

procedure TCustomSensor.RemoveSensor;
begin
  SensorRemoved;
  if FManager <> nil then
    FManager.RemoveSensor(Self);
end;

class procedure TCustomSensor.SensorError(const Msg: String;
  Params: array of const);
begin
  ErrorClass.CreateFmt(Msg, Params);
end;

class procedure TCustomSensor.SensorError(const Msg: String);
begin
  ErrorClass.Create(Msg);
end;

procedure TCustomSensor.SensorRemoved;
begin
  if Assigned(FOnSensorRemoved) then
    FOnSensorRemoved(Self);
end;

procedure TCustomSensor.SetCustomProperty(const Prop; const Value: Variant);
begin
  {}
end;

procedure TCustomSensor.Start;
begin
  if not Started then
    FStarted := DoStart;
end;

procedure TCustomSensor.StateChanged;
begin
  if Assigned(FOnStateChanged) then
    FOnStateChanged(Self);
end;

procedure TCustomSensor.Stop;
begin
  if Started then
  begin
    DoStop;
    FStarted := False;
  end;
end;

{ TCustomLocationSensor }

constructor TCustomLocationSensor.Create(AManager: TSensorManager);
begin
  inherited;
  FRegions := TRegionList.Create;
  FRegions.OnNotify := RegionNotify;
end;

destructor TCustomLocationSensor.Destroy;
begin
  Stop;
  FRegions.Free;
  inherited Destroy;
end;

procedure TCustomLocationSensor.DoExitRegion(const Region: TLocationRegion);
begin
  if Assigned(OnExitRegion) then
    OnExitRegion(Self, Region);
end;

procedure TCustomLocationSensor.DoHeadingChanged(const AHeading: THeading);
begin
  if Assigned(OnHeadingChanged) then
    OnHeadingChanged(Self, AHeading);
end;

procedure TCustomLocationSensor.DoLocationChanged(const OldLocation,
  NewLocation: TLocationCoord2D);
begin
  if Assigned(OnLocationChanged) and not (SameValue(OldLocation.Latitude, NewLocation.Latitude) and
    SameValue(OldLocation.Longitude, NewLocation.Longitude)) then
    OnLocationChanged(Self, OldLocation, NewLocation);
end;

procedure TCustomLocationSensor.DoEnterRegion(const Region: TLocationRegion);
begin
  if Assigned(OnEnterRegion) then
    OnEnterRegion(Self, Region);
end;

class procedure TGeocoder.DoGeocode(const Coords: TArray<TLocationCoord2D>);
begin
  if Assigned(OnGeocode) then
    OnGeocode(Coords);
end;

class procedure TGeocoder.DoGeocodeReverse(const Address: TCivicAddress);
begin
  if Assigned(OnGeocodeReverse) then
    OnGeocodeReverse(Address);
end;

class procedure TGeocoder.Geocode(
  const Address: TCivicAddress);
begin
  if Geocoding then
    GeocodeError(SGeocodeMultipleRequests);

  FGeocoding := True;
  GeocodeRequest(Address);
  FGeocoding := False;
end;

class procedure TGeocoder.GeocodeReverse(
  const Coords: TLocationCoord2D);
begin
  if Geocoding then
    GeocodeError(SGeocodeMultipleRequests);

  FGeocoding := True;
  GeocodeReverseRequest(Coords);
  FGeocoding := False;
end;

class function TGeocoder.GeocoderImplementer: TGeocoderClass;
begin
  Result := GetGeocoderImplementer;
end;

class function TGeocoder.Geocoding: Boolean;
begin
  Result := FGeocoding;
end;

class function TGeocoder.GetCurrent: TGeocoderClass;
begin
  if not Assigned(TGeocoder.FCurrent) then
    TGeocoder.FCurrent := TPlatformGeocoder.GeocoderImplementer;

  Result := TGeocoder.FCurrent;
end;

class procedure TGeocoder.Initialize;
begin
  // do nothing
end;

function TCustomLocationSensor.GetDoubleProperty(Prop: TProperty): Double;
begin
  Result := NaN;
end;

function TCustomLocationSensor.GetSensorCategory: TSensorCategory;
begin
  Result := TSensorCategory.Location;
end;

function TCustomLocationSensor.GetStringProperty(Prop: TProperty): string;
begin
  Result := '';
end;

procedure TCustomLocationSensor.RegionAdded(const Item: TLocationRegion);
begin
  // do nothing
end;

procedure TCustomLocationSensor.RegionNotify(Sender: TObject;
  const Item: TLocationRegion; Action: TCollectionNotification);
begin
  if Action = cnAdded then
                                                                             
    RegionAdded(Item)
  else
                                                              
    RegionRemoved(Item);
end;

procedure TCustomLocationSensor.RegionRemoved(const Item: TLocationRegion);
begin
  // do nothing
end;

procedure TCustomLocationSensor.SetLocationChange(Value: TLocationChangeType);
begin
  if FLocationChange <> Value then
  begin
    if Started then
      SensorError(SLocationSensorStarted);

    FLocationChange := Value;
    DoLocationChangeType;
  end;
end;

procedure TCustomLocationSensor.SetOptimize(const Value: Boolean);
begin
  if FOptimize <> Value then
  begin
    if Started then
      SensorError(SLocationSensorStarted);

    FOptimize := Value;
    DoOptimize;
  end;
end;

{ TCustomMotionSensor }

constructor TCustomMotionSensor.Create(AManager: TSensorManager);
begin
  inherited Create(AManager);
end;

function TCustomMotionSensor.GetDoubleProperty(Prop: TProperty): Double;
begin
  Result := NaN;
end;

function TCustomMotionSensor.GetSensorCategory: TSensorCategory;
begin
  Result := TSensorCategory.Motion;
end;

{ TCustomLightSensor }

function TCustomLightSensor.GetDoubleProperty(Prop: TProperty): Double;
begin
  Result := NaN;
end;

function TCustomLightSensor.GetSensorCategory: TSensorCategory;
begin
  Result := TSensorCategory.Light;
end;

{ TCustomMechanicalSensor }

function TCustomMechanicalSensor.GetBooleanProperty(Prop: TProperty): Boolean;
begin
  Result := False;
end;

function TCustomMechanicalSensor.GetDoubleProperty(Prop: TProperty): Double;
begin
  Result := NaN;
end;

function TCustomMechanicalSensor.GetSensorCategory: TSensorCategory;
begin
  Result := TSensorCategory.Mechanical;
end;

function TCustomMechanicalSensor.GetStateProperty(Prop: TProperty): Cardinal;
begin
  Result := 0;
end;

{ TCustomEnvironmentalSensor }

function TCustomEnvironmentalSensor.GetDoubleProperty(Prop: TProperty): Double;
begin
  Result := NaN;
end;

function TCustomEnvironmentalSensor.GetSensorCategory: TSensorCategory;
begin
  Result := TSensorCategory.Environmental;
end;

{ TCustomOrientationSensor }

constructor TCustomOrientationSensor.Create(AManager: TSensorManager);
begin
  inherited Create(AManager);
  FStarted := False;
end;

function TCustomOrientationSensor.GetDoubleProperty(Prop: TProperty): Double;
begin
  Result := NaN;
end;

function TCustomOrientationSensor.GetSensorCategory: TSensorCategory;
begin
  Result := TSensorCategory.Orientation;
end;

{ TCustomElectricalSensor }

function TCustomElectricalSensor.GetDoubleProperty(Prop: TProperty): Double;
begin
  Result := NaN;
end;

function TCustomElectricalSensor.GetSensorCategory: TSensorCategory;
begin
  Result := TSensorCategory.Electrical;
end;

{ TCustomScannerSensor }

function TCustomScannerSensor.GetSensorCategory: TSensorCategory;
begin
  Result := TSensorCategory.Scanner;
end;

function TCustomScannerSensor.GetStringProperty(Prop: TProperty): string;
begin
  Result := '';
end;

function TCustomScannerSensor.GetUInt64Property(Prop: TProperty): UInt64;
begin
  Result := 0;
end;

{ TCustomBiometricSensor }

function TCustomBiometricSensor.GetBooleanProperty(Prop: TProperty): Boolean;
begin
  Result := False;
end;

function TCustomBiometricSensor.GetDoubleProperty(Prop: TProperty): Double;
begin
  Result := NaN;
end;

function TCustomBiometricSensor.GetSensorCategory: TSensorCategory;
begin
  Result := TSensorCategory.Biometric
end;

{ TCivicAddress }

procedure TCivicAddress.AssignTo(Dest: TPersistent);
var
  D: TCivicAddress;
begin
  inherited;

  if Dest is TCivicAddress then
  begin
    D := TCivicAddress(Dest);

    D.Coord := Coord;
    D.Address := Address;
    D.AdminArea := AdminArea;
    D.CountryCode := CountryCode;
    D.CountryName := CountryName;
    D.FeatureName := FeatureName;
    D.Locale := Locale;
    D.Locality := Locality;
    D.Phone := Phone;
    D.PostalCode := PostalCode;
    D.Premises := Premises;
    D.SubAdminArea := SubAdminArea;
    D.SubLocality := SubLocality;
    D.SubThoroughfare := SubThoroughfare;
    D.Thoroughfare := Thoroughfare;
    D.URL := URL;
  end;
end;

function TCivicAddress.ConcatAddrStrings(const Strings: array of String): String;
const
  CSeparator = ', ';
var
  i: Integer;
begin
  Result := '';
  if Length(Strings) > 0 then
    for i := Low(Strings) to High(Strings) do
    begin
      Result := Result + Strings[i];

      if i < High(Strings) then
        Result := Result + CSeparator;
    end;
end;

function TCivicAddress.ToString: String;
var
  Handled: Boolean;
begin
  if Assigned(OnConvertAddress) then
  begin
    OnConvertAddress(Self, Result, Handled);

    if not Handled then
      Result := ConcatAddrStrings([
        Address,
        AdminArea,
        CountryCode,
        FeatureName,
        AdminArea,
        CountryCode,
        CountryName,
        FeatureName,
        Locale,
        Locality,
        Phone,
        PostalCode,
        Premises,
        SubAdminArea,
        SubLocality,
        SubThoroughfare,
        Thoroughfare]);
  end
  else
    Result := ConcatAddrStrings([
        Address,
        AdminArea,
        CountryCode,
        FeatureName,
        AdminArea,
        CountryCode,
        CountryName,
        FeatureName,
        Locale,
        Locality,
        Phone,
        PostalCode,
        Premises,
        SubAdminArea,
        SubLocality,
        SubThoroughfare,
        Thoroughfare]);
end;

class procedure TGeocoder.GeocodeError(const Msg: String);
begin
  raise EGeocoderException.Create(Msg);
end;

{ TGpsStatus }

class function TGpsStatus.GetCurrent: TGpsStatusClass;
begin
  if not Assigned(TGpsStatus.FCurrent) then
  begin
    TGpsStatus.FCurrent := TPlatformGpsStatus.GetGpsStatusImplementer;
    TGpsStatus.FCurrent.Initialize;
  end;

  Result := TGpsStatus.FCurrent;
end;

class procedure TGpsStatus.GpsStatusError(const Msg: String);
begin
  raise EGpsStatusException.Create(Msg);
end;

class procedure TGpsStatus.Initialize;
begin
  // do nothing
end;

constructor TLocationRegion.Create(const ACenter: TLocationCoord2D;
  ARadius: TLocationDistance; const AID: String);
begin
  FID := AID;
  FCenter := ACenter;
  FRadius := ARadius;
end;

class function TLocationRegion.Empty: TLocationRegion;
begin
  Result := TLocationRegion.Create(TLocationCoord2D.Create(0, 0), 0, '');
end;

initialization
  TGeocoder.FGeoRevAddress := TCivicAddress.Create;

finalization
  FreeAndNil(TGeocoder.FGeoRevAddress);

end.
