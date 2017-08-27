unit Model.REST.Cerimonie;

interface

uses
  System.Generics.Collections, BOs, System.Constant, System.Net.HttpClient,
  InterfacesService, ObjectsMappers, System.Net.URLClient;

type

  // RRP stays for REST Remote Proxy
  ICerimonieApiRRP = interface
    ['{4AB89818-9BCA-4D15-8F8D-CE4B11788989}']
    function GetImpegni: TObjectList<TImpegno>;
    function GetNominativi(const AIDImpegno: Integer; const ANominativo: string)
      : TObjectList<TAnagrafica>;
    function GetEventStats(const AIDImpegno: Integer): TStatistica;
    function GetGruppo(const AIDImpegno: Integer; const AIDAnagrafica: Integer)
      : TObjectList<TAnagrafica>;
    function DoCheckAction(const AIDImpegnoAnagrafica: Integer;
      const ADirection: TCheckDirection): Integer;
    function PostAnagrafica(const AAnagrafica: TAnagrafica;
      const AObjOwner: boolean = true): Integer;
  end;

  TFilter = class(TObject)
  private
    FIdImpegno: Integer;
    FNominativo: string;
    FIdAnag: Integer;
    FAnagrafica: TAnagrafica;
    FIdAnagimpegni: Integer;
    procedure SetIdImpegno(const Value: Integer);
    procedure SetNominativo(const Value: string);
    procedure SetIdAnag(const Value: Integer);
    procedure SetAnagrafica(const Value: TAnagrafica);
    procedure SetIdAnagimpegni(const Value: Integer);
  public
    destructor Destroy; override;
    property IdImpegno: Integer read FIdImpegno write SetIdImpegno;
    property Nominativo: string read FNominativo write SetNominativo;
    property IdAnag: Integer read FIdAnag write SetIdAnag;
    property IdAnagimpegni: Integer read FIdAnagimpegni write SetIdAnagimpegni;
    property Anagrafica: TAnagrafica read FAnagrafica write SetAnagrafica;
  end;

  TCerimonieRRP = class(TInterfacedObject, ICerimonieApiRRP)
  private
    FClient: THTTPClient;
    FSecurityService: ISecurityService;
  protected
    procedure CheckOnBeforeRequest;
    function DoInternalPost(const AURL: string; ABody: TObject): IHTTPResponse;
    function DoInternalGet(const AURL: string): IHTTPResponse;
    procedure CheckResponse(const AResp: IHTTPResponse);
    procedure NeedClientCertificateCallback(const Sender: TObject;
      const ARequest: TURLRequest; const ACertificateList: TCertificateList;
      var AnIndex: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function GetImpegni: TObjectList<TImpegno>;
    function GetNominativi(const AIDImpegno: Integer; const ANominativo: string)
      : TObjectList<TAnagrafica>;
    function GetEventStats(const AIDImpegno: Integer): TStatistica;
    function GetGruppo(const AIDImpegno: Integer; const AIDAnagrafica: Integer)
      : TObjectList<TAnagrafica>;
    function DoCheckAction(const AIDImpegnoAnagrafica: Integer;
      const ADirection: TCheckDirection): Integer;
    function PostAnagrafica(const AAnagrafica: TAnagrafica;
      const AObjOwner: boolean = true): Integer;
  end;

  TCerimonieRPPTest = class(TInterfacedObject, ICerimonieApiRRP)
  public
    function GetImpegni: TObjectList<TImpegno>;
    function GetNominativi(const AIDImpegno: Integer; const ANominativo: string)
      : TObjectList<TAnagrafica>;
    function GetEventStats(const AIDImpegno: Integer): TStatistica;
    function GetGruppo(const AIDImpegno: Integer; const AIDAnagrafica: Integer)
      : TObjectList<TAnagrafica>;
    function DoCheckAction(const AIDImpegnoAnagrafica: Integer;
      const ADirection: TCheckDirection): Integer;
    function PostAnagrafica(const AAnagrafica: TAnagrafica;
      const AObjOwner: boolean = true): Integer;

  end;

implementation

uses
  System.SysUtils, DateUtils, REST.JSON, System.JSON,
  System.Classes, REST.Types,
  System.NetConsts, NetworkState, FactoryService;

const
  BASE_URL =
{$IFDEF MSWINDOWS}
    'http://pdrweb/restimpegnitest/api';
{$ELSE}
  'https://gm.quirinale.it/restimpegnitest/api';
{$ENDIF}
ENDPOINT_IMPEGNI = '/impegni';
ENDPOINT_NOMINATIVI = '/nominativi';
ENDPOINT_STATISTICHE = '/statistiche';
ENDPOINT_GRUPPO = '/gruppo';
ENDPOINT_INGRESSO = '/ingresso';
ENDPOINT_ANAGRAFICA = '/anagrafica';

{ TCerimonieRRP }

procedure TCerimonieRRP.CheckOnBeforeRequest;
var
  LSSID: string;
begin
  if not GetGlobalNetworkState.IsConnected then
    raise Exception.Create('Non sei connesso alla rete');
{$IF defined(ANDROID) or defined(IOS)}
  LSSID := GetGlobalNetworkState.CurrentSSID.Replace('"', '');
  if LSSID <> 'LanM' then
    raise Exception.Create('Rete non connessa a LanM');
{$ENDIF}
end;

procedure TCerimonieRRP.CheckResponse(const AResp: IHTTPResponse);
var
  LStr: string;
begin
  LStr := Format('%s : %s [ %s ]', [AResp.StatusCode.ToString, AResp.StatusText,
    AResp.ContentAsString]);
  if AResp.StatusCode >= 400 then
    raise Exception.CreateFmt('%s : %s [ %s ]', [AResp.StatusCode.ToString,
      AResp.StatusText, AResp.ContentAsString]);
end;

constructor TCerimonieRRP.Create;
begin
  inherited Create;
  FClient := THTTPClient.Create;
  FClient.ContentType := CONTENTTYPE_APPLICATION_JSON;
  FClient.OnNeedClientCertificate := NeedClientCertificateCallback;
  FSecurityService := GetSecurityService;
  FSecurityService.InitializeCertificate;
end;

destructor TCerimonieRRP.Destroy;
begin
  FClient.Free;
  inherited;
end;

function TCerimonieRRP.DoCheckAction(const AIDImpegnoAnagrafica: Integer;
  const ADirection: TCheckDirection): Integer;
var
  LFilter: TFilter;
  LResp: IHTTPResponse;
begin
  LFilter := TFilter.Create;
  try
    LFilter.Anagrafica := TAnagrafica.Create;
    LFilter.Anagrafica.IdAnagimpegni := AIDImpegnoAnagrafica;
    LFilter.Anagrafica.Entrata := Integer(ADirection);
    LFilter.Anagrafica.UsrEntrata := 'prova';
    LResp := DoInternalPost(BASE_URL + ENDPOINT_INGRESSO, LFilter);
    CheckResponse(LResp);
    Result := LResp.ContentAsString.ToInteger;
  finally
    LFilter.Free;
  end;
end;

function TCerimonieRRP.DoInternalGet(const AURL: string): IHTTPResponse;
var
  LReq: IHTTPRequest;
begin
  CheckOnBeforeRequest;
  LReq := FClient.GetRequest('GET', AURL);
  FSecurityService.SetupRequest(LReq);
  Result := FClient.Execute(LReq);
  CheckResponse(Result);
  if Result.ContentAsString.IsEmpty then
    raise Exception.CreateFmt
      ('Error: empty result by retrieving data from %s ', [AURL]);
end;

function TCerimonieRRP.DoInternalPost(const AURL: string; ABody: TObject)
  : IHTTPResponse;
var
  LStream: TStringStream;
  LReq: IHTTPRequest;
  LBodyJSONStr: string;
begin
  CheckOnBeforeRequest;
  LBodyJSONStr := Mapper.ObjectToJSONObjectString(ABody);
  LStream := TStringStream.Create(LBodyJSONStr);
  try
    // d.spinetti
    // è un fix momentaneo in quanto non si riesce a capire perché,
    // utilizzando il GetRequest con il verbo POST in http il server restituisce 500 internal server error
    {$IFDEF IOS}
    if AURL.Contains('https://') then
    begin
      LReq := FClient.GetRequest('POST', AURL);
      LReq.HeaderValue['content-type'] := CONTENTTYPE_APPLICATION_JSON;
      LReq.SourceStream := LStream;
      FSecurityService.SetupRequest(LReq);
      Result := FClient.Execute(LReq);//, LStream);
    end
    else
    {$ENDIF}
      Result := FClient.Post(AURL, LStream);
    CheckResponse(Result);
    if Result.ContentAsString.IsEmpty then
      raise Exception.CreateFmt
        ('Error: empty result by retrieving data from %s ', [AURL]);
  finally
    LStream.Free;
  end;
end;

function TCerimonieRRP.GetEventStats(const AIDImpegno: Integer): TStatistica;
var
  LResp: IHTTPResponse;
  LFilter: TFilter;
  LObj: TJSONObject;
begin
  LFilter := TFilter.Create;
  try
    LFilter.IdImpegno := AIDImpegno;
    LResp := DoInternalPost(BASE_URL + ENDPOINT_STATISTICHE, LFilter);
    LObj := TJSONObject.ParseJSONValue(LResp.ContentAsString) as TJSONObject;
    try
      Result := Mapper.JSONObjectToObject<TStatistica>(LObj);
    finally
      LObj.Free;
    end;
  finally
    LFilter.Free;
  end;
end;

function TCerimonieRRP.GetGruppo(const AIDImpegno, AIDAnagrafica: Integer)
  : TObjectList<TAnagrafica>;
var
  LResp: IHTTPResponse;
  LFilter: TFilter;
  LArray: TJSONArray;
begin
  LFilter := TFilter.Create;
  try
    LFilter.IdImpegno := AIDImpegno;
    LFilter.IdAnag := AIDAnagrafica;
    LResp := DoInternalPost(BASE_URL + ENDPOINT_GRUPPO, LFilter);
    LArray := TJSONObject.ParseJSONValue(LResp.ContentAsString) as TJSONArray;
    try
      Result := TObjectList<TAnagrafica>.Create;
      Mapper.JSONArrayToObjectList<TAnagrafica>(Result, LArray, false);
    finally
      LArray.Free;
    end;
  finally
    LFilter.Free;
  end;
end;

function TCerimonieRRP.GetImpegni: TObjectList<TImpegno>;
var
  LResp: IHTTPResponse;
  LArray: TJSONArray;
begin
  Result := TObjectList<TImpegno>.Create;
  LResp := DoInternalGet(BASE_URL + ENDPOINT_IMPEGNI);
  LArray := TJSONObject.ParseJSONValue(LResp.ContentAsString) as TJSONArray;
  try
    Mapper.JSONArrayToObjectList<TImpegno>(Result, LArray, false);
  finally
    LArray.Free;
  end;
end;

function TCerimonieRRP.GetNominativi(const AIDImpegno: Integer;
  const ANominativo: string): TObjectList<TAnagrafica>;
var
  LResp: IHTTPResponse;
  LArray: TJSONArray;
  LFilter: TFilter;
begin
  LFilter := TFilter.Create;
  try
    LFilter.IdImpegno := AIDImpegno;
    LFilter.Nominativo := ANominativo;
    LResp := DoInternalPost(BASE_URL + ENDPOINT_NOMINATIVI, LFilter);
    LArray := TJSONObject.ParseJSONValue(LResp.ContentAsString) as TJSONArray;
    try
      Result := Mapper.JSONArrayToObjectList<TAnagrafica>(LArray, false);
    finally
      LArray.Free;
    end;
  finally
    LFilter.Free;
  end;
end;

procedure TCerimonieRRP.NeedClientCertificateCallback(const Sender: TObject;
  const ARequest: TURLRequest; const ACertificateList: TCertificateList;
  var AnIndex: Integer);
begin
  AnIndex := 0;
end;

function TCerimonieRRP.PostAnagrafica(const AAnagrafica: TAnagrafica;
  const AObjOwner: boolean): Integer;
var
  LResp: IHTTPResponse;
  LFilter: TFilter;
begin
  LFilter := TFilter.Create;
  try
    LFilter.Anagrafica := AAnagrafica;
    LResp := DoInternalPost(BASE_URL + ENDPOINT_ANAGRAFICA, LFilter);
    CheckResponse(LResp);
    Result := LResp.ContentAsString.ToInteger;
  finally
    if not AObjOwner then
      LFilter.Anagrafica := nil;
    LFilter.Free;
  end;
end;

{ TCerimonieRPPTest }

function TCerimonieRPPTest.DoCheckAction(const AIDImpegnoAnagrafica: Integer;
  const ADirection: TCheckDirection): Integer;
begin
  Result := -1;
end;

function TCerimonieRPPTest.GetEventStats(const AIDImpegno: Integer)
  : TStatistica;
begin
  Result := nil;
end;

function TCerimonieRPPTest.GetGruppo(const AIDImpegno, AIDAnagrafica: Integer)
  : TObjectList<TAnagrafica>;
begin
  Result := nil;
end;

function TCerimonieRPPTest.GetImpegni: TObjectList<TImpegno>;
var
  LImpegno: TImpegno;
  I: Integer;
begin
  Randomize;
  Result := TObjectList<TImpegno>.Create;
  for I := 0 to 5 do
  begin
    LImpegno := TImpegno.Create;
    LImpegno.ID := I;
    LImpegno.Date := DateTimeToStr(IncDay(Now, -I));
    LImpegno.Time := TimeToStr(IncHour(Now, -I));
    LImpegno.Description := Format('Impegno di test %d', [I]);
    LImpegno.Attendees := Random(1000) + 1; // 1 to 1000
    Result.Add(LImpegno);
  end;

end;

function TCerimonieRPPTest.GetNominativi(const AIDImpegno: Integer;
  const ANominativo: string): TObjectList<TAnagrafica>;
begin
  Result := nil;
end;

function TCerimonieRPPTest.PostAnagrafica(const AAnagrafica: TAnagrafica;
  const AObjOwner: boolean): Integer;
begin
  Result := -1;
end;

{ TFilter }

destructor TFilter.Destroy;
begin
  FAnagrafica.Free;
  inherited;
end;

procedure TFilter.SetAnagrafica(const Value: TAnagrafica);
begin
  FAnagrafica := Value;
end;

procedure TFilter.SetIdAnag(const Value: Integer);
begin
  FIdAnag := Value;
end;

procedure TFilter.SetIdAnagimpegni(const Value: Integer);
begin
  FIdAnagimpegni := Value;
end;

procedure TFilter.SetIdImpegno(const Value: Integer);
begin
  FIdImpegno := Value;
end;

procedure TFilter.SetNominativo(const Value: string);
begin
  FNominativo := Value;
end;

end.
