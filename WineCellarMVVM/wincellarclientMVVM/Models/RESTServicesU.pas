unit RESTServicesU;

interface

uses
  Generics.Collections, WinesBO,
  ObjectsMappers, MVCFramework.RESTClient,
  MVCFramework.RESTAdapter, MVCFramework.Commons;

type

  IWineCellarModel = interface(IInvokable)
    ['{3407C9CD-9425-48CB-81B9-55731652C1D1}']

    [RESTResource(HttpGet, '/wines')]
    [MapperListOf(TWine)]
    [Mapping(TWines)]
    function GetWineList: TWines;

    [RESTResource(httpPOST, '/wines')]
    procedure SaveWine([Body] AWine: TWine);

    [RESTResource(httpPUT, '/wines/{id}')]
    procedure UpdateWineById([Param('id')] AID: integer; [Body] AWine: TWine);

  end;

function CreateWineRESTService: IWineCellarModel;

implementation

function CreateWineRESTService: IWineCellarModel;
var
  RESTAdapter: TRESTAdapter<IWineCellarModel>;
begin
  RESTAdapter := TRESTAdapter<IWineCellarModel>.Create;
  Result := RESTAdapter.Build('localhost', 3000);
end;

end.
