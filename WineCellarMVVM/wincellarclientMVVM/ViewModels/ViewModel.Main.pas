unit ViewModel.Main;

interface

uses
  WinesBO, System.Constant, System.Generics.Collections,
  System.SyncObjs, FMX.Graphics, RESTServicesU;

type

  IWineViewModel = interface
    ['{842FC694-6DAF-4982-A26B-FC0DF2D5AF13}']
    procedure SetRESTService(const Value: IWineCellarModel);
    function GetRESTService: IWineCellarModel;
    procedure SetCurrentWines(const Value: TWines);
    function GetCurrentWines: TWines;
    procedure RequestWines;
    procedure SaveWine(const AWine: TWine; const AObjOwner: boolean = true);
    property RESTService: IWineCellarModel read GetRESTService
      write SetRESTService;
    property CurrentWines: TWines read GetCurrentWines write SetCurrentWines;
  end;

function CreateWineViewModel(const aEnviroment: TEnviroment = TEnviroment.eProd)
  : IWineViewModel;

implementation

uses
  JobQueue, EventBus, EventsU, System.SysUtils,
  System.UITypes, System.Rtti, System.DateUtils, FMX.Types, System.Classes;

type

  TMainViewModel = class(TInterfacedObject, IWineViewModel)
  private
    FModel: IWineCellarModel;
    FJobQueue: IJobQueue;
    FBackgroundEvent: Integer;
    FCurrentWines: TWines;
    function GetRESTService: IWineCellarModel;
    procedure SetRESTService(const Value: IWineCellarModel);
    procedure SetCurrentWines(const Value: TWines);
    function GetCurrentWines: TWines;
  protected
    procedure CommonPreRequestProc(const ANeedLoading: boolean = true);
    procedure CommonPostRequestProc(const ANeedLoading: boolean = true);
    procedure StartBackgroundJob(AProc: TProc; const PCS: TCriticalSection;
      const ALoadWaitView: boolean = true);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RequestWines;
    procedure SaveWine(const AWine: TWine; const AObjOwner: boolean = true);
    property RESTService: IWineCellarModel read GetRESTService
      write SetRESTService;
    property CurrentWines: TWines read GetCurrentWines write SetCurrentWines;
  end;

  { TMainViewModel }

procedure TMainViewModel.CommonPostRequestProc(const ANeedLoading
  : boolean = true);
begin
  if not ANeedLoading then
    exit;
  Dec(FBackgroundEvent);
  TEventBus.GetDefault.Post(TLoadingEvent.Create(FBackgroundEvent > 0));
end;

procedure TMainViewModel.CommonPreRequestProc(const ANeedLoading
  : boolean = true);
begin
  if not ANeedLoading then
    exit;
  Inc(FBackgroundEvent);
  TEventBus.GetDefault.Post(TLoadingEvent.Create(FBackgroundEvent > 0));
end;

constructor TMainViewModel.Create;
begin
  inherited Create;
  FJobQueue := TJobQueue.Create;
  FBackgroundEvent := 0;
end;

destructor TMainViewModel.Destroy;
begin
  inherited;
end;

function TMainViewModel.GetCurrentWines: TWines;
begin
  Result := FCurrentWines;
end;

function TMainViewModel.GetRESTService: IWineCellarModel;
begin
  Result := FModel;
end;

procedure TMainViewModel.SaveWine(const AWine: TWine;
  const AObjOwner: boolean = true);
begin
  StartBackgroundJob(
    procedure
    begin
      if AWine.id > 0 then
        FModel.UpdateWineById(AWine.id, AWine)
      else
        FModel.SaveWine(AWine);
      RequestWines;
    end, nil);
end;

procedure TMainViewModel.RequestWines;
begin
  StartBackgroundJob(
    procedure
    begin
      CurrentWines := FModel.GetWineList;
      TEventBus.GetDefault.Post(TWinesListEvent.Create);
    end, nil);
end;

procedure TMainViewModel.SetCurrentWines(const Value: TWines);
begin
  FCurrentWines := Value;
end;

procedure TMainViewModel.SetRESTService(const Value: IWineCellarModel);
begin
  FModel := Value;
end;

procedure TMainViewModel.StartBackgroundJob(AProc: TProc;
const PCS: TCriticalSection; const ALoadWaitView: boolean = true);
begin
  FJobQueue.AddJobInBackground(
    procedure
    begin
      if Assigned(PCS) then
        PCS.Acquire;
      try
        CommonPreRequestProc(ALoadWaitView);
        try
          // otherwise the calls are too fast :)
          TThread.Sleep(2000);
          AProc();
        except
          on E: TWCException do
            TEventBus.GetDefault.Post(TUINotificationEvent.Create(E.Message,
              TMsgDlgType.mtError));
          on E: Exception do
          begin
            Log.d('Error %s', [E.Message]);
            TEventBus.GetDefault.Post(TUINotificationEvent.Create
              ('An error occurs', TMsgDlgType.mtError));
          end
        end;
        CommonPostRequestProc(ALoadWaitView);
      finally
        if Assigned(PCS) then
          PCS.Release;
      end;
    end);
end;

// Simple factory for ViewModel
function CreateWineViewModel(const aEnviroment: TEnviroment): IWineViewModel;
begin
  case aEnviroment of
    eTest:
      Result := TMainViewModel.Create;
    eProd:
      Result := TMainViewModel.Create;
  end;
end;

end.
