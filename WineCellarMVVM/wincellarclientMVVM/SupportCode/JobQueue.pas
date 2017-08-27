unit JobQueue;

interface

uses
  System.Threading, System.SysUtils;

type
  IJobQueue = interface
    ['{FE24933B-1C4A-4874-B6E5-DEC7123CE35A}']
    procedure AddJobInBackground(AJob: TProc; ASuccess: TProc = nil;
      AError: TProc = nil); overload;
  end;

  TJobQueue = class(TInterfacedObject, IJobQueue)
  private
    FThreadPool: TThreadPool;
  public
    constructor Create(AThreadPool: TThreadPool = nil);
    destructor Destroy; override;
    procedure AddJobInBackground(AJob: TProc; ASuccess: TProc = nil;
      AError: TProc = nil);
  end;

implementation

{ TJobQueue }

procedure TJobQueue.AddJobInBackground(AJob: TProc; ASuccess: TProc;
  AError: TProc);
begin
  FThreadPool.QueueWorkItem(
    procedure
    begin
      try
        AJob();
        if Assigned(ASuccess) then
          ASuccess();
      except
        if Assigned(AError) then
          AError();
      end
    end);
end;

constructor TJobQueue.Create(AThreadPool: TThreadPool);
begin
  inherited Create;
  if Assigned(AThreadPool) then
    FThreadPool := AThreadPool
  else
    FThreadPool := TThreadPool.Create;
end;

destructor TJobQueue.Destroy;
begin
  FThreadPool.Free;
  inherited;
end;

end.
