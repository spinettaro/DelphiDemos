unit ImgLoadThreadU;

interface

uses
  System.Classes,
  FMX.Graphics;

type
  TNotifyBitmapLoaded = procedure(Sender: TObject; Bitmap: TBitmap) of object;

  TImageLoaderThread = class(TThread)
  private
    { Private declarations }
    FNotifyWhenFinished: TNotifyEvent;
    FNotifyBitmapLoaded: TNotifyBitmapLoaded;
    FBitmap: TBitmap;
    FImgURL: string;
    procedure SetImgURL(const Value: string);

  protected
    procedure SendNotifyFinished;
    procedure SendNotifyBitmapLoaded;
    procedure Execute; override;
    destructor Destroy; override;

  public
    constructor Create(Suspended: boolean; NotifyWhenFinished: TNotifyEvent;
      NotifyBitmapLoaded: TNotifyBitmapLoaded); virtual;
    property ImgURL: string read FImgURL write SetImgURL;
  end;

implementation

uses IdBaseComponent,
  IdComponent,
  IdTCPConnection,
  IdTCPClient,
  IdHTTP,
  System.IOUtils,
  System.Generics.collections,
  System.SysUtils;

{ TImageLoaderThread }

constructor TImageLoaderThread.Create(Suspended: boolean;
  NotifyWhenFinished: TNotifyEvent; NotifyBitmapLoaded: TNotifyBitmapLoaded);
begin
  FNotifyWhenFinished := NotifyWhenFinished;
  FNotifyBitmapLoaded := NotifyBitmapLoaded;
  FBitmap := TBitmap.Create;
  inherited Create(Suspended);
end;

destructor TImageLoaderThread.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TImageLoaderThread.Execute;
var
  LStream: TStream;
  LHttp: TIdHTTP;
  I: Integer;
begin
  LStream := nil;
  LHttp := nil;

  try
    LStream := TMemoryStream.Create;
    LHttp := TIdHTTP.Create(nil);

    LStream.Position := 0;
    LHttp.Get(ImgURL, LStream);

    FBitmap.LoadFromStream(LStream);

    Synchronize(SendNotifyBitmapLoaded);
    Synchronize(SendNotifyFinished);

  finally
    FreeAndNil(LHttp);
    FreeAndNil(LStream);
  end;
end;

procedure TImageLoaderThread.SendNotifyBitmapLoaded;
begin
  if Assigned(FNotifyBitmapLoaded) then
  begin
    FNotifyBitmapLoaded(self, FBitmap);
  end;
end;

procedure TImageLoaderThread.SendNotifyFinished;
begin
  if Assigned(FNotifyWhenFinished) then
    FNotifyWhenFinished(self);
end;

procedure TImageLoaderThread.SetImgURL(const Value: string);
begin
  FImgURL := Value;
end;

end.
