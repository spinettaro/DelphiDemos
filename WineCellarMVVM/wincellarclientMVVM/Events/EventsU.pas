unit EventsU;

interface

uses
  System.Generics.Collections, WinesBO, System.UITypes;

type

  TLoadingEvent = class(TObject)
  private
    FWait: Boolean;
    procedure SetWait(const Value: Boolean);
  public
    constructor Create(const AWait: Boolean);
    property Wait: Boolean read FWait write SetWait;
  end;

  TWinesListEvent = class(TObject)
  end;

  TNotificationType = (ntSuccess, ntError, ntWarning);

  TUINotificationEvent = class(TObject)
  private
    FText: string;
    FNType: TMsgDlgType;
    procedure SetText(const Value: string);
    procedure SetNType(const Value: TMsgDlgType);
  public
    constructor Create(const AText: string; const AType: TMsgDlgType);
    property Text: string read FText write SetText;
    property NType: TMsgDlgType read FNType write SetNType;
  end;

implementation

uses
  ObjectsMappers;

{ TUIErrorEvent }

constructor TUINotificationEvent.Create(const AText: string;
  const AType: TMsgDlgType);
begin
  inherited Create;
  FText := AText;
  FNType := AType;
end;

procedure TUINotificationEvent.SetNType(const Value: TMsgDlgType);
begin
  FNType := Value;
end;

procedure TUINotificationEvent.SetText(const Value: string);
begin
  FText := Value;
end;

{ TBackgroundEvent }

constructor TLoadingEvent.Create(const AWait: Boolean);
begin
  inherited Create;
  FWait := AWait;
end;

procedure TLoadingEvent.SetWait(const Value: Boolean);
begin
  FWait := Value;
end;

end.
