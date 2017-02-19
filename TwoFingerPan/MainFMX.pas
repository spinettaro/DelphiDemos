unit MainFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Gestures, FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Platform,
  FMX.Ani, FMX.MultiView;

type
  TForm1 = class(TForm)
    Rectangle1: TRectangle;
    Image1: TImage;
    procedure FormShow(Sender: TObject);
    procedure FormTouch(Sender: TObject; const Touches: TTouches;
      const Action: TTouchAction);
  private
    FStartingPoint: TPointF;
    FDeviceScale: Single;
    ScreenSvc: IFMXScreenService;
    FScreenWidth: Single;
    FScreenHeight: Single;
    previousTranslateX: Single;
    startX: Single;
    previousTranslateY: Single;
    startY: Single;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormShow(Sender: TObject);
begin

  FStartingPoint := PointF(0, 0);
  if TPlatformServices.Current.SupportsPlatformService(IFMXScreenService,
    IInterface(ScreenSvc)) then
  begin
    FDeviceScale := ScreenSvc.GetScreenScale();
    FScreenWidth := ScreenSvc.GetScreenSize().X;
    FScreenHeight := ScreenSvc.GetScreenSize().Y;
  end;

  Rectangle1.Width := FScreenWidth * 2;
  Rectangle1.Height := FScreenHeight * 2;
  Rectangle1.Position.X := 0;
  Rectangle1.Position.Y := 0;

end;

procedure TForm1.FormTouch(Sender: TObject; const Touches: TTouches;
  const Action: TTouchAction);
begin
  if Length(Touches) <> 2 then
    exit;
  case Action of
    TTouchAction.None:
      ;
    TTouchAction.Up:
      begin
        previousTranslateX := Rectangle1.Position.X;
        previousTranslateY := Rectangle1.Position.Y;
      end;
    TTouchAction.Down:
      begin
        startX := Touches[1].Location.X - previousTranslateX;
        startY := Touches[1].Location.Y - previousTranslateY;
      end;
    TTouchAction.Move:
      begin
        Rectangle1.Position.X := Touches[1].Location.X - startX;
        Rectangle1.Position.Y := Touches[1].Location.Y - startY;
      end;
    TTouchAction.Cancel:
      ;
  end;

end;

end.
