unit MainFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, Androidapi.JNIBridge, Androidapi.JNI.Embarcadero,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  Androidapi.JNI.AdMob, Androidapi.JNI.JavaTypes, FMX.Advertising,
  FMX.Controls.Presentation;

type

  TMyAdViewListener = class(TJavaLocal, JIAdListener)
  private
    FAD: JInterstitialAd;
  public
    constructor Create(AAD: JInterstitialAd);
    procedure onAdClosed; cdecl;
    procedure onAdFailedToLoad(errorCode: Integer); cdecl;
    procedure onAdLeftApplication; cdecl;
    procedure onAdOpened; cdecl;
    procedure onAdLoaded; cdecl;
  end;

  TForm2 = class(TForm)
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    LAdViewListener: TMyAdViewListener;
    FInterStitial: JInterstitialAd;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses FMX.Platform.Android, Androidapi.Helpers, FMX.Helpers.Android, Androidapi.JNI.PlayServices;

{$R *.fmx}

procedure TForm2.Button1Click(Sender: TObject);
var
  LADRequestBuilder: JAdRequest_Builder;
  LadRequest: JAdRequest;
begin
  LADRequestBuilder := TJAdRequest_Builder.Create;
  LADRequestBuilder.addTestDevice(MainActivity.getDeviceID);
  LadRequest := LADRequestBuilder.build();
  LAdViewListener := TMyAdViewListener.Create(FInterStitial);
  CallInUIThread(
    procedure
    begin
      FInterStitial.setAdListener(TJAdListenerAdapter.JavaClass.init
        (LAdViewListener));
      FInterStitial.loadAd(LadRequest);
    end);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FInterStitial := TJInterstitialAd.JavaClass.init(MainActivity);
  FInterStitial.setAdUnitId
    (StringToJString({your-publisher-code-goes-here}));
end;

{ TMyAdViewListener }

constructor TMyAdViewListener.Create(AAD: JInterstitialAd);
begin
  inherited Create;
  FAD := AAD;
end;

procedure TMyAdViewListener.onAdClosed;
begin

end;

procedure TMyAdViewListener.onAdFailedToLoad(errorCode: Integer);
begin

end;

procedure TMyAdViewListener.onAdLeftApplication;
begin

end;

procedure TMyAdViewListener.onAdLoaded;
begin
  FAD.show;
end;

procedure TMyAdViewListener.onAdOpened;
begin

end;

end.
