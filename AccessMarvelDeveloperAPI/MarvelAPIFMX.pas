unit MarvelAPIFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, ImgLoadThreadU,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  IPPeerClient, FMX.Layouts, FMX.Memo, REST.Client, Data.Bind.Components,
  Data.Bind.ObjectScope, FMX.Objects, FMX.Controls.Presentation;

type
  THeaderFooterForm = class(TForm)
    Header: TToolBar;
    Footer: TToolBar;
    HeaderLabel: TLabel;
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    RESTResponse1: TRESTResponse;
    btnNext: TButton;
    btnPrevious: TButton;
    ScaledLayout1: TScaledLayout;
    Text1: TText;
    Image1: TImage;
    FooterLayout: TLayout;
    AniIndicator1: TAniIndicator;
    Label1: TLabel;
    procedure RESTRequest1HTTPProtocolError(Sender: TCustomRESTRequest);
    procedure btnNextClick(Sender: TObject);
    procedure btnPreviousClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FOffSet: Integer;
    { Private declarations }
    procedure ExecuteRequest;
    procedure OnAfterRequest;
  protected
    procedure ToogleFooterLayout(const AToogle: boolean);
  public
    { Public declarations }
    procedure OnBitmapLoaded(ASender: TObject; ABitmap: TBitmap);
  end;

var
  HeaderFooterForm: THeaderFooterForm;

implementation

uses IdHashMessageDigest, idHash, System.JSON, DateUtils, ConstantU;

{$R *.fmx}

procedure THeaderFooterForm.btnNextClick(Sender: TObject);
begin
  FOffSet := FOffSet + 1;
  ExecuteRequest;
end;

procedure THeaderFooterForm.btnPreviousClick(Sender: TObject);
begin
  if FOffSet > 0 then
    FOffSet := FOffSet - 1;
  ExecuteRequest;
end;

procedure THeaderFooterForm.ExecuteRequest;
var
  TS: string;
  imd5: TIdHashMessageDigest;
  HASHStr: string;
begin
  // StartWait show a progress dialog until the request is finished
  ToogleFooterLayout(true);
  TS := IntToStr(DateTimeToUnix(Now));
  RESTRequest1.Params.ParameterByName('OFFSET').Value := FOffSet.ToString;
  RESTRequest1.Params.ParameterByName('TS').Value := TS;
  RESTRequest1.Params.ParameterByName('APIKEY').Value := PUBLIC_KEY;
  imd5 := TIdHashMessageDigest5.Create;
  try
    HASHStr := TS + PRIVATE_KEY + PUBLIC_KEY;
    RESTRequest1.Params.ParameterByName('HASH').Value :=
      imd5.HashStringAsHex(HASHStr).ToLower;
    RESTRequest1.ExecuteAsync(OnAfterRequest);
  finally
    imd5.Free;
  end;
end;

procedure THeaderFooterForm.FormShow(Sender: TObject);
begin
  ToogleFooterLayout(false);
  ExecuteRequest;
end;

procedure THeaderFooterForm.OnAfterRequest;
var
  RetObject: TJSONObject;
  RetData: TJSONObject;
  MResult: TJSONObject;
  Loader: TImageLoaderThread;
  Thumbnail: TJSONObject;
begin
  try
    RetObject := TJSONObject.ParseJSONValue(RESTResponse1.Content)
      as TJSONObject;
    RetData := RetObject.GetValue('data') as TJSONObject;
    MResult := (RetData.GetValue('results') as TJSONArray).Get(0)
      as TJSONObject;
    Thumbnail := MResult.GetValue('thumbnail') as TJSONObject;
    Text1.Text := MResult.GetValue('name').Value;
    Loader := TImageLoaderThread.Create(true, nil, OnBitmapLoaded);
    Loader.ImgURL := Thumbnail.GetValue('path').Value + '.' +
      Thumbnail.GetValue('extension').Value;
    Loader.Start;
  finally
    ToogleFooterLayout(false);
  end;
end;

procedure THeaderFooterForm.OnBitmapLoaded(ASender: TObject; ABitmap: TBitmap);
begin
  Image1.Bitmap.Assign(ABitmap);
end;

procedure THeaderFooterForm.RESTRequest1HTTPProtocolError
  (Sender: TCustomRESTRequest);
begin
  ToogleFooterLayout(false);
  ShowMessage(RESTResponse1.Content);
end;

procedure THeaderFooterForm.ToogleFooterLayout(const AToogle: boolean);
begin
  FooterLayout.Visible := AToogle;
  AniIndicator1.Enabled := AToogle;
end;

end.
