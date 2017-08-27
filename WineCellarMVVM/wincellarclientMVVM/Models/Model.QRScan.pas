unit Model.QRScan;

interface

uses
  ZXing.ScanManager, FMX.Graphics, System.Generics.Collections,
  ZXing.DecodeHintType;

type

  IQRScanManager = interface
    ['{82DD5E90-6BD3-47D4-B2A8-343A9ABB89EB}']
    function Scan(const PBitmapForScan: TBitmap): string;
  end;

  TZXINGQRScan = class(TInterfacedObject, IQRScanManager)
  private
    FScanManager: TScanManager;
    FHints: TDictionary<TDecodeHintType, TObject>;
  public
    constructor Create;
    destructor Destroy; override;
    function Scan(const PBitmapForScan: TBitmap): string;
  end;

implementation

uses
  ZXing.BarcodeFormat, ZXing.ReadResult, System.SysUtils;

{ TZXINGQRScan }

constructor TZXINGQRScan.Create;
var
  LList: TList<TBarcodeFormat>;
begin
  FHints := TDictionary<TDecodeHintType, TObject>.Create;
  FHints.Add(ZXing.DecodeHintType.TRY_HARDER, nil);
  LList := TList<TBarcodeFormat>.Create;
  LList.Add(TBarcodeFormat.QR_CODE);
  FHints.Add(ZXing.DecodeHintType.POSSIBLE_FORMATS, LList);
  FScanManager := TScanManager.Create(TBarcodeFormat.QR_CODE, nil);
end;

destructor TZXINGQRScan.Destroy;
begin
  FScanManager.Free;
  FHints.Free;
  inherited;
end;

function TZXINGQRScan.Scan(const PBitmapForScan: TBitmap): string;
var
  LRead: TReadResult;
begin
  LRead := FScanManager.Scan(PBitmapForScan);
  try
    if not Assigned(LRead) then
      exit('');
    Result := LRead.text;
  finally
    LRead.Free;
  end;
end;

end.
