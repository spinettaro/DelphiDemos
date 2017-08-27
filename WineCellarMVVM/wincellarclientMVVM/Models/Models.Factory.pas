unit Models.Factory;

interface

uses
  Model.REST.Cerimonie, Model.QRScan, System.Constant;

function CreateCerimonieApiRRP(const aEnviroment: TEnviroment = eProd)
  : ICerimonieApiRRP;

function CreateQRScanManager(const aEnviroment: TEnviroment = eProd)
  : IQRScanManager;

implementation

function CreateCerimonieApiRRP(const aEnviroment: TEnviroment = eProd)
  : ICerimonieApiRRP;
begin
  case aEnviroment of
    eTest:
      Result := TCerimonieRPPTest.Create;
    eProd:
      Result := TCerimonieRRP.Create;
  end;
end;

function CreateQRScanManager(const aEnviroment: TEnviroment = eProd)
  : IQRScanManager;
begin
  case aEnviroment of
    eTest:
      Result := TZXINGQRScan.Create;
    eProd:
      Result := TZXINGQRScan.Create;
  end;
end;

end.
