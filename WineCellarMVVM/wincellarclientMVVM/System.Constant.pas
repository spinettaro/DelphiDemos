unit System.Constant;

interface

uses
  System.SysUtils;

type

  TEnviroment = (eTest, eProd);

  TWCException = class(Exception)

  end;

var
  Enviroment: TEnviroment = eProd;

implementation

end.
