unit RetryHelperU;

interface

uses
  System.SysUtils;

type
  TRetryHelper = class(TObject)
    class procedure DoRetry(const ATimes: Integer; ADelayInMillis: Int64;
      AProc: TProc);
  end;

implementation

uses
  System.Classes;

{ TRetryer }

class procedure TRetryHelper.DoRetry(const ATimes: Integer;
  ADelayInMillis: Int64; AProc: TProc);
var
  LRetry: Integer;
begin
  LRetry := 0;
  while true do
  begin
    try
      Inc(LRetry);
      AProc();
      break;
    except
      on E: Exception do
      begin
        if LRetry = ATimes then
          raise E;
        if ADelayInMillis > 0 then
          TThread.Sleep(ADelayInMillis);
      end
    end;
  end;
end;

end.
