unit RetryerTestU;

interface

uses
  DUnitX.TestFramework;

type

  [TestFixture]
  TRetryHelperTest = class(TObject)
  public
    // Sample Methods
    // Simple single Test
    [Test]
    procedure SimpleRetryTest;
  end;

implementation

uses
  RetryHelperU, System.SysUtils;

procedure TRetryHelperTest.SimpleRetryTest;
var
  LProc: TProc;
  LCount: Integer;
  LRetry: Integer;
begin
  LCount := 0;
  LRetry := 3;
  LProc := procedure
    begin
      Inc(LCount);
      if LCount < LRetry then
        raise Exception.Create('Error Message');
    end;
  TRetryHelper.DoRetry(LRetry, 0, LProc);
  Assert.AreEqual(LRetry, LCount);
end;

initialization

TDUnitX.RegisterTestFixture(TRetryHelperTest);

end.
