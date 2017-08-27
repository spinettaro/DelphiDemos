unit Models.Validators;

interface

uses
  BOs, Validators.Engine;

type

  TAnagraficaInsertValidator = class(TInterfacedObject, IValidator<TAnagrafica>)
  public
    function Validate(aEntity: TAnagrafica): IValidationResult;
  end;

implementation

uses
  System.SysUtils;

{ TAnagraficaInsertValidator }

function TAnagraficaInsertValidator.Validate(aEntity: TAnagrafica)
  : IValidationResult;
var
  lIsValid: boolean;
begin
  Result := TValidationResult.Create;
  lIsValid := not(aEntity.Nome.IsEmpty or aEntity.Cognome.IsEmpty or
    aEntity.DataNascita.IsEmpty or aEntity.LuogoNascita.IsEmpty);
  if not lIsValid then
    Result.BrokenRules :=
      ['I campi nome, cognome, data e luogo nascita sono obbligatori '];
end;

initialization

// registro i validator nel container
TValidationEngine.ValidationContainer.RegisterValidatorFor<TAnagrafica>
  ('AnagraficaInsertValidation', TAnagraficaInsertValidator.Create);

finalization

end.
