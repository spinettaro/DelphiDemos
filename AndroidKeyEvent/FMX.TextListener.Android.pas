unit FMX.TextListener.Android;

interface

uses
  System.SyncObjs, System.Generics.Collections, Androidapi.JNI.Embarcadero,
  Androidapi.JNIBridge, Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes, FMX.Forms, FMX.Types;

type
  TCustomAndroidTextListener = class(TJavaLocal, JFMXTextListener)
  private
    FForm: TCommonCustomForm;
    FLastStr: string;
  protected
    function TryAsKey(const aChar: Char; var Key: Word): Boolean;
  public
    constructor Create(aForm: TCommonCustomForm);
    procedure onComposingText(beginPosition: Integer;
      endPosition: Integer); cdecl;
    procedure onSkipKeyEvent(event: JKeyEvent); cdecl;
    procedure onTextUpdated(text: JCharSequence; position: Integer); cdecl;
  end;

procedure RegisterTextListener(const aForm: TCommonCustomForm);

implementation

uses
  Androidapi.Helpers, System.SysUtils, FMX.Platform.Android, System.Character,
  FMX.Helpers.Android, System.Math, System.UITypes, FMX.KeyMapping;

var
  FTextListener: TCustomAndroidTextListener;

procedure RegisterTextListener(const aForm: TCommonCustomForm);
var
  FFMXTxp: JFMXTextEditorProxy;
begin
  if Assigned(FTextListener) then
    exit;
  FTextListener := TCustomAndroidTextListener.Create(aForm);
  FFMXTxp := FMX.Platform.Android.MainActivity.getTextEditorProxy;
  // the code below should be equivalent to INPUT_ALPHABET
  // FFMXTxp.setInputType(TJInputType.JavaClass.TYPE_CLASS_TEXT or
  // TJInputType.JavaClass.TYPE_TEXT_VARIATION_VISIBLE_PASSWORD or
  // TJInputType.JavaClass.TYPE_TEXT_FLAG_NO_SUGGESTIONS);
  FFMXTxp.setInputType(TJFMXTextEditorProxy.JavaClass.INPUT_ALPHABET);
  FFMXTxp.addTextListener(FTextListener);
end;

procedure UnregisterTextListener;
var
  FFMXTxp: JFMXTextEditorProxy;
begin
  if not Assigned(FTextListener) then
    exit;
  FFMXTxp := FMX.Platform.Android.MainActivity.getTextEditorProxy;
  FFMXTxp.removeTextListener(FTextListener);
  FTextListener.Free;
end;

{ TCustomTextListener }

constructor TCustomAndroidTextListener.Create(aForm: TCommonCustomForm);
begin
  inherited Create;
  FForm := aForm;
end;

function TCustomAndroidTextListener.TryAsKey(const aChar: Char;
  var Key: Word): Boolean;
var
  LOrd: Word;
  LType: TUnicodeCategory;
begin
  LOrd := 0;
  LType := System.Character.TCharacter.GetUnicodeCategory(aChar);
  // lo spazio va calcolato a parte perché è presente un BUG in FForm.KeyDown(LKey, LChar, []);
  // in quanto il carattere di spazio lo trasforma in #0 impedendone l'invio
  case LType of
    TUnicodeCategory.ucControl:
      begin
        LOrd := Ord(aChar);
        if LOrd in [vkReturn, vkLineFeed] then
          LOrd := vkReturn;
      end;
    TUnicodeCategory.ucSpaceSeparator:
      LOrd := vkSpace;
  end;
  Result := LOrd > 0;
  if Result then
    Key := LOrd;
end;

procedure TCustomAndroidTextListener.onComposingText(beginPosition,
  endPosition: Integer);
begin
end;

procedure TCustomAndroidTextListener.onSkipKeyEvent(event: JKeyEvent);
begin
end;

procedure TCustomAndroidTextListener.onTextUpdated(text: JCharSequence;
  position: Integer);
var
  LStr: string;
  LKey: Word;
  LChar: Char;
  LPosition: Integer;
  LValidString: string;
begin
  LStr := JCharSequenceToStr(text);
  LKey := 0;
  try
    // no valid character
    if position = 0 then
      exit;
    // string on mobile are 0 based
    // the input string contain all characters pressed until this time. So if I pressed f then o then o is something like this "[3]foo"
    // I have to calculate the last digit character
    LPosition := 2 + ifthen(position = 0, 1, Trunc(Log10(position) + 1))
      + position;
    LValidString := LStr.Substring(LPosition - position);
    if FLastStr.Contains(LValidString) and
      ((FLastStr.Length - LValidString.Length) = 1) then
    begin
      // delete command was pressed
      LChar := #0;
      LKey := System.UITypes.vkBack;
    end
    else
      LChar := LStr[LPosition - 1];
    // I need to understand if the key is not a char but a key to respect the method signature.
    // See documentation for more details http://docwiki.embarcadero.com/Libraries/Berlin/en/FMX.Forms.TCommonCustomForm.KeyDown
    if TryAsKey(LChar, LKey) then
      LChar := #0;
    FForm.KeyDown(LKey, LChar, []);
  finally
    FLastStr := LValidString;
  end;
end;

end.
