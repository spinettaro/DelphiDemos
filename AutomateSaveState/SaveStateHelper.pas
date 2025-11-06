{ *******************************************************************************
  Copyright 2015 Daniele Spinetti

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
  ******************************************************************************** }

unit SaveStateHelper;

interface

uses
  FMX.Forms;

type

  TSaveStateHelper = class helper for TCustomForm
  public const
    DATA_FIELD = 'data';
    TAG = 'tag';
    TAG_FLOAT = 'tagfloat';
    TAG_STRING = 'tagstring';
    procedure SaveFormState;
    procedure LoadFormState;
  end;

implementation

uses System.JSON, FMX.Types, System.Classes, System.SysUtils, IOUtils,
  REST.JSON;

{ TSaveStateHelper }

procedure TSaveStateHelper.LoadFormState;
var
  R: TBinaryReader;
  FormJSONObject: TJSONObject;
  I: Integer;
  FMXObj: TFmxObject;
  FMXJObj: TJSONObject;
begin
  try
    SaveState.StoragePath := TPath.GetHomePath;
    if not(SaveState.Stream.Size > 0) then
      exit;
    // Recover previously typed values in all control.
    R := TBinaryReader.Create(SaveState.Stream);
    try
      FormJSONObject := TJSONObject.ParseJSONValue(R.ReadString) as TJSONObject;
      try
        for I := 0 to Self.ComponentCount - 1 do
        begin
          if (Self.Components[I].Name = '') or not(Self.Components[I] is TFmxObject) then
            continue;
          FMXObj := Self.Components[I] as TFmxObject;
          FMXJObj := FormJSONObject.Values[FMXObj.Name] as TJSONObject;
          if not Assigned(FMXJObj) then
            Continue;

          case FMXObj.Data.Kind of
            tkUnknown:
              ;
            tkInteger:
              FMXObj.Data :=
                (FMXJObj.GetValue(DATA_FIELD) as TJSONNumber).AsInt;
            tkChar:
              ;
            tkEnumeration:
              if FMXJObj.GetValue(DATA_FIELD) is TJSONTrue then
                FMXObj.Data := true
              else
                FMXObj.Data := false;
            tkFloat:
              FMXObj.Data :=
                (FMXJObj.GetValue(DATA_FIELD) as TJSONNumber).AsDouble;
            tkString, tkUString, tkLString, tkWString:
              FMXObj.Data :=
                (FMXJObj.GetValue(DATA_FIELD) as TJSONString).Value;
            tkSet:
              ;
            tkClass:
              ;
            tkMethod:
              ;
            tkWChar:
              ;
            tkVariant:
              ;
            tkArray:
              ;
            tkRecord:
              ;
            tkInterface:
              ;
            tkInt64:
              FMXObj.Data :=
                (FMXJObj.GetValue(DATA_FIELD) as TJSONNumber).AsInt64;
            tkDynArray:
              ;
            tkClassRef:
              ;
            tkPointer:
              ;
            tkProcedure:
              ;
          end;

          // Tags
          FMXJObj := FormJSONObject.Values[FMXObj.Name + TAG] as TJSONObject;
          if Assigned(FMXJObj) then
            FMXObj.Tag := (FMXJObj.GetValue(TAG) as TJSONNumber).AsInt;
          FMXJObj := FormJSONObject.Values[FMXObj.Name + TAG_FLOAT] as TJSONObject;
          if Assigned(FMXJObj) then
            FMXObj.TagFloat := (FMXJObj.GetValue(TAG_FLOAT) as TJSONNumber).AsDouble;
          FMXJObj := FormJSONObject.Values[FMXObj.Name + TAG_STRING] as TJSONObject;
          if Assigned(FMXJObj) then
            FMXObj.TagString := (FMXJObj.GetValue(TAG_STRING) as TJSONString).Value;
        end;
      finally
        FormJSONObject.Free;
      end;
    finally
      R.Free;
    end;
  except
    on e: Exception do
      Log.d('SaveStateHelper', Self, e.Message);
  end;
end;

procedure TSaveStateHelper.SaveFormState;
var
  FormJSONObject: TJSONObject;
  I: Integer;
  FMXObj: TFmxObject;
  FMXJObj: TJSONObject;
  W: TBinaryWriter;
begin
  try
    FormJSONObject := TJSONObject.Create;
    try
      for I := 0 to Self.ComponentCount - 1 do
      begin
        if (Self.Components[I].Name = '') or not(Self.Components[I] is TFmxObject) then
          continue;
        FMXObj := Self.Components[I] as TFmxObject;
        FMXJObj := TJSONObject.Create;
        case FMXObj.Data.Kind of
          tkUnknown:
            ;
          tkInteger:
            FMXJObj.AddPair(DATA_FIELD,
              TJSONNumber.Create(FMXObj.Data.AsInteger));
          tkChar:
            ;
          tkEnumeration:
            if FMXObj.Data.AsBoolean then
              FMXJObj.AddPair(DATA_FIELD, TJSONTrue.Create)
            else
              FMXJObj.AddPair(DATA_FIELD, TJSONFalse.Create);
          tkFloat:
            FMXJObj.AddPair(DATA_FIELD,
              TJSONNumber.Create(FMXObj.Data.AsExtended));
          tkString, tkUString, tkLString, tkWString:
            FMXJObj.AddPair(DATA_FIELD, FMXObj.Data.AsString);
          tkSet:
            ;
          tkClass:
            ;
          tkMethod:
            ;
          tkWChar:
            ;
          tkVariant:
            ;
          tkArray:
            ;
          tkRecord:
            ;
          tkInterface:
            ;
          tkInt64:
            FMXJObj.AddPair(DATA_FIELD,
              TJSONNumber.Create(FMXObj.Data.AsInt64));
          tkDynArray:
            ;
          tkClassRef:
            ;
          tkPointer:
            ;
          tkProcedure:
            ;
        end;
        FormJSONObject.AddPair(FMXObj.Name, FMXJObj);

        // Tags
        FMXJObj := TJSONObject.Create;
        FMXJObj.AddPair(TAG,
          TJSONNumber.Create(FMXObj.Tag));
        FormJSONObject.AddPair(FMXObj.Name + TAG, FMXJObj);

        FMXJObj := TJSONObject.Create;
        FMXJObj.AddPair(TAG_FLOAT,
          TJSONNumber.Create(FMXObj.TagFloat));
        FormJSONObject.AddPair(FMXObj.Name + TAG_FLOAT, FMXJObj);

        FMXJObj := TJSONObject.Create;
        FMXJObj.AddPair(TAG_STRING, FMXObj.TagString);
        FormJSONObject.AddPair(FMXObj.Name + TAG_STRING, FMXJObj);

      end;
      SaveState.Stream.Clear;
      W := TBinaryWriter.Create(SaveState.Stream);
      try
        W.Write(FormJSONObject.ToJSON);
      finally
        W.Free;
      end;
    finally
      FormJSONObject.Free;
    end;
  except
    on e: Exception do
      Log.d('SaveStateHelper', Self, e.Message);
  end;
end;

end.
