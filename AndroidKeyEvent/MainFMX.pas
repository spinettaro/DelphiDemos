unit MainFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  FMX.TextListener.Android, FMX.Platform, FMX.VirtualKeyboard;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
  FService: IFMXVirtualKeyboardService;
begin
  TPlatformServices.Current.SupportsPlatformService(IFMXVirtualKeyboardService,
    IInterface(FService));
  if (FService <> nil) then
    FService.ShowVirtualKeyboard(Self);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RegisterTextListener(Self);
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
begin
  if KeyChar = #0 then
  begin
    if (Key = vkDelete) or (Key = vkBack) then
      Memo1.Lines.Text := Memo1.Lines.Text.Substring(0,
        Memo1.Lines.Text.Length - 1);
  end
  else
    Memo1.Lines.Text := Memo1.Lines.Text + KeyChar;
end;

end.
