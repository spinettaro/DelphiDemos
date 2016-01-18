unit SaveStateFRM;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, FMX.Memo;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    GroupBox1: TGroupBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormSaveState(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses SaveStateHelper;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LoadFormState;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Log.d('Sono in destroy');
  SaveFormState;
end;

procedure TForm1.FormSaveState(Sender: TObject);
begin
  Log.d('Sono in FormSaveState');
  SaveFormState;
end;

end.
