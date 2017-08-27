unit MainFMX;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TabControl,
  FMX.StdCtrls, FMX.Gestures, MVCFramework.RESTAdapter, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, Data.Bind.GenData,
  FMX.Bind.GenData, Data.Bind.EngExt, FMX.Bind.DBEngExt, System.Rtti,
  System.Bindings.Outputs, FMX.Bind.Editors, Data.Bind.Components,
  Data.Bind.ObjectScope, System.Actions, FMX.ActnList, FMX.ListBox, FMX.Layouts,
  FMX.MultiView, FMX.Edit, FMX.ListView, FMX.Controls.Presentation,
  ViewModel.Main,
  System.Generics.Collections, WinesBO,
  Data.Bind.Controls, FMX.Bind.Navigator, EventsU, EventBus.Attributes,
  EventBus.Commons;

type
  TTabbedForm = class(TForm)
    HeaderToolBar: TToolBar;
    ToolBarLabel: TLabel;
    TabControl1: TTabControl;
    WineListTabItem: TTabItem;
    EdtTabItem: TTabItem;
    BtnWineList: TButton;
    ActionList1: TActionList;
    acWineList: TAction;
    WineListView: TListView;
    MultiView1: TMultiView;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    DrawerBtn: TButton;
    PrototypeBindSource1: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    EdtName: TEdit;
    LblName: TLabel;
    LblCountry: TLabel;
    EdtCountry: TEdit;
    EdtRegion: TEdit;
    LblRegion: TLabel;
    LblYear: TLabel;
    EdtYear: TEdit;
    EdtGrapes: TEdit;
    LblGrapes: TLabel;
    LinkControlToField1: TLinkControlToField;
    LinkControlToField2: TLinkControlToField;
    LinkControlToField3: TLinkControlToField;
    LinkControlToField4: TLinkControlToField;
    LinkControlToField5: TLinkControlToField;
    LinkListControlToField1: TLinkListControlToField;
    ListBoxItem2: TListBoxItem;
    ChangeTabActionEdtWine: TChangeTabAction;
    ChangeTabActionWineList: TChangeTabAction;
    ToolBar1: TToolBar;
    Button1: TButton;
    acSaveWine: TAction;
    EdtID: TEdit;
    LinkControlToField6: TLinkControlToField;
    procedure FormCreate(Sender: TObject);
    procedure acWineListExecute(Sender: TObject);
    procedure PrototypeBindSource1CreateAdapter(Sender: TObject;
      var ABindSourceAdapter: TBindSourceAdapter);
    procedure WineListViewItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure ListBoxItem1Click(Sender: TObject);
    procedure ListBoxItem2Click(Sender: TObject);
    procedure acSaveWineExecute(Sender: TObject);
  private
    FWineViewModel: IWineViewModel;
    WinesAdapter: TListBindSourceAdapter<TWine>;
    procedure InitializeMVVM;
    { Private declarations }
  protected
    function GetWine: TWine;
  public
    { Public declarations }
    [Subscribe(TThreadMode.Main)]
    procedure OnWinesReady(AEvent: TWinesListEvent);
  end;

var
  TabbedForm: TTabbedForm;

implementation

uses Generics.Collections, RESTServicesU, EventBus;

{$R *.fmx}

procedure TTabbedForm.acSaveWineExecute(Sender: TObject);
var
  LWine: TWine;
begin
  LWine := GetWine;
  if LWine.id > 0 then
    FWineViewModel.PutWine(LWine)
  else
    FWineViewModel.PostWine(LWine);
end;

procedure TTabbedForm.acWineListExecute(Sender: TObject);
begin
  FWineViewModel.RequestWines;
end;

procedure TTabbedForm.FormCreate(Sender: TObject);
begin
  { This defines the default active tab at runtime }
  TabControl1.ActiveTab := WineListTabItem;
  PrototypeBindSource1.Active := true;
  InitializeMVVM;
end;

function TTabbedForm.GetWine: TWine;
var
  FWines: TObjectList<TWine>;
begin
  Result := TWine.Create;
  if not EdtID.Text.IsEmpty then
    Result.id := EdtID.Text.ToInteger;
  Result.name := EdtName.Text;
  Result.year := EdtYear.Text;
  Result.grapes := EdtGrapes.Text;
  Result.country := EdtCountry.Text;
  Result.region := EdtRegion.Text;
  FWines := TObjectList<TWine>(WinesAdapter.List);
  Result.description := FWines[PrototypeBindSource1.ItemIndex].description;
end;

procedure TTabbedForm.InitializeMVVM;
begin
  // MVVM
  FWineViewModel := CreateWineViewModel;
  FWineViewModel.RESTService := CreateWineRESTService;
  TEventBus.GetDefault.RegisterSubscriber(Self);
end;

procedure TTabbedForm.ListBoxItem1Click(Sender: TObject);
begin
  ChangeTabActionWineList.ExecuteTarget(Sender);
  MultiView1.HideMaster;
end;

procedure TTabbedForm.ListBoxItem2Click(Sender: TObject);
begin
  PrototypeBindSource1.Insert;
  ChangeTabActionEdtWine.ExecuteTarget(Sender);
  MultiView1.HideMaster;
end;

procedure TTabbedForm.OnWinesReady(AEvent: TWinesListEvent);
begin
  try
    WinesAdapter.Active := false;
    WinesAdapter.SetList(FWineViewModel.ActualWines);
    WinesAdapter.Active := true;
    TabControl1.GotoVisibleTab(WineListTabItem.Index);
  finally
    AEvent.Free;
  end;
end;

procedure TTabbedForm.PrototypeBindSource1CreateAdapter(Sender: TObject;
  var ABindSourceAdapter: TBindSourceAdapter);
begin
  WinesAdapter := TListBindSourceAdapter<TWine>.Create(PrototypeBindSource1);
  ABindSourceAdapter := WinesAdapter;
end;

procedure TTabbedForm.WineListViewItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
  ChangeTabActionEdtWine.ExecuteTarget(Sender);
end;

end.
