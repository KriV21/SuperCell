unit OMB.Cells.EditorFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  OMB.Cells, FMX.Layouts, FMX.Colors, FMX.ListBox, FMX.Controls.Presentation,
  FMX.Edit, FMX.Objects, FMX.TreeView, FMX.ScrollBox, FMX.Memo;

type
  TOMBEditor = class(TFrame)
    Layout1: TLayout;
    Splitter1: TSplitter;
    Layout2: TLayout;
    ombCellView: TOMBCell;
    Layout3: TLayout;
    Splitter2: TSplitter;
    Layout4: TLayout;
    Rectangle1: TRectangle;
    TreeView1: TTreeView;
    lbProperties: TListBox;
    lbiAlign: TListBoxItem;
    Text1: TText;
    lbiWidth: TListBoxItem;
    Text2: TText;
    edWidth: TEdit;
    cbAlign: TComboBox;
    lbiHeight: TListBoxItem;
    Text3: TText;
    edHeight: TEdit;
    lbiColor: TListBoxItem;
    Text4: TText;
    cbColor: TComboColorBox;
    lbiMarginLeft: TListBoxItem;
    Text5: TText;
    edMarginLeft: TEdit;
    lbiMarginTop: TListBoxItem;
    Text6: TText;
    edMarginTop: TEdit;
    lbiMarginRight: TListBoxItem;
    Text7: TText;
    edMarginRight: TEdit;
    lbiMarginBottom: TListBoxItem;
    Text8: TText;
    edMarginBottom: TEdit;
    Rectangle2: TRectangle;
    lbiPaddingLeft: TListBoxItem;
    Text9: TText;
    edPaddingLeft: TEdit;
    lbiPaddingTop: TListBoxItem;
    Text10: TText;
    edPaddingTop: TEdit;
    lbiPaddingRignt: TListBoxItem;
    Text11: TText;
    edPaddingRight: TEdit;
    lbiPaddingBottom: TListBoxItem;
    Text12: TText;
    edPaddingBottom: TEdit;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    lbiName: TListBoxItem;
    Text13: TText;
    edDataName: TEdit;
  private
    { Private declarations }
  protected
    procedure SetProperties(Element : TOMBElement);
  public
    constructor Create(AOwner : TComponent); override;
    procedure Load(const Scheme : String);
    procedure ChangeSelected(Sender : TObject);
    { Public declarations }
  end;

implementation

{$R *.fmx}

{ TOBMEditor }

procedure TOMBEditor.ChangeSelected(Sender: TObject);
begin
  SetProperties(Sender as TOMBElement);
end;

constructor TOMBEditor.Create(AOwner: TComponent);
begin
  inherited;
  ombCellView.OnChangeSelected := ChangeSelected;
end;

procedure TOMBEditor.Load(const Scheme: String);
begin
  ombCellView.EditMode := True;
  ombCellView.Load(Scheme);
end;

procedure TOMBEditor.SetProperties(Element: TOMBElement);
begin
  edWidth.Text := Element.Width.Str;
  edHeight.Text := Element.Height.Str;
  edDataName.Text := Element.Name;
  edMarginTop.Text := Element.Margins.Top.Str;
  edMarginLeft.Text := Element.Margins.Left.Str;
  edMarginRight.Text := Element.Margins.Right.Str;
  edMarginBottom.Text := Element.Margins.Bottom.Str;
  edPaddingLeft.Text := Element.Padding.Left.Str;
  edPaddingTop.Text := Element.Padding.Top.Str;
  edPaddingRight.Text := Element.Padding.Right.Str;
  edPaddingBottom.Text := Element.Padding.Bottom.Str;
end;

end.
