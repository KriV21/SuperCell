unit OMB.Cells;

interface

uses Sysutils, Classes, FMX.Controls, FMX.Graphics, FMX.Types, System.TypInfo,
  System.Generics.Collections, System.Types, System.Rtti, Variants, System.NetEncoding,
  System.UITypes, Xml.XMLDoc, Xml.XMLIntf, Xml.xmldom, FMX.Objects,
  System.Diagnostics, FMX.ani, FMX.Utils, FMX.ImgList;

type
  TOMBElemetAlign = (Left, Right, Top, Bottom, Center, Client, Contents);
//  TOMBImageWrapMode = (Origin, Center, );

  TOMBProperty = record
    Str : string;
    class operator Implicit(const Value: string): TOMBProperty;
  end;

  TOMBPropertyHelper = record helper for TOMBProperty
    procedure Split(var S1,S2 : String);
    function IsPercent : Boolean;
    function Value : Single;
    function Calc(CurrentValue : Single) : Single;
  end;

  TOMBElement = class;
  TOMBCell = class;

  TOMBObject = class
  private
    FName: String;
    procedure SetName(const Value: String);

    procedure AssingName;
    class function GetNodeName : String;
  protected
    function GetStructure: String; virtual;
    procedure SetStructure(const Value: String); virtual;

    procedure SaveToNode(aXMLNode :  IXMLNode); virtual;
    procedure LoadFromNode(aXMLNode :  IXMLNode); virtual;
    function IsNeedSave : Boolean; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Save(Doc : IXMLDocument);
    class function Load(Doc : IXMLDocument) : TOMBObject; overload;
    class function Load(Doc : String) : TOMBObject; overload;
    property Structure: String read GetStructure write SetStructure;
    property Name : String read FName write SetName;
  end;

  TOMBClass = class of TOMBObject;

  TOMBMargins = class(TOMBObject)
  private
    FRight: TOMBProperty;
    FBottom: TOMBProperty;
    FTop: TOMBProperty;
    FLeft: TOMBProperty;
    procedure SetBottom(const Value: TOMBProperty);
    procedure SetLeft(const Value: TOMBProperty);
    procedure SetRight(const Value: TOMBProperty);
    procedure SetTop(const Value: TOMBProperty);
  private const
    str_Left    = 'left';
    str_Top     = 'top';
    str_Right   = 'right';
    str_Bottom  = 'bottom';
    procedure SaveToNode(aXMLNode :  IXMLNode); override;
    procedure LoadFromNode(aXMLNode :  IXMLNode); override;
    function IsNeedSave : Boolean; override;
  protected
    [weak] FParent: TOMBElement;
    procedure Changed;
  public
    constructor Create; override;
    procedure SetValues(const aLeft, aRight, aTop, aBottom : String);
    property Left : TOMBProperty read FLeft write SetLeft;
    property Top : TOMBProperty read FTop write SetTop;
    property Right : TOMBProperty read FRight write SetRight;
    property Bottom : TOMBProperty read FBottom write SetBottom;
  end;

  TOMBFont = class(TOMBObject)
  private const
    str_Family    = 'family';
    str_Size     = 'size';
    str_Color   = 'color';
    str_Bold  = 'bold';
    str_Underline  = 'underline';
    str_Italic  = 'italic';
    str_StrikeOut  = 'strikeout';
  private
    FUnderline: Boolean;
    FColor: TAlphaColor;
    FFamily: String;
    FSize: Single;
    FItalic: Boolean;
    FStrikeOut: Boolean;
    FBold: Boolean;
    procedure SetBold(const Value: Boolean);
    procedure SetColor(const Value: TAlphaColor);
    procedure SetFamily(const Value: String);
    procedure SetItalic(const Value: Boolean);
    procedure SetSize(const Value: Single);
    procedure SetStrikeOut(const Value: Boolean);
    procedure SetUnderline(const Value: Boolean);
    procedure SaveToNode(aXMLNode :  IXMLNode); override;
    procedure LoadFromNode(aXMLNode :  IXMLNode); override;
    function IsNeedSave : Boolean; override;
  protected
    FFont : TFont;
    function Font : TFont;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Family : String read FFamily write SetFamily;
    property Size : Single read FSize write SetSize;
    property Color : TAlphaColor read FColor write SetColor;
    property Bold : Boolean read FBold write SetBold;
    property Underline : Boolean read FUnderline write SetUnderline;
    property Italic : Boolean read FItalic write SetItalic;
    property StrikeOut : Boolean read FStrikeOut write SetStrikeOut;
  end;

  TOMBElementsList = class(TObjectList<TOMBElement>)
  end;

  TOMBElement = class(TOMBObject)
  private const
    str_Width   = 'width';
    str_Height  = 'height';
    str_Margins = 'margins';
    str_Padding = 'padding';
    str_DataName = 'dataname';
    str_Chilgs = 'childs';
    str_Align = 'align';
    str_Opacity = 'opacity';
    str_Visible = 'visible';
    str_BgColor = 'bcolor';
  private
    FAlign: TOMBElemetAlign;
    [weak] FCell: TOMBCell;
    [weak] FParent: TOMBElement;
    FMargins: TOMBMargins;
    FVisible: Boolean;
    FBodyColor: TAlphaColor;
    FOpacity: Single;
    FPadding: TOMBMargins;
    FWidth: TOMBProperty;
    FHeight: TOMBProperty;

    procedure InternalAlign;
    procedure CalcOriginPlace;
    procedure PaintDesignTime(Canvas : TCanvas);
    procedure PaintEditTime(Canvas : TCanvas);
    procedure PaintEditSelectedTime(Canvas : TCanvas);

    procedure SetAlign(const Value: TOMBElemetAlign);
    procedure SetCell(const Value: TOMBCell);
    procedure SetMargins(const Value: TOMBMargins);
    procedure SetParent(const Value: TOMBElement);
    procedure SetVisible(const Value: Boolean);
    procedure SetBodyColor(const Value: TAlphaColor);
    procedure SetOpacity(const Value: Single);
    procedure SetPadding(const Value: TOMBMargins);
    procedure SetHeight(const Value: TOMBProperty);
    procedure SetWidth(const Value: TOMBProperty);
  protected
    State: TCanvasSaveState;
    CalcPlace, OwnerPlace, CalcMargins, CalcPadding :  TRectF;
    Childs : TOMBElementsList;
    procedure Resize(aBounds : TRectF); virtual;
    procedure Paint(Canvas : TCanvas);
    procedure InternalPaint(Canvas : TCanvas); virtual;
    function IsDesignMode : Boolean;
    function IsEditMode : Boolean;
    function IsSelected : Boolean;

    procedure MarginsChanged;
    function FullHeight : Single;
    function FullWidth : Single;

    function AbsoluteOpacity : Single;
    function GetStructure: String; override;
    procedure SetStructure(const Value: String); override;
    procedure SaveToNode(aXMLNode :  IXMLNode); override;
    procedure LoadFromNode(aXMLNode :  IXMLNode); override;

    function GetData(const aKey : String; out aData : TValue) : Boolean;
    procedure ApplyData;
    procedure SetData(const Value : TValue); virtual;
    function FindElement(const aName : String) : TOMBElement;
    procedure SetPropertyValue(const SS : TArray<String>; const aValue : TValue);
    function ActiveCell : TOMBCell;
    function FindElementAt(X, Y : Single) : TOMBElement;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Cell :  TOMBCell read FCell write SetCell;
    property Parent : TOMBElement read FParent write SetParent;
    property Align : TOMBElemetAlign read FAlign write SetAlign;

    property Width : TOMBProperty read FWidth write SetWidth;
    property Height : TOMBProperty read FHeight write SetHeight;

    property Margins : TOMBMargins read FMargins write SetMargins;
    property Padding : TOMBMargins read FPadding write SetPadding;

    property Visible : Boolean read FVisible write SetVisible;
    property BodyColor : TAlphaColor read FBodyColor write SetBodyColor;
    property Opacity : Single read FOpacity write SetOpacity;
  end;

  TOMBText = class(TOMBElement)
  private const
    str_Fornt = 'font';
    str_Text = 'text';
  private
    FFont: TOMBFont;
    FHorzAlign: TTextAlign;
    FWordWrap: Boolean;
    FVertAlign: TTextAlign;
    FText: String;
    procedure SetFont(const Value: TOMBFont);
    procedure SetHorzAlign(const Value: TTextAlign);
    procedure SetVertAlign(const Value: TTextAlign);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetText(const Value: String);
  protected
    procedure InternalPaint(Canvas : TCanvas); override;
    procedure SaveToNode(aXMLNode :  IXMLNode); override;
    procedure LoadFromNode(aXMLNode :  IXMLNode); override;
    procedure SetData(const Value : TValue); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Font : TOMBFont read FFont write SetFont;
    property WordWrap : Boolean read FWordWrap write SetWordWrap;
    property VertAlign : TTextAlign read FVertAlign write SetVertAlign;
    property HorzAlign : TTextAlign read FHorzAlign write SetHorzAlign;
    property Text : String read FText write SetText;
  end;

  TOMBLayout = class(TOMBElement)

  end;

  TOMBImage = class(TOMBElement)
  private const
    str_WrapMode = 'mode';
  private
    FImage: TBitmap;
    FWrapMode: TImageWrapMode;
    procedure SetImage(const Value: TBitmap); virtual;
    procedure SetWrapMode(const Value: TImageWrapMode);
  protected
    procedure InternalPaint(Canvas : TCanvas); override;
    procedure DrawBitmap(const Canvas: TCanvas; const ARect: TRectF; const ABitmap: TBitmap; const AOpacity: Single = 1.0);
    procedure SaveToNode(aXMLNode :  IXMLNode); override;
    procedure LoadFromNode(aXMLNode :  IXMLNode); override;
    procedure SetData(const Value : TValue); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Image : TBitmap read FImage write SetImage;
    property WrapMode : TImageWrapMode read FWrapMode write SetWrapMode;
  end;

  TOMBCell = class (TControl)
  private
    FPlan: TStringList;
    FCell: TOMBElement;
    FImages: TImageList;
    FEditMode: Boolean;
    FSelected: TOMBElement;
    FOnChangeSelected: TNotifyEvent;
    procedure SetPlan(const Value: TStringList);
    procedure SetCell(const Value: TOMBElement);
    function GetData(Index: String): TValue;
    procedure SetData(Index: String; const Value: TValue);
    procedure SetImages(const Value: TImageList);
    procedure SetEditMode(const Value: Boolean);
    procedure SetSelected(const Value: TOMBElement);
    procedure SetOnChangeSelected(const Value: TNotifyEvent);
//    procedure PlanChanged(Sender : TObject);
  protected
    FDataUpdateted : Boolean;
    FData : TDictionary<String,TValue>;
    procedure Paint; override;
    procedure Resize; override;
    procedure ApplyData;
    function FindElement(const aName : String) :TOMBElement;
    function FindElementAt(X, Y : Single) : TOMBElement;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState;
      X, Y: Single); override;
    procedure ChangeSelected;
  public
    tm_lastPaint, tm_LastResize : Int64;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Cell : TOMBElement read FCell write SetCell;
    property Data[Index : String] : TValue read GetData write SetData;
    procedure Load(const Scheme : String);

    property EditMode : Boolean read FEditMode write SetEditMode;
    property Selected: TOMBElement read FSelected write SetSelected;
    property OnChangeSelected : TNotifyEvent read FOnChangeSelected write SetOnChangeSelected;
  published
    property Images : TImageList read FImages write SetImages;
    property Align;
    property Anchors;
    property ClipChildren;
    property ClipParent;
    property Cursor;
    property DragMode;
    property EnableDragHighlight;
    property Enabled;
    property Locked;
    property Height;
    property HitTest default False;
    property Padding;
    property Opacity;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property TouchTargetExpansion;
    property Visible;
    property Width;
    property TabOrder;
    property TabStop;
    { Events }
    property OnPainting;
    property OnPaint;
    property OnResize;
    { Drag and Drop events }
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    { Mouse events }
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

procedure RegisterOMBClass(aClass : TOMBClass);
procedure UnRegisterOMBClass(aClass : TOMBClass);

procedure Register;

implementation

const
  str_ClassName = 'class';
  str_Name = 'name';

type
  TOMBClassCollection = class(TList<TOMBClass>)
  end;

var
  LClassList : TOMBClassCollection;
  OMBObjects  : TDictionary<String, TOMBObject>;

procedure Register;
begin
  RegisterComponents('OMB', [TOMBCell]);
end;

procedure RegisterOMBClass(aClass : TOMBClass);
begin
  if LClassList.IndexOf(aClass)=-1 then
    LClassList.Add(aClass);
end;

procedure UnRegisterOMBClass(aClass : TOMBClass);
begin
  LClassList.Remove(aClass);
end;

function FindClass(const aName : String) : TOMBClass;
var
  Cl : TOMBClass;
begin
  Result := nil;
  for Cl in LClassList do
  begin
    if Cl.GetNodeName.Equals(aName) then
    begin
      Result := Cl;
      Break;
    end;
  end;
end;


function GetAttr(Node : IXMLNode; const aName : String; Def : OleVariant) : OleVariant;
begin
  Result := Def;
  if Node.HasAttribute(aName) then
    Result := Node.Attributes[aName];
end;

{ TOMBMargins }

procedure TOMBMargins.Changed;
begin

end;

constructor TOMBMargins.Create;
begin
  inherited;
  FParent := nil;
  FRight.Str := '0px';
  FBottom.Str := '0px';
  FTop.Str := '0px';
  FLeft.Str := '0px';
end;

function TOMBMargins.IsNeedSave: Boolean;
begin
  Result := (FRight.Value<>0) or (FBottom.Value<>0)
    or (FTop.Value<>0) or (FBottom.Value<>0);
end;

procedure TOMBMargins.LoadFromNode(aXMLNode: IXMLNode);

begin
  inherited;
  FTop := GetAttr(aXMLNode, str_Top, '0px');
  FRight := GetAttr(aXMLNode, str_Right, '0px');
  FBottom := GetAttr(aXMLNode, str_Bottom, '0px');
  FLeft := GetAttr(aXMLNode, str_Left, '0px');
end;

procedure TOMBMargins.SaveToNode(aXMLNode: IXMLNode);
begin
  inherited;
  if FTop.Value<>0 then
    aXMLNode.Attributes[str_Top] := FTop.Str;
  if FBottom.Value<>0 then
    aXMLNode.Attributes[str_Bottom] := FBottom.Str;
  if FRight.Value<>0 then
    aXMLNode.Attributes[str_Right] := FRight.Str;
  if FLeft.Value<>0 then
    aXMLNode.Attributes[str_Left] := FLeft.Str;
end;

procedure TOMBMargins.SetBottom(const Value: TOMBProperty);
begin
  if FBottom.Str<>Value.Str then
  begin
    FBottom := Value;
    Changed;
  end;
end;

procedure TOMBMargins.SetLeft(const Value: TOMBProperty);
begin
  if FLeft.Str<>Value.Str then
  begin
    FLeft := Value;
    Changed;
  end;
end;

procedure TOMBMargins.SetRight(const Value: TOMBProperty);
begin
  if FRight.Str<>Value.Str then
  begin
    FRight := Value;
    Changed;
  end;
end;

procedure TOMBMargins.SetTop(const Value: TOMBProperty);
begin
  if FTop.Str<>Value.Str then
  begin
    FTop := Value;
    Changed;
  end;
end;

procedure TOMBMargins.SetValues(const aLeft, aRight, aTop, aBottom: String);
begin
  FLeft := aLeft;
  FTop := aTop;
  FRight := aRight;
  FBottom := aBottom;
  Changed;
end;

{ TOMBElement }

function TOMBElement.AbsoluteOpacity: Single;
begin
  Result := 1;
  if Assigned(FCell) then
    Result := FCell.AbsoluteOpacity * FOpacity else
    if FParent<>nil then
      Result := FParent.AbsoluteOpacity * FOpacity;
end;

function TOMBElement.ActiveCell: TOMBCell;
begin
  Result := FCell;
  if (Result=nil) and (FParent<>nil) then
    Result := FParent.ActiveCell;

end;

procedure TOMBElement.ApplyData;
var
  Value : TValue;
begin
  if FName<>'' then
  begin
    if GetData(FName, Value) then
      SetData(Value);
  end;
end;

procedure TOMBElement.CalcOriginPlace;
var
  dX, dY : Single;
begin
  CalcPlace := OwnerPlace;
  CalcMargins.Left := Margins.FLeft.Calc(CalcPlace.Width);
  CalcMargins.Top := Margins.FTop.Calc(CalcPlace.Height);
  CalcMargins.Right := Margins.FRight.Calc(CalcPlace.Width);
  CalcMargins.Bottom := Margins.FBottom.Calc(CalcPlace.Height);

  CalcPadding.Left := Padding.FLeft.Calc(CalcPlace.Width);
  CalcPadding.Top := Padding.FTop.Calc(CalcPlace.Height);
  CalcPadding.Right := Padding.FRight.Calc(CalcPlace.Width);
  CalcPadding.Bottom := Padding.FBottom.Calc(CalcPlace.Height);

  CalcPlace.Width := Width.Calc(OwnerPlace.Width);
  CalcPlace.Height := Height.Calc(OwnerPlace.Height);

  case Align of
    TOMBElemetAlign.Left: begin
      dX := - CalcPlace.Left + CalcMargins.Left;
      dY := 0;
      CalcPlace.Offset(dX, dY);
      CalcPlace.Top := CalcMargins.Top;
      CalcPlace.Height := OwnerPlace.Height - CalcMargins.Top - CalcMargins.Bottom;
    end;
    TOMBElemetAlign.Right: begin
      dX := OwnerPlace.Right - CalcPlace.Left - CalcMargins.Right -  CalcPlace.Width;
      dY := 0;
      CalcPlace.Offset(dX, dY);
      CalcPlace.Top := CalcMargins.Top;
      CalcPlace.Height := OwnerPlace.Height - CalcMargins.Top - CalcMargins.Bottom;
    end;
    TOMBElemetAlign.Top:
    begin
      dX := 0;
      dY := -CalcPlace.Top+ CalcMargins.Top;
      CalcPlace.Offset(dX, dY);
      CalcPlace.Width := OwnerPlace.Width - CalcMargins.Left - CalcMargins.Right;
      CalcPlace.Left := OwnerPlace.Left + CalcMargins.Left;
    end;
    TOMBElemetAlign.Bottom:
    begin
      dX := 0;
      dY := OwnerPlace.Bottom - CalcPlace.Top - CalcMargins.Bottom - CalcPlace.Height;
      CalcPlace.Offset(dX, dY);
      CalcPlace.Width := OwnerPlace.Width - CalcMargins.Left - CalcMargins.Right;
      CalcPlace.Left := OwnerPlace.Left + CalcMargins.Left;
    end;
    TOMBElemetAlign.Center:
    begin
      CalcPlace.Left := OwnerPlace.Left + (OwnerPlace.Width - CalcPlace.Width) / 2;
      CalcPlace.Top := OwnerPlace.Top + (OwnerPlace.Height - CalcPlace.Height) / 2;
    end;
    TOMBElemetAlign.Client, TOMBElemetAlign.Contents:
    begin
      CalcPlace.Left := OwnerPlace.Left + CalcMargins.Left;
      CalcPlace.Top := OwnerPlace.Top + CalcMargins.Top;
      CalcPlace.Width := OwnerPlace.Width - CalcMargins.Left - CalcMargins.Right;
      CalcPlace.Height := OwnerPlace.Height - CalcMargins.Top - CalcMargins.Bottom;
    end;
  end;
end;

constructor TOMBElement.Create;
begin
  inherited;
  FWidth := '60 px';
  FHeight := '35px';
  FOpacity :=1;
  FBodyColor := 0;
  FVisible := True;
  FAlign := TOMBElemetAlign.Top;
  FCell := nil;
  FParent := nil;
  FMargins := TOMBMargins.Create;
  FMargins.FParent := Self;
  FPadding := TOMBMargins.Create;
  FPadding.FParent := Self;
  Childs := TOMBElementsList.Create(True);
end;

destructor TOMBElement.Destroy;
begin
  FParent := nil;
  Childs.Clear;
  FreeAndNil(Childs);
  FreeAndNil(FMargins);
  FreeAndNil(FPadding);
  inherited;
end;

function TOMBElement.FindElement(const aName: String): TOMBElement;
var
  MyElem: TOMBElement;
begin
  Result := nil;

  if (FName<>'') and aName.Equals(FName) then
    Result := Self else
    for MyElem in Childs do
    begin
      Result := MyElem.FindElement(aName);
      if Result<>nil then
        Break;
    end;

end;

function TOMBElement.FindElementAt(X, Y: Single): TOMBElement;
var
  MyElem, Find: TOMBElement;
begin
  Result := nil;
  if  CalcPlace.Contains(TPointF.Create(X,Y)) then
  begin
    Find := nil;
    for MyElem in Childs do
    begin
      Find := MyElem.FindElementAt(X,Y);
      if Find<>nil then
        break;
    end;
    if Find=nil then
      Find := Self;
    Result := Find;
  end;
end;

function TOMBElement.FullHeight: Single;
begin
  Result := CalcMargins.Top + CalcPlace.Height + CalcMargins.Bottom;
end;

function TOMBElement.FullWidth: Single;
begin
  Result := CalcMargins.Left + CalcPlace.Width + CalcMargins.Right;
end;

function TOMBElement.GetData(const aKey : String; out aData: TValue): Boolean;
begin
  Result := False;
  if FCell<>nil then
    Result := FCell.FData.TryGetValue(aKey, aData) else
    if FParent<>nil then
      Result := FParent.GetData(aKey, aData);
end;

function TOMBElement.GetStructure: String;
begin

end;

procedure TOMBElement.InternalAlign;
var
  RC : TRectF;

  procedure reAlign(AAlignMode :  TOMBElemetAlign);
  var
    Element : TOMBElement;
  begin
    for Element in Childs do
    begin
      if AAlignMode=Element.Align then
      begin
        Element.Resize(RC);
        case AAlignMode of
          TOMBElemetAlign.Left: RC.Left := RC.Left + Element.FullWidth;
          TOMBElemetAlign.Right: RC.Right := RC.Right - Element.FullWidth;
          TOMBElemetAlign.Top: RC.Top := RC.Top + Element.FullHeight;
          TOMBElemetAlign.Bottom: RC.Bottom := RC.Bottom  - Element.FullHeight;
          TOMBElemetAlign.Center: ;
          TOMBElemetAlign.Client: ;
          TOMBElemetAlign.Contents: ;
        end;
      end;
    end;
  end;

begin
  CalcOriginPlace;
  RC:= CalcPlace;
  RC.Left := RC.Left + CalcPadding.Left;
  RC.Top := RC.Top + CalcPadding.Top;
  RC.Right := RC.Right - CalcPadding.Right;
  RC.Bottom := RC.Bottom - CalcPadding.Bottom;
  reAlign(TOMBElemetAlign.Contents);
  reAlign(TOMBElemetAlign.Top);
  reAlign(TOMBElemetAlign.Bottom);
  reAlign(TOMBElemetAlign.Left);
  reAlign(TOMBElemetAlign.Right);
  reAlign(TOMBElemetAlign.Center);
  reAlign(TOMBElemetAlign.Client);
end;

procedure TOMBElement.InternalPaint(Canvas: TCanvas);
begin
  Canvas.Stroke.Color :=  FBodyColor;
  Canvas.Stroke.Kind :=  TBrushKind.Solid;
  Canvas.Fill.Color := FBodyColor;
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.FillRect(CalcPlace, 0,0, [], AbsoluteOpacity);
end;

function TOMBElement.IsDesignMode: Boolean;
begin
  Result := True;
  if ActiveCell<>nil then
    Result :=  csDesigning in ActiveCell.ComponentState;
end;

function TOMBElement.IsEditMode: Boolean;
begin
  Result := True;
  if ActiveCell<>nil then
    Result :=  ActiveCell.EditMode;
end;

function TOMBElement.IsSelected: Boolean;
begin
  Result := ActiveCell.Selected = self;
end;

procedure TOMBElement.LoadFromNode(aXMLNode: IXMLNode);
var
  Ch : IXMLNode;
  I: Integer;
  Cl : TOMBClass;
  Obj : TOMBElement;
  S : String;
begin
  Inherited;
  if aXMLNode=nil then
    Exit;

  S := GetAttr(aXMLNode, str_Align, 'Top');
  FAlign := TOMBElemetAlign(GetEnumValue(PTypeInfo(TypeInfo(TOMBElemetAlign)), S));

  FBodyColor := StrToInt('$'+GetAttr(aXMLNode, str_BgColor, '0'));

  FVisible := GetAttr(aXMLNode, str_Visible, True);

  FOpacity := GetAttr(aXMLNode, str_Opacity, 1);

  FWidth := getAttr(aXMLNode, str_Width, '0 px');

  FHeight := getAttr(aXMLNode, str_Height, '0 px');


  for I := 0 to aXMLNode.ChildNodes.Count-1 do
  begin
    Ch := aXMLNode.ChildNodes.Nodes[I];
    if Ch.NodeName=str_Margins then
    begin
      FMargins.LoadFromNode(Ch);
      Continue
    end;

    if Ch.NodeName=str_Padding then
    begin
      FPadding.LoadFromNode(Ch);
      Continue;
    end;

    Cl := FindClass(Ch.NodeName);
    if (Cl<>nil) and (Cl.InheritsFrom(TOMBElement)) then
    begin
      Obj := Cl.Create as TOMBElement;
      Obj.Parent := Self;
      Obj.LoadFromNode(Ch);
    end;
  end;
end;

procedure TOMBElement.MarginsChanged;
begin
  InternalAlign;
end;

procedure TOMBElement.Paint(Canvas: TCanvas);
var
  Element: TOMBElement;
begin
  if FCell<>nil then
    State := Canvas.SaveState;
  try
  InternalPaint(Canvas);

  if IsDesignMode then
    PaintDesignTime(Canvas);

  for Element in Childs do
  begin
    Element.Paint(Canvas);
  end;

  if IsEditMode then
  begin
    if IsSelected then
      PaintEditSelectedTime(Canvas) else
      PaintEditTime(Canvas);
  end;


  finally
    if FCell<>nil then
      Canvas.RestoreState(State);
  end;
end;

procedure TOMBElement.PaintDesignTime(Canvas: TCanvas);
const
  Dash: array [Boolean] of TStrokeDash = (TStrokeDash.Dot, TStrokeDash.Dash);
var
  R: TRectF;
  State: TCanvasSaveState;
begin
  R := CalcPlace;
  InflateRect(R, -0.5, -0.5);
  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.Stroke.Dash := Dash[Visible];
  Canvas.Stroke.Color := TAlphaColorRec.Black;
  Canvas.Fill.Kind := TBrushKind.None;
  Canvas.Stroke.Thickness := 1;
    Canvas.DrawRect(R, 0, 0, AllCorners, 0.3);
    Canvas.Font.Size := 8;
end;

procedure TOMBElement.PaintEditSelectedTime(Canvas: TCanvas);
const
  Dash: array [Boolean] of TStrokeDash = (TStrokeDash.Dot, TStrokeDash.Dash);
var
  R: TRectF;
  State: TCanvasSaveState;
begin
  R := CalcPlace;
  InflateRect(R, -0.5, -0.5);
  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.Stroke.Dash := Dash[Visible];
  Canvas.Stroke.Color := TAlphaColorRec.Black;
  Canvas.Fill.Kind := TBrushKind.None;
  Canvas.Stroke.Thickness := 3;
    Canvas.DrawRect(R, 0, 0, AllCorners, 0.7);
    Canvas.Font.Size := 8;
end;

procedure TOMBElement.PaintEditTime(Canvas: TCanvas);
const
  Dash: array [Boolean] of TStrokeDash = (TStrokeDash.Dot, TStrokeDash.Dash);
var
  R: TRectF;
  State: TCanvasSaveState;
begin
  R := CalcPlace;
  InflateRect(R, -0.5, -0.5);
  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.Stroke.Dash := Dash[Visible];
  Canvas.Stroke.Color := TAlphaColorRec.Black;
  Canvas.Fill.Kind := TBrushKind.None;
  Canvas.Stroke.Thickness := 1;
    Canvas.DrawRect(R, 0, 0, AllCorners, 0.1);
    Canvas.Font.Size := 8;
end;

procedure TOMBElement.Resize(aBounds: TRectF);
begin
  OwnerPlace := aBounds;
  InternalAlign;
end;

procedure TOMBElement.SaveToNode(aXMLNode: IXMLNode);
var
  Element : TOMBObject;
begin
  inherited;

  if FMargins.IsNeedSave then
    FMargins.SaveToNode(aXMLNode.AddChild(str_Margins));
  if FPadding.IsNeedSave then
    FPadding.SaveToNode(aXMLNode.AddChild(str_Padding));

  if not FVisible then
    aXMLNode.Attributes[str_Visible] := FVisible;

  if FOpacity<1 then
    aXMLNode.Attributes[str_Opacity] := FOpacity;

  if FWidth.Value<>0 then
    aXMLNode.Attributes[str_Width] := FWidth.Str;

  if FHeight.Value<>0 then
    aXMLNode.Attributes[str_Height] := FHeight.Str;

  if FBodyColor<>0 then
    aXMLNode.Attributes[str_BgColor] := IntToHex(FBodyColor, 8);

  if FAlign<>TOMBElemetAlign.Top then
    aXMLNode.Attributes[str_Align] := GetEnumName(PTypeInfo(TypeInfo(TOMBElemetAlign)), Ord(Align));


  for Element in Childs do
  begin
    Element.SaveToNode(aXMLNode.AddChild(Element.GetNodeName));
  end;
end;

procedure TOMBElement.SetAlign(const Value: TOMBElemetAlign);
begin
  FAlign := Value;
end;

procedure TOMBElement.SetBodyColor(const Value: TAlphaColor);
begin
  FBodyColor := Value;
end;

procedure TOMBElement.SetCell(const Value: TOMBCell);
begin
  FCell := Value;
end;

procedure TOMBElement.SetData(const Value: TValue);
begin

end;

procedure TOMBElement.SetHeight(const Value: TOMBProperty);
begin
  FHeight := Value;
end;


procedure TOMBElement.SetMargins(const Value: TOMBMargins);
begin
  FMargins := Value;
  InternalAlign;
end;

procedure TOMBElement.SetOpacity(const Value: Single);
begin
  FOpacity := Value;
end;

procedure TOMBElement.SetPadding(const Value: TOMBMargins);
begin
  FPadding := Value;
end;

procedure TOMBElement.SetParent(const Value: TOMBElement);
begin
  if Value<>nil then
    if not Value.InheritsFrom(TOMBElement) then
       raise Exception.Create('Invalid object class ['+Value.ClassName+']');


  if FParent<>nil then
    FParent.Childs.Remove(self);

  FParent := Value;

  if Value<>nil then
    Value.Childs.Add(self);

  if Value<>nil then
  begin
    OwnerPlace := Value.CalcPlace;
    InternalAlign;
  end;
end;

procedure TOMBElement.SetPropertyValue(const SS: TArray<String>;
  const aValue: TValue);
begin
end;

procedure TOMBElement.SetStructure(const Value: String);
begin
  inherited;

end;

procedure TOMBElement.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

procedure TOMBElement.SetWidth(const Value: TOMBProperty);
begin
  FWidth := Value;
end;

{ TOMBCell }

procedure TOMBCell.Resize;
begin
  inherited;
  if FCell <> nil then
    FCell.Resize(LocalRect);
  
end;

procedure TOMBCell.ApplyData;
var
  aData : TPair<String,TValue>;
  S, aName, aProperty : string;
  Elm : TOMBElement;
  Obj : TObject;
begin
  if FCell<>nil then
  begin
    for aData in FData do
    begin
      aProperty := aData.Key;
      aName := GetToken(aProperty, '.');
      Elm := FindElement(aName);
      Obj := Elm;
      if Elm<>nil then
        FindProperty(Obj, aProperty, procedure(Instance: TObject; Prop: TRttiProperty)
        begin
          if Prop<>nil then
            Prop.SetValue(Instance, aData.Value);
        end);
    end;
  end;
end;

procedure TOMBCell.ChangeSelected;
begin
  if Assigned(FOnChangeSelected) then
    FOnChangeSelected(FSelected);

end;

constructor TOMBCell.Create(AOwner: TComponent);
begin
  inherited;
  FOnChangeSelected := nil;
  FData := TDictionary<String,TValue>.Create;
  FImages := nil;
  FEditMode := False;
  FSelected := nil;
end;

destructor TOMBCell.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

function TOMBCell.FindElement(const aName: String): TOMBElement;
begin
  if FCell<>nil then
    Result := FCell.FindElement(aName);
end;

function TOMBCell.FindElementAt(X, Y: Single): TOMBElement;
begin
  Result := FCell.FindElementAt(X,Y);
end;

function TOMBCell.GetData(Index: String): TValue;
begin

end;

procedure TOMBCell.Load(const Scheme: String);
begin
  FreeAndNil(FCell);
  Cell := TOMBElement.Load(Scheme) as TOMBElement;
end;

procedure TOMBCell.MouseClick(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
var
  Item : TOMBElement;
begin
  inherited;
  Item := FindElementAt(X,Y);
  Selected := Item;
  Repaint;
end;

procedure TOMBCell.Paint;
var
  SW : TStopwatch;
begin
  SW :=  TStopwatch.StartNew;
  try
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
  if FCell<>nil then
  begin
    if FDataUpdateted then
      ApplyData;
    FCell.Paint(Canvas);
  end;
  finally
    SW.Stop;
    tm_lastPaint := SW.ElapsedMilliseconds;
  end;
end;

procedure TOMBCell.SetCell(const Value: TOMBElement);
begin
  if FCell<>nil then
    FCell.FCell := nil;

  FCell := Value;

  if FCell<>nil then
  begin
    Opacity := 0;
    FDataUpdateted := True;
    FCell.FCell := Self;
    FCell.Resize(LocalRect);
    TAnimator.AnimateFloat(Self, 'Opacity', 1);
  end;
  Repaint;
end;

procedure TOMBCell.SetData(Index: String; const Value: TValue);
begin
  FData.AddOrSetValue(Index, Value);
  FDataUpdateted := True;
end;

procedure TOMBCell.SetEditMode(const Value: Boolean);
begin
  FEditMode := Value;
end;

procedure TOMBCell.SetImages(const Value: TImageList);
begin
  FImages := Value;
end;

procedure TOMBCell.SetOnChangeSelected(const Value: TNotifyEvent);
begin
  FOnChangeSelected := Value;
end;

procedure TOMBCell.SetPlan(const Value: TStringList);
begin
  FPlan := Value;
end;



procedure TOMBCell.SetSelected(const Value: TOMBElement);
begin
  if FSelected<>Value then
  begin
    FSelected := Value;
    ChangeSelected;
  end;
end;

{ TOMBPersistent }

procedure TOMBObject.AssingName;
var
  S : String;
  I : Integer;
  C : Char;
begin
  for I := 1 to 1000 do
  begin
    S:= ClassName + I.ToString;
    C := S.Chars[0];
    if C in ['t', 'T'] then
      S := S.Remove(0,1);

    if not OMBObjects.ContainsKey(S) then
    begin
      Name := S;
      Break;
    end;
  end;

end;

constructor TOMBObject.Create;
begin
  inherited;
  Name := '';
//  AssingName;
end;

destructor TOMBObject.Destroy;
begin

  inherited;
end;

class function TOMBObject.GetNodeName: String;
begin
  Result := ClassName.ToLower;
  if Result.Chars[0] = 't' then
    Result := Result.Remove(0,1);
  if Pos('omb', Result)=Low(Result) then
    Result := Result.Remove(0,3);
end;

function TOMBObject.GetStructure: String;
begin

end;

function TOMBObject.IsNeedSave: Boolean;
begin
  Result := True;
end;

class function TOMBObject.Load(Doc: IXMLDocument): TOMBObject;
var
  Cl : TOMBClass;
  Node : IXMLNode;
begin
  Node := Doc.DocumentElement;
  if Node<>nil then
  begin
    Cl := FindClass(Node.NodeName);
    if Cl <> nil then
    begin
      Result := Cl.Create;
      Result.LoadFromNode(Node);
    end;
  end;
end;

class function TOMBObject.Load(Doc: String): TOMBObject;
var
  Dc : IXMLDocument;
begin
  Dc := NewXMLDocument;
  Dc.LoadFromXML(Doc);
  Result := Load(Dc);
end;

procedure TOMBObject.LoadFromNode(aXMLNode: IXMLNode);
begin
  Name := VarToStr(aXMLNode.Attributes[str_Name]);
end;

procedure TOMBObject.Save(Doc: IXMLDocument);
begin
  Doc.DocumentElement := Doc.CreateElement(GetNodeName, '');
  SaveToNode(Doc.DocumentElement);
end;

procedure TOMBObject.SaveToNode(aXMLNode: IXMLNode);
begin
  if FName<>'' then
    aXMLNode.Attributes[str_Name] := Name;
end;

procedure TOMBObject.SetName(const Value: String);
begin
  if FName<>Value then
  begin
    FName := Value;
  end;
end;

procedure TOMBObject.SetStructure(const Value: String);
begin

end;

{ TOMBFont }

constructor TOMBFont.Create;
begin
  inherited;
  FSize := 12;
  FFamily := 'Robot';
  FColor := TAlphaColorRec.Black;
  FItalic := False;
  FStrikeOut := False;
  FBold := False;
  FUnderline := False;
  FFont := TFont.Create;
end;

destructor TOMBFont.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;

function TOMBFont.Font: TFont;
begin
  FFont.Family := Family;
  FFont.Size := Size;
  FFont.Style := [];
  if FUnderline then FFont.Style := FFont.Style + [TFontStyle.fsUnderline];
  if FItalic then FFont.Style := FFont.Style + [TFontStyle.fsItalic];
  if FStrikeOut then FFont.Style := FFont.Style + [TFontStyle.fsStrikeOut];
  if FBold then FFont.Style := FFont.Style + [TFontStyle.fsBold];
  Result := FFont;
end;

function TOMBFont.IsNeedSave: Boolean;
begin
  Result := (FFamily<>'Robot') or (FSize<>12) or
    (FColor <> TAlphaColorRec.Black) or FUnderline or FBold or FItalic or
    FStrikeOut;
end;

procedure TOMBFont.LoadFromNode(aXMLNode: IXMLNode);
begin
  inherited;
  FFamily := GetAttr(aXMLNode, str_Family, 'Roboto');
  FSize := GetAttr(aXMLNode, str_Size, 12);
  FColor := StrToInt('$'+GetAttr(aXMLNode, str_Color, '$FF000000'));
  FItalic := GetAttr(aXMLNode, str_Italic, False);
  FUnderline := GetAttr(aXMLNode, str_Underline, False);
  FBold := GetAttr(aXMLNode, str_Bold, False);
  FStrikeOut := GetAttr(aXMLNode, str_StrikeOut, False);
end;

procedure TOMBFont.SaveToNode(aXMLNode: IXMLNode);
begin
  inherited;
  if FFamily<>'Robot' then
    aXMLNode.Attributes[str_Family] := FFamily;
  if FSize<>12 then
    aXMLNode.Attributes[str_Size] := FSize;
  if FColor<>TAlphaColorRec.Black then
    aXMLNode.Attributes[str_Color] := IntToHex(FColor, 8);

  if FUnderline then
    aXMLNode.Attributes[str_Underline] := FUnderline;
  if FBold then
    aXMLNode.Attributes[str_Bold] := FBold;
  if FStrikeOut then
    aXMLNode.Attributes[str_StrikeOut] := FStrikeOut;
  if FItalic then
    aXMLNode.Attributes[str_Italic] := FItalic;
end;

procedure TOMBFont.SetBold(const Value: Boolean);
begin
  FBold := Value;
end;

procedure TOMBFont.SetColor(const Value: TAlphaColor);
begin
  FColor := Value;
end;

procedure TOMBFont.SetFamily(const Value: String);
begin
  FFamily := Value;
end;

procedure TOMBFont.SetItalic(const Value: Boolean);
begin
  FItalic := Value;
end;

procedure TOMBFont.SetSize(const Value: Single);
begin
  FSize := Value;
end;

procedure TOMBFont.SetStrikeOut(const Value: Boolean);
begin
  FStrikeOut := Value;
end;

procedure TOMBFont.SetUnderline(const Value: Boolean);
begin
  FUnderline := Value;
end;

{ TOMBText }

constructor TOMBText.Create;
begin
  inherited;
  FFont := TOMBFont.Create;
  FWordWrap := True;
  FHorzAlign := TTextAlign.Leading;
  FVertAlign := TTextAlign.Leading;
end;

destructor TOMBText.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;

procedure TOMBText.InternalPaint(Canvas: TCanvas);
begin
  inherited;
  Canvas.Font.Assign(FFont.Font);
  Canvas.Fill.Color := FFont.Color;
  Canvas.Fill.Kind := TBrushKind.Solid;
  Canvas.FillText(CalcPlace, FText, FWordWrap, AbsoluteOpacity, [], FHorzAlign, FVertAlign);
end;

procedure TOMBText.LoadFromNode(aXMLNode: IXMLNode);
var
  FontNode : IXMLNode;
begin
  inherited;
  FText := GetAttr(aXMLNode, str_Text, '');
  FontNode := aXMLNode.ChildNodes.FindNode(str_Fornt);
  if FontNode<>nil then
    Font.LoadFromNode(FontNode);
end;

procedure TOMBText.SaveToNode(aXMLNode: IXMLNode);
begin
  inherited;
  if FFont.IsNeedSave then
    FFont.SaveToNode(aXMLNode.AddChild(str_Fornt));
  if FText<>'' then
    aXMLNode.Attributes[str_Text] := FText;
end;

procedure TOMBText.SetData(const Value: TValue);
begin
  Text := Value.AsString;
end;

procedure TOMBText.SetFont(const Value: TOMBFont);
begin
  FFont := Value;
end;

procedure TOMBText.SetHorzAlign(const Value: TTextAlign);
begin
  FHorzAlign := Value;
end;

procedure TOMBText.SetText(const Value: String);
begin
  FText := Value;
end;

procedure TOMBText.SetVertAlign(const Value: TTextAlign);
begin
  FVertAlign := Value;
end;

procedure TOMBText.SetWordWrap(const Value: Boolean);
begin
  FWordWrap := Value;
end;

{ TOMBPropertyHelper }

function TOMBPropertyHelper.Calc(CurrentValue: Single): Single;
begin
  Result := Value;
  if IsPercent then
  begin
    Result := CurrentValue * Value /100;
  end;
end;

function TOMBPropertyHelper.IsPercent: Boolean;
begin
  Result := Str.Contains('%');
end;

procedure TOMBPropertyHelper.Split(var S1, S2 : String);
var
  I : Integer;
begin
  S1 := Str.ToLower;
  S2 := '';
  I := S1.IndexOf('%');
//  I := Pos('%', S1);
  if I<Low(S1) then
    I := S1.IndexOf('px');
//    I := Pos('px', S1);
  if I>=Low(S1) then
  begin
    S2 := S1.Substring(I, S1.Length);
    S1 := S1.Substring(0,I);
  end;
end;

function TOMBPropertyHelper.Value: Single;
var
  S1, S2 : String;
begin
  Split(S1,S2);
  Result := S1.ToSingle;
end;

{ TOMBProperty }

class operator TOMBProperty.Implicit(const Value: string): TOMBProperty;
begin
  Result.Str := Value;
end;

{ TOMBImage }

constructor TOMBImage.Create;
begin
  inherited;
  FImage := nil;
  FWrapMode := TImageWrapMode.Original;
end;

destructor TOMBImage.Destroy;
begin
  inherited;
end;

procedure TOMBImage.DrawBitmap(const Canvas: TCanvas; const ARect: TRectF;
  const ABitmap: TBitmap; const AOpacity: Single);
var
  LR, R, IntersectionRect: TRectF;
  I, J: Integer;
  FScreenScale : Single;
begin
  FScreenScale := 1.0;
  LR := TRectF.Create(ARect.Left * FScreenScale, ARect.Top * FScreenScale, ARect.Right * FScreenScale,
    ARect.Bottom * FScreenScale);


    case FWrapMode of
      TImageWrapMode.Original:
        begin
          R := TRectF.Create(ARect.Left, ARect.Top, ARect.Left + ABitmap.Width, ARect.Top + ABitmap.Height);
          IntersectRect(IntersectionRect, LR, R);
          Canvas.DrawBitmap(ABitmap, TRectF.Create(0, 0, IntersectionRect.Width, IntersectionRect.Height),
            TRectF.Create(R.Left, R.Top, R.Left + IntersectionRect.Width / FScreenScale, R.Top + IntersectionRect.Height / FScreenScale),
              AOpacity, False)
        end;
      TImageWrapMode.Fit:
        begin
          R := TRectF.Create(0, 0, ABitmap.Width / ABitmap.BitmapScale, ABitmap.Height / ABitmap.BitmapScale);
          R := R.FitInto(ARect).SnapToPixel(Canvas.Scale, False);
          Canvas.DrawBitmap(ABitmap, TRectF.Create(0, 0, ABitmap.Width, ABitmap.Height), R, AOpacity, False);
        end;
      TImageWrapMode.Stretch:
        begin
          Canvas.DrawBitmap(ABitmap, TRectF.Create(0, 0, ABitmap.Width, ABitmap.Height), ARect, AOpacity, False)
        end;
      TImageWrapMode.Tile:
        begin
          for I := 0 to Trunc(LR.Width / ABitmap.Width) + 1 do
            for J := 0 to Trunc(LR.Height / ABitmap.Height) + 1 do
            begin
              R := TRectF.Create(0, 0, ABitmap.Width, ABitmap.Height);
              OffsetRect(R, I * ABitmap.Width, J * ABitmap.Height);
              IntersectRect(IntersectionRect, LR, R);
              Canvas.DrawBitmap(ABitmap, TRectF.Create(0, 0, IntersectionRect.Width, IntersectionRect.Height),
                TRectF.Create(R.Left / FScreenScale, R.Top / FScreenScale, (R.Left + IntersectionRect.Width) / FScreenScale,
                  (R.Top + IntersectionRect.Height) / FScreenScale), AOpacity, True)
            end;
        end;
      TImageWrapMode.Center:
        begin
          R := TRectF.Create(0, 0, ABitmap.Width / ABitmap.BitmapScale, ABitmap.Height / ABitmap.BitmapScale);
          R := R.CenterAt(ARect).SnapToPixel(Canvas.Scale, False);
          Canvas.DrawBitmap(ABitmap, TRectF.Create(0, 0, ABitmap.Width, ABitmap.Height), R, AOpacity, False);
        end;
      TImageWrapMode.Place:
        begin
          R := TRectF.Create(0, 0, ABitmap.Width / ABitmap.BitmapScale, ABitmap.Height / ABitmap.BitmapScale);
          R := R.PlaceInto(ARect).SnapToPixel(Canvas.Scale, False);
          Canvas.DrawBitmap(ABitmap, TRectF.Create(0, 0, ABitmap.Width, ABitmap.Height), R, AOpacity, False);
        end;
    end;
end;

procedure TOMBImage.InternalPaint(Canvas: TCanvas);
var
  R, R1 : TRectF;
begin
  inherited;
  if (FImage<>nil) and (not FImage.IsEmpty) then
  begin
    DrawBitmap(Canvas, CalcPlace, FImage, AbsoluteOpacity);
  end;
end;

procedure TOMBImage.LoadFromNode(aXMLNode: IXMLNode);
begin
  inherited;
  FWrapMode := TImageWrapMode(GetEnumValue(PTypeInfo(TypeInfo(TImageWrapMode)), aXMLNode.Attributes[str_WrapMode]));
end;

procedure TOMBImage.SaveToNode(aXMLNode: IXMLNode);
begin
  inherited;
  if FWrapMode<>TImageWrapMode.Original then
    aXMLNode.Attributes[str_WrapMode] := GetEnumName(PTypeInfo(TypeInfo(TImageWrapMode)), Ord(WrapMode));
end;

procedure TOMBImage.SetData(const Value: TValue);
begin
  Image := Value.AsObject as TBitmap;
end;

procedure TOMBImage.SetImage(const Value: TBitmap);
begin
  FImage := Value;
end;

procedure TOMBImage.SetWrapMode(const Value: TImageWrapMode);
begin
  FWrapMode := Value;
end;


initialization
  LClassList := TOMBClassCollection.Create;
  OMBObjects := TDictionary<String,TOMBObject>.Create;
  RegisterOMBClass(TOMBMargins);
  RegisterOMBClass(TOMBFont);
  RegisterOMBClass(TOMBElement);
  RegisterOMBClass(TOMBText);
  RegisterOMBClass(TOMBLayout);
  RegisterOMBClass(TOMBImage);
finalization
  FreeAndNil(LClassList);
  FreeAndNil(OMBObjects);
end.
