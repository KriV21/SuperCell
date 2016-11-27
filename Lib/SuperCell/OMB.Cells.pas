unit OMB.Cells;

interface

uses Sysutils, Classes, FMX.Controls, FMX.Graphics, FMX.Types,
  System.Generics.Collections, System.Types, System.Rtti,
  System.UITypes, XSuperObject;

type
  TOMBElemetAlign = (None, Left, Right, Top, Bottom, Center, Client, Contents);

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
    function GetJSON: ISuperObject; virtual;
    procedure SetJSON(const Value: ISuperObject); virtual;
  public
    constructor Create; virtual;
    property JSON : ISuperObject read GetJSON write SetJSON;
  end;

  TOMBClass = class of TOMBObject;

  TOMBBounds = class(TOMBObject)
  private
    FWidth: TOMBProperty;
    FTop: TOMBProperty;
    FHeight: TOMBProperty;
    FLeft: TOMBProperty;
    procedure SetHeight(const Value: TOMBProperty);
    procedure SetLeft(const Value: TOMBProperty);
    procedure SetTop(const Value: TOMBProperty);
    procedure SetWidth(const Value: TOMBProperty);
    function GetJSON: ISuperObject; override;
    procedure SetJSON(const Value: ISuperObject); override;
  private const
    str_Width   = 'width';
    str_Height  = 'height';
    str_Top     = 'top';
    str_Left    = 'left';
  protected
    [weak] FParent: TOMBElement;
    procedure Changed;
  public
    constructor Create; override;
    property Left : TOMBProperty read FLeft write SetLeft;
    property Top : TOMBProperty read FTop write SetTop;
    property Width : TOMBProperty read FWidth write SetWidth;
    property Height : TOMBProperty read FHeight write SetHeight;
  end;

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
    function GetJSON: ISuperObject; override;
    procedure SetJSON(const Value: ISuperObject); override;
  protected
    [weak] FParent: TOMBElement;
    procedure Changed;
  public
    constructor Create; override;
    property Left : TOMBProperty read FLeft write SetLeft;
    property Top : TOMBProperty read FTop write SetTop;
    property Right : TOMBProperty read FRight write SetRight;
    property Bottom : TOMBProperty read FBottom write SetBottom;
  end;

  TOMBFont = class(TOMBObject)
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
  private const
    str_Family    = 'family';
    str_Size     = 'size';
    str_Color   = 'color';
    str_Bold  = 'bold';
    str_Underline  = 'underline';
    str_Italic  = 'italic';
    str_StrikeOut  = 'strikeout';
    function GetJSON: ISuperObject; override;
    procedure SetJSON(const Value: ISuperObject); override;
  public
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
    str_Bounds = 'bounds';
    str_Margins = 'margins';
    str_DataName = 'dataname';
    str_Chilgs = 'childs';
  private
    FAlign: TOMBElemetAlign;
    [weak] FCell: TOMBCell;
    [weak] FParent: TOMBElement;
    FMargins: TOMBMargins;
    FBounds: TOMBBounds;
    FValue: TValue;
    FName: String;
    FVisible: Boolean;
    procedure InternalAlign;
    procedure CalcOriginPlace;
    procedure PaintDesignTime(Canvas : TCanvas);
    procedure SetAlign(const Value: TOMBElemetAlign);
    procedure SetBounds(const Value: TOMBBounds);
    procedure SetCell(const Value: TOMBCell);
    procedure SetMargins(const Value: TOMBMargins);
    procedure SetParent(const Value: TOMBElement);
    procedure SetValue(const Value: TValue);
    procedure SetName(const Value: String);
    procedure SetVisible(const Value: Boolean);
  protected
    CalcPlace, OwnerPlace, CalcMargins :  TRectF;
    Childs : TOMBElementsList;
    procedure Resize(aBounds : TRectF); virtual;
    procedure Paint(Canvas : TCanvas);
    procedure InternalPaint(Canvas : TCanvas); virtual;
    function IsDesignMode : Boolean;

    procedure BoundsChanged;
    procedure MarginsChanged;
    function GetJSON: ISuperObject; override;
    procedure SetJSON(const Value: ISuperObject); override;
    function FullHeight : Single;
    function FullWidth : Single;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Cell :  TOMBCell read FCell write SetCell;
    property Parent : TOMBElement read FParent write SetParent;
    property Align : TOMBElemetAlign read FAlign write SetAlign;
    property Bounds : TOMBBounds read FBounds write SetBounds;
    property Margins : TOMBMargins read FMargins write SetMargins;
    property Value : TValue read FValue write SetValue;
    property Name : String read FName write SetName;
    property Visible : Boolean read FVisible write SetVisible;
  end;

  TOMBText = class(TOMBElement)
  private
    FFont: TOMBFont;
    procedure SetFont(const Value: TOMBFont);
  protected
    function GetJSON: ISuperObject; override;
    procedure SetJSON(const Value: ISuperObject); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Font : TOMBFont read FFont write SetFont;
  end;

  TOMBLayout = class(TOMBElement)

  end;

  TOMBCell = class (TControl)
  private
    FPlan: TStringList;
    FCell: TOMBElement;
    procedure SetPlan(const Value: TStringList);
    procedure SetCell(const Value: TOMBElement);
//    procedure PlanChanged(Sender : TObject);
  protected
    procedure Paint; override;
    procedure Resize; override;
  public
    property Cell : TOMBElement read FCell write SetCell;
  published
//    property Plan : TStringList read FPlan write SetPlan;
  published
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
  str_ClassName = 'omb_class';

type
  TOMBClassCollection = class(TList<TOMBClass>)
  end;

var
  LClassList : TOMBClassCollection;

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

{ TOMBBounds }

procedure TOMBBounds.Changed;
begin

end;

constructor TOMBBounds.Create;
begin
  inherited;
  FParent := nil;
  FWidth.Str := '60 px';
  FHeight.Str := '35px';
  FLeft.Str := '0px';
  FTop.Str := '0px';
end;

function TOMBBounds.GetJSON: ISuperObject;
begin
  Result := Inherited;
  Result.S[str_Width]       := FWidth.Str;
  Result.S[str_Height]      := FHeight.Str;
  Result.S[str_Top]         := FTop.Str;
  Result.S[str_Left]        := FLeft.Str;
end;

procedure TOMBBounds.SetHeight(const Value: TOMBProperty);
begin
  if FHeight.Str<>Value.Str then
  begin
    FHeight := Value;
    Changed;
  end;
end;

procedure TOMBBounds.SetJSON(const Value: ISuperObject);
begin
  FWidth.Str  := Value.S[str_Width];
  FHeight.Str := Value.S[str_Height];
  FTop.Str    := Value.S[str_Top];
  FLeft.Str   := Value.S[str_Left];
end;

procedure TOMBBounds.SetLeft(const Value: TOMBProperty);
begin
  if FLeft.Str<>Value.Str then
  begin
    FLeft := Value;
    Changed;
  end;
end;

procedure TOMBBounds.SetTop(const Value: TOMBProperty);
begin
  if FTop.Str<>Value.Str then
  begin
    FTop := Value;
    Changed;
  end;
end;

procedure TOMBBounds.SetWidth(const Value: TOMBProperty);
begin
  if FWidth.Str<>Value.Str then
  begin
    FWidth := Value;
    Changed;
  end;
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

function TOMBMargins.GetJSON: ISuperObject;
begin
  Result := Inherited;
  Result.S[str_Right]       := FRight.Str;
  Result.S[str_Bottom]      := FBottom.Str;
  Result.S[str_Top]         := FTop.Str;
  Result.S[str_Left]        := FLeft.Str;
end;

procedure TOMBMargins.SetBottom(const Value: TOMBProperty);
begin
  if FBottom.Str<>Value.Str then
  begin
    FBottom := Value;
    Changed;
  end;
end;

procedure TOMBMargins.SetJSON(const Value: ISuperObject);
begin
  FRight.Str  := Value.S[str_Right];
  FBottom.Str := Value.S[str_Bottom];
  FTop.Str    := Value.S[str_Top];
  FLeft.Str   := Value.S[str_Left];
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

{ TOMBElement }

procedure TOMBElement.BoundsChanged;
begin
  CalcOriginPlace;
  InternalAlign;
end;

procedure TOMBElement.CalcOriginPlace;
begin
  CalcPlace := OwnerPlace;
  CalcMargins.Left := Margins.FLeft.Calc(CalcPlace.Width);
  CalcMargins.Top := Margins.FTop.Calc(CalcPlace.Height);
  CalcMargins.Right := Margins.FRight.Calc(CalcPlace.Width);
  CalcMargins.Bottom := Margins.FBottom.Calc(CalcPlace.Height);

  CalcPlace.Left := OwnerPlace.Left+CalcMargins.Left+Bounds.Left.Calc(OwnerPlace.Width);
  CalcPlace.Top := OwnerPlace.Top+CalcMargins.Top+Bounds.Top.Calc(OwnerPlace.Height);
  CalcPlace.Width := Bounds.Width.Calc(OwnerPlace.Width);
  CalcPlace.Height := Bounds.Height.Calc(OwnerPlace.Height);

  case Align of
    None: ;
    Left: begin
      CalcPlace.Left := CalcMargins.Left + OwnerPlace.Left;
      CalcPlace.Height := OwnerPlace.Height - CalcMargins.Top - CalcMargins.Bottom;
    end;
    Right: begin
      CalcPlace.Left := OwnerPlace.Right - CalcPlace.Width - CalcMargins.Right;
      CalcPlace.Height := OwnerPlace.Height - CalcMargins.Top - CalcMargins.Bottom;
    end;
    Top:
    begin
      CalcPlace.Top := OwnerPlace.Top + CalcMargins.Top;
      CalcPlace.Width := OwnerPlace.Width - CalcMargins.Left - CalcMargins.Right;
    end;
    Bottom:
    begin
      CalcPlace.Top := OwnerPlace.Bottom - CalcPlace.Width - CalcMargins.Bottom;
      CalcPlace.Width := OwnerPlace.Width - CalcMargins.Left - CalcMargins.Right;
    end;
    Center:
    begin
      CalcPlace.Left := OwnerPlace.Left + (OwnerPlace.Width - CalcPlace.Width) / 2;
      CalcPlace.Top := OwnerPlace.Top + (OwnerPlace.Height - CalcPlace.Height) / 2;
    end;
    Client, Contents:
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
  FVisible := True;
  FAlign := None;
  FCell := nil;
  FParent := nil;
  FMargins := TOMBMargins.Create;
  FMargins.FParent := Self;
  FBounds := TOMBBounds.Create;
  FBounds.FParent := Self;
  Childs := TOMBElementsList.Create(True);
  FValue := TValue.Empty;
  FName := '';
end;

destructor TOMBElement.Destroy;
begin
  FParent := nil;
  Childs.Clear;
  FreeAndNil(Childs);
  FreeAndNil(FBounds);
  FreeAndNil(FMargins);
  inherited;
end;

function TOMBElement.FullHeight: Single;
begin
  Result := CalcMargins.Top + CalcPlace.Height + CalcMargins.Bottom;
end;

function TOMBElement.FullWidth: Single;
begin
  Result := CalcMargins.Left + CalcPlace.Width + CalcMargins.Right;
end;

function TOMBElement.GetJSON: ISuperObject;
var
  Element : TOMBElement;
  Ar : ISuperArray;
begin
  Result := inherited;
  Result.O[str_Bounds] := FBounds.JSON;
  Result.O[str_Margins] := FMargins.JSON;
  Result.S[str_DataName] := FName;
  Ar := Result.A['childs'];
  for Element in Childs do
  begin
    Ar.Add(Element.JSON);
  end;
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
          None: ;
          Left: RC.Left := RC.Left + Element.FullWidth;
          Right: RC.Right := RC.Right - Element.FullWidth;
          Top: RC.Top := RC.Top + Element.FullHeight;
          Bottom: RC.Bottom := RC.Bottom  - Element.FullHeight;
          Center: ;
          Client: ;
          Contents: ;
        end;
      end;
    end;
  end;

begin
  CalcOriginPlace;
  RC:= CalcPlace;
  reAlign(None);
  reAlign(Contents);
  reAlign(Top);
  reAlign(Bottom);
  reAlign(Left);
  reAlign(Right);
  reAlign(Center);
  reAlign(Client);
end;

procedure TOMBElement.InternalPaint(Canvas: TCanvas);
begin

end;

function TOMBElement.IsDesignMode: Boolean;
begin
  Result := True;
end;

procedure TOMBElement.MarginsChanged;
begin
  InternalAlign;
end;

procedure TOMBElement.Paint(Canvas: TCanvas);
var
  Element: TOMBElement;
begin
  if IsDesignMode then
    PaintDesignTime(Canvas) else
    InternalPaint(Canvas);
  for Element in Childs do
  begin
    Element.Paint(Canvas);
  end;
end;

procedure TOMBElement.PaintDesignTime(Canvas: TCanvas);
const
  Dash: array [Boolean] of TStrokeDash = (TStrokeDash.Dot, TStrokeDash.Dash);
var
  R: TRectF;
  State: TCanvasSaveState;
  FInPaintTo : Boolean;
begin
  FInPaintTo := False;
  if not FInPaintTo then
  begin
    R := CalcPlace;
    InflateRect(R, -0.5, -0.5);
    State := Canvas.SaveState;
    try
      Canvas.Stroke.Kind := TBrushKind.Solid;
      Canvas.Stroke.Dash := Dash[Visible];
      Canvas.Stroke.Color := TAlphaColorRec.Black;
      Canvas.Stroke.Thickness := 1;
        Canvas.DrawRect(R, 0, 0, AllCorners, 0.3);
      Canvas.Font.Size := 8;
      Canvas.Fill.Color := TAlphaColorRec.Black;
      Canvas.FillText(R, ClassName, False, 0.4, [], TTextAlign.Leading, TTextAlign.Leading);
    finally
      Canvas.RestoreState(State);
    end;
  end;
end;

procedure TOMBElement.Resize(aBounds: TRectF);
begin
  OwnerPlace := aBounds;
  InternalAlign;
end;

procedure TOMBElement.SetAlign(const Value: TOMBElemetAlign);
begin
  FAlign := Value;
end;

procedure TOMBElement.SetBounds(const Value: TOMBBounds);
begin
  FBounds := Value;
end;

procedure TOMBElement.SetCell(const Value: TOMBCell);
begin
  FCell := Value;
end;

procedure TOMBElement.SetJSON(const Value: ISuperObject);
begin
end;

procedure TOMBElement.SetMargins(const Value: TOMBMargins);
begin
  FMargins := Value;
  InternalAlign;
end;

procedure TOMBElement.SetName(const Value: String);
begin
  FName := Value;
end;

procedure TOMBElement.SetParent(const Value: TOMBElement);
begin
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

procedure TOMBElement.SetValue(const Value: TValue);
begin
  FValue := Value;
end;

procedure TOMBElement.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

{ TOMBCell }

procedure TOMBCell.Resize;
begin
  inherited;
  if FCell <> nil then
    FCell.Resize(LocalRect);
  
end;

procedure TOMBCell.Paint;
begin
  inherited;
  if (csDesigning in ComponentState) and not Locked then
    DrawDesignBorder;
  if FCell<>nil then
    FCell.Paint(Canvas);
end;

procedure TOMBCell.SetCell(const Value: TOMBElement);
begin
  if FCell<>nil then
    FCell.FCell := nil;

  FCell := Value;

  if FCell<>nil then
    FCell.FCell := Self;
  FCell.Resize(LocalRect);
  Repaint;
end;

procedure TOMBCell.SetPlan(const Value: TStringList);
begin
  FPlan := Value;
end;



{ TOMBPersistent }

constructor TOMBObject.Create;
begin
  inherited;

end;

function TOMBObject.GetJSON: ISuperObject;
begin
  Result := SO;
  Result.S[str_ClassName] := ClassName;
end;

procedure TOMBObject.SetJSON(const Value: ISuperObject);
begin

end;

{ TOMBFont }

function TOMBFont.GetJSON: ISuperObject;
begin
  Result := inherited;
  Result.S[str_Family] := FFamily;
  Result.F[str_Size] := FSize;
  Result.I[str_Color] := FColor;
  Result.B[str_Bold] := FBold;
  Result.B[str_Underline] := FUnderline;
  Result.B[str_Italic] := FItalic;
  Result.B[str_StrikeOut] := FStrikeOut;
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

procedure TOMBFont.SetJSON(const Value: ISuperObject);
begin

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
end;

destructor TOMBText.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;

function TOMBText.GetJSON: ISuperObject;
begin
  Result := Inherited;
  Result.O['font'] := FFont.JSON;
end;

procedure TOMBText.SetFont(const Value: TOMBFont);
begin
  FFont := Value;
end;

procedure TOMBText.SetJSON(const Value: ISuperObject);
begin
  inherited;

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
  I := Pos('%', S1);
  if I<Low(S1) then
    I := Pos('px', S1);
  if I>=Low(S1) then
  begin
    S2 := Copy(Str, I, Length(Str));
    S1 := Copy(Str, Low(Str),I-1);
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

initialization
  LClassList := TOMBClassCollection.Create;
  RegisterOMBClass(TOMBBounds);
  RegisterOMBClass(TOMBMargins);
  RegisterOMBClass(TOMBFont);
  RegisterOMBClass(TOMBElement);
  RegisterOMBClass(TOMBText);
  RegisterOMBClass(TOMBLayout);
finalization
  FreeAndNil(LClassList);
end.
