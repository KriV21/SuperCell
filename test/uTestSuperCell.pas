unit uTestSuperCell;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls, OMB.Cells, FMX.Layouts,
  FMX.Objects, System.ImageList, FMX.ImgList, Xml.XMLDoc, Xml.XMLIntf, Xml.xmldom;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    OMBCell1: TOMBCell;
    Layout1: TLayout;
    Label1: TLabel;
    Memo2: TMemo;
    ImageList1: TImageList;
    Image1: TImage;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    procedure CreateItem;
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses XSuperObject;

procedure TForm1.Button1Click(Sender: TObject);
var
  LaAll, LaBody, LaClient, laLEft, laRight : TOMBLayout;
  txt1, txt2 : TOMBText;
  Img : TOMBImage;
  Img2 : TOMBStaticImage;
  Dom : IXMLDocument;
begin
  LaBody :=  TOMBLayout.Create;
  LaBody.Align := Client;
  LaBody.Name := 'Body';
  LaBody.BodyColor := $FFF8F8F8;
  txt1 := TOMBText.Create;
  txt1.Align := TOMBElemetAlign.Top;
  txt1.Height := '40%';
  txt1.Parent := LaBody;
  txt1.Text := Memo2.Text;
  txt1.Margins.Top := '5%';
  txt2 := TOMBText.Create;
  txt2.Align := TOMBElemetAlign.Bottom;
  txt2.Height := '30%';
  txt2.Parent := LaBody;
  txt2.Font.Color := TAlphaColorRec.Navy;
  txt2.Text := 'Буря мглою небо кроит....';
  txt2.Name := 'comment';
  txt2.Margins.Left := '2%';
  txt2.Margins.Top := '2%';
  txt2.Margins.Right := '2%';
  txt2.Margins.Bottom := '2%';
  LaClient := TOMBLayout.Create;
  LaClient.Parent := LaBody;
  LaClient.Align := Client;

  LaAll := TOMBLayout.Create;
  LaAll.Align := Contents;
  LaBody.Parent := LaAll;
  LaAll.Name :='All';
  LaAll.BodyColor := TAlphaColorRec.White;

  laLEft := TOMBLayout.Create;
  laLEft.Align := TOMBElemetAlign.Left;
  laLEft.Width := '150';
  laLEft.Parent := LaAll;
  laLEft.Margins.Top := '35';
  laLEft.Padding.SetValues('4','4','4','4');

  Img := TOMBImage.Create;
  Img.Height := '120';
  Img.Align := TOMBElemetAlign.Top;
  Img.WrapMode := TImageWrapMode.Fit;
  Img.Image := ImageList1.Source.Items[0].MultiResBitmap.Bitmaps[1];
  Img.Parent := laLEft;

  laRight := TOMBLayout.Create;
  laRight.Align := TOMBElemetAlign.Right;
  laRight.Width := '105';
  laRight.Parent := LaAll;

  Img2 := TOMBStaticImage.Create;
  Img2.Parent := laRight;
  Img2.Align := TOMBElemetAlign.Bottom;
  Img2.WrapMode := TImageWrapMode.Place;
  Img2.Image := ImageList1.Source.Items[1].MultiResBitmap.Bitmaps[1];

  Dom := NewXMLDocument;

  LaAll.Save(Dom);
  Memo1.Text := FormatXMLData(Dom.XML.Text);
  OMBCell1.Data['comment.Text'] := 'Это просто праздник.';
  OMBCell1.Data['comment.BodyColor'] := $FF808080;
  OMBCell1.Cell := TOMBElement.Load(Dom) as TOMBElement;
end;

procedure TForm1.CreateItem;
begin

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Label1.Text := OMBCell1.tm_lastPaint.ToString;
end;

end.
