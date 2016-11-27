unit uTestSuperCell;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls, OMB.Cells, FMX.Layouts;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    OMBCell1: TOMBCell;
    Layout1: TLayout;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses XSuperObject;

procedure TForm1.Button1Click(Sender: TObject);
var
  La1 : TOMBLayout;
  txt1, txt2 : TOMBText;
begin
  La1 :=  TOMBLayout.Create;
  La1.Align := Client;
  txt1 := TOMBText.Create;
  txt1.Align := TOMBElemetAlign.Top;
  txt1.Bounds.Height := '40%';
  txt1.Parent := La1;
  txt2 := TOMBText.Create;
  txt2.Align := TOMBElemetAlign.Top;
  txt2.Bounds.Height := '30%';
  txt2.Parent := La1;
  Memo1.Text := La1.JSON.AsJSON(True);
  OMBCell1.Cell := La1;
end;

end.
