program TestSuperCell;

uses
  System.StartUpCopy,
  FMX.Forms,
  uTestSuperCell in 'uTestSuperCell.pas' {Form1},
  OMB.Cells in '..\Lib\SuperCell\OMB.Cells.pas' {$R *.res},
  OMB.Cells.EditorFrame in '..\Lib\SuperCell\OMB.Cells.EditorFrame.pas' {OMBEditor: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
