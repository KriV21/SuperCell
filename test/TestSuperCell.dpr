program TestSuperCell;

uses
  System.StartUpCopy,
  FMX.Forms,
  uTestSuperCell in 'uTestSuperCell.pas' {Form1},
  XSuperJSON in '..\Lib\SuperObjext\XSuperJSON.pas',
  XSuperObject in '..\Lib\SuperObjext\XSuperObject.pas',
  OMB.Cells in '..\Lib\SuperCell\OMB.Cells.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
