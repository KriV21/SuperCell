program TestSuperCell;

uses
  System.StartUpCopy,
  FMX.Forms,
  uTestSuperCell in 'uTestSuperCell.pas' {Form1},
  OMB.Cells in '..\Lib\SuperCell\OMB.Cells.pas',
  XSuperJSON in '..\Lib\SuperObject\XSuperJSON.pas',
  XSuperObject in '..\Lib\SuperObject\XSuperObject.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
