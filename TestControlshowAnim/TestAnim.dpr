program TestAnim;

uses
  Vcl.Forms,
  frmTestPageAnimMain in 'frmTestPageAnimMain.pas' {Form3},
  uUIShowings in 'uUIShowings.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
