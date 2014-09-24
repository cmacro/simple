program TestToolbar;

uses
  Vcl.Forms,
  ufrmTestToolbar in 'ufrmTestToolbar.pas' {Form24},
  uMTToolbars in 'uMTToolbars.pas',
  uUISkins in '..\..\Terminator\Source\UI\base\uUISkins.pas',
  uUIRes in '..\..\Terminator\Source\UI\uUIRes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm24, Form24);
  Application.Run;
end.
