program TestToolbar;

uses
  Vcl.Forms,
  ufrmTestToolbar in 'ufrmTestToolbar.pas' {Form24},
  umyToolbars in 'umyToolbars.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm24, Form24);
  Application.Run;
end.
