program testCaptionToolbar;

uses
  Vcl.Forms,
  ufrmCaptionToolbar in 'ufrmCaptionToolbar.pas' {Form11},
  ufrmDispatchWnd in 'ufrmDispatchWnd.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  //Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm11, Form11);
  Application.Run;
end.
