unit frmTestPageAnimMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  dxGDIPlusClasses, Vcl.Imaging.jpeg;

type
  TForm3 = class(TForm)
    Button1: TButton;
    Panel3: TPanel;
    pnlTree: TPanel;
    pnlSapling: TPanel;
    Image1: TImage;
    Image2: TImage;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }


  end;

var
  Form3: TForm3;

implementation

uses
  uUIShowings;

{$R *.dfm}

procedure TForm3.Button1Click(Sender: TObject);
var
  v1,v2:TWinControl;
begin
  if pnlTree.Visible then
  begin
    v1 := pnlTree;
    v2 := pnlSapling;
  end
  else
  begin
    v1 := pnlSapling;
    v2 := pnlTree;
  end;

  PrepareForAnimation(v1);
  V1.Visible  := False;
  v2.Visible  := True;
  AnimShowControl(v2, 500);

end;

end.
