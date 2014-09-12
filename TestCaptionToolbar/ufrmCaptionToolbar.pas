unit ufrmCaptionToolbar;

interface

uses
  Messages, SysUtils, Variants, Types, Controls, Forms, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, Windows, Classes, Graphics, Actions, ActnList, ToolWin,
  Vcl.ImgList, Vcl.Buttons,

  uFormSkins;

type
  TForm11 = class(TForm)
    Button1: TButton;
    Shape1: TShape;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ActionList1: TActionList;
    Action1: TAction;
    Action2: TAction;
    Action3: TAction;
    ImageList1: TImageList;
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FTest: TskForm;
  protected
    procedure WndProc(var message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form11: TForm11;

implementation


{$R *.dfm}



{ TForm11 }

constructor TForm11.Create(AOwner: TComponent);
begin
  FTest := TskForm.Create(Self);
  inherited;
end;

destructor TForm11.Destroy;
begin
  inherited;
  FreeAndNil(FTest);
end;

procedure TForm11.Action1Execute(Sender: TObject);
begin
  Tag := Tag + 1;
  Caption := format('test %d', [Tag]);
end;

procedure TForm11.Action2Execute(Sender: TObject);
begin
  if Shape1.Shape <> High(TShapeType) then
    Shape1.Shape := Succ(Shape1.Shape)
  else
    Shape1.Shape := low(TShapeType);
end;

procedure TForm11.SpeedButton1Click(Sender: TObject);
begin
  Caption := format('test %d', [1]);
end;

procedure TForm11.WndProc(var message: TMessage);
begin
  if not FTest.DoHandleMessage(Message) then
    inherited;
end;

end.
