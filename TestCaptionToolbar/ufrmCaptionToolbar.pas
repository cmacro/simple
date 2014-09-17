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
    ImageList2: TImageList;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure Action3Execute(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
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

procedure TForm11.FormCreate(Sender: TObject);
begin
  FTest.Toolbar.Images := ImageList2;
  FTest.Toolbar.Add(Action1, 0);
  FTest.Toolbar.Add(Action2, 1);
  FTest.Toolbar.Add(Action3, 2);
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

procedure TForm11.Action3Execute(Sender: TObject);
begin
  Action1.Enabled := not Action1.Enabled;
end;

procedure TForm11.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then
    FTest.Toolbar.Images := nil
  else
    FTest.Toolbar.Images := ImageList2;
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
