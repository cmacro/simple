unit ufrmTestToolbar;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uMTToolbars, System.Actions, Vcl.ActnList, Vcl.ImgList, Vcl.ComCtrls,
  Vcl.ToolWin, Vcl.StdCtrls;

type
  TForm24 = class(TForm)
    ActionList1: TActionList;
    ImageList1: TImageList;
    Action1: TAction;
    Action2: TAction;
    Action3: TAction;
    Action4: TAction;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    Button1: TButton;
    Action5: TAction;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure Action2Execute(Sender: TObject);
    procedure Action3Execute(Sender: TObject);
    procedure Action5Execute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    FToolbar: TmtToolbar;
  public
    { Public declarations }
  end;

var
  Form24: TForm24;

implementation

uses
  uUISkins;

{$R *.dfm}

type
  TacAction = class(TBasicAction);

procedure PrintMsg(const Msg:string); overload;
begin
  Form24.Memo1.Lines.Add(Msg);
end;

procedure PrintMsg(const AFormat: string; const Args: array of const); overload;
begin
  PrintMsg(Format(AFormat, Args));
end;



procedure TForm24.FormCreate(Sender: TObject);
begin
  FToolbar := TmtToolbar.Create(Self);
  FToolbar.ShowHint := True;
  FToolbar.Parent := Self;
  FToolbar.Height := 30;
  FToolbar.Left := 100;
  FToolbar.Top := 100;
  //FToolbar.color := clSilver;

  FToolbar.Add(Action1);
  FToolbar.Add(Action2);
  FToolbar.Add(Action3);
  FToolbar.Add(Action5);
  FToolbar.Add(Action4);
end;

procedure TForm24.Action1Execute(Sender: TObject);
begin
  Tag := Tag + 1;
  Caption := Format('Test %d', [tag]);
end;

procedure TForm24.Action2Execute(Sender: TObject);
begin
  Action1.Visible := not Action1.Visible;
  if Action1.Visible then
    Action2.ImageIndex := 2
  else
    Action2.ImageIndex := 1;
end;

procedure TForm24.Action3Execute(Sender: TObject);
begin
  Action1.Enabled := not Action1.Enabled;
end;

procedure TForm24.Action5Execute(Sender: TObject);
begin
  if Action1.ImageIndex < ImageList1.Count - 1 then
     Action1.ImageIndex := Action1.ImageIndex + 1
  else
    Action1.ImageIndex := 0;
end;

procedure TForm24.Button1Click(Sender: TObject);
//var
//  r: TRect;
//  rFrame: TRect;
//  I: Integer;
//
//  procedure DrawSkinIndicator(v: TSkinIndicator);
//  begin
//    mtUISkin.DrawButtonState(canvas.Handle, v, r, 255);
//    OffsetRect(r, r.Width + 2, 0);
//    mtUISkin.DrawButtonState(canvas.Handle, v, r, 200);
//    OffsetRect(r, r.Width + 2, 0);
//    mtUISkin.DrawButtonState(canvas.Handle, v, r, 180);
//    OffsetRect(r, r.Width + 2, 0);
//    mtUISkin.DrawButtonState(canvas.Handle, v, r, 50);
//    OffsetRect(r, r.Width + 2, 0);
//    mtUISkin.DrawButtonState(canvas.Handle, v, r, 0);
//
//  end;

var
  cCtrl: TWinControl;
begin
//  //FToolbar.Top := FToolbar.Top + 20;
//  r := Rect(10, 30, 30, 50);
//  rFrame := r;
//
//  DrawSkinIndicator(siInactive);
//  r := Rect(10, 30, 30, 50);
//  OffsetRect(r, 0, r.Height + 2);
//  DrawSkinIndicator(siHover);
//  r := Rect(10, 30, 30, 50);
//  OffsetRect(r, 0, r.Height + 2);
//  OffsetRect(r, 0, r.Height + 2);
//  DrawSkinIndicator(siHover);
//
//
//
//  rFrame.BottomRight := r.BottomRight;
//  InflateRect(rFrame, 2, 2);
//  OffsetRect(rFrame, -1, -1);
//  canvas.Pen.Color := clBlue;
//  Canvas.MoveTo(rFrame.Left, rFrame.Top);
//  Canvas.LineTo(rFrame.Right, rFrame.Top);
//  Canvas.LineTo(rFrame.Right, rFrame.Bottom);
//  Canvas.LineTo(rFrame.Left, rFrame.Bottom);
//  Canvas.LineTo(rFrame.Left, rFrame.Top);


  cCtrl := FindVCLWindow(FToolbar.ClientToScreen(Point(10, 10)));
  if cCtrl = nil then
    PrintMsg('FToolbar : nil')
  else
    PrintMsg('FToolbar : %s', [cCtrl.ClassName]);

  cCtrl := FindVCLWindow(ToolBar1.ClientToScreen(Point(10, 10)));
  if cCtrl = nil then
    PrintMsg('ToolBar1 : nil')
  else
    PrintMsg('ToolBar1 : %s', [cCtrl.ClassName]);

  cCtrl := FindVCLWindow(ToolButton1.ClientToScreen(Point(10, 10)));
  if cCtrl = nil then
    PrintMsg('ToolButton1 : nil')
  else
    PrintMsg('ToolButton1 : %s', [cCtrl.ClassName]);


end;



end.
