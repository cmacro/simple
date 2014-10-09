program MergeRes;


{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,
  Classes,
  Vcl.Graphics,
  System.SysUtils,
  Vcl.Imaging.pngimage,
  ZLib;

const
  MSG_NONAMES = '没有资源图标文件名称列表';

type
  TPrintProc = procedure (const AVal: string) of object;

  TDataType = (dtIconMerge, dtPngPack);

  TParams = class
  private
    FileName: string;
    OutFileName: string;
    Kind: TDataType;

    function ReadFileName: Boolean;
  end;

  TConvertRes = class
  private
    FParams: TParams;
    FIconMap: TBitmap;
    FOnLogEvent: TPrintProc;
    function GetSourceFile: string;
    procedure BuildMap(w, h:Integer);
    procedure AddLog(const AMsg: string);
  public
    destructor Destroy; override;
    constructor Create(AFiles: TParams); virtual;

    function Exec(PrintMsg: TPrintProc): Boolean; virtual; abstract;

    property SourceFile: string read GetSourceFile;
    property ResMap: TBitmap read FIconMap;
    property OnLogEvent: TPrintProc read FOnLogEvent write FOnLogEvent;
  end;

  TPngPack = class(TConvertRes)
  public
    function Exec(PrintMsg: TPrintProc): Boolean; override;
  end;

  TMergeIcons = class(TConvertRes)
  private
    FIcon: TPngImage;
    FRowCnt: Integer;
    FColCnt: Integer;
    FFiles: TStringList;
    FWidth: integer;
    FHeight: integer;

    procedure BuildResMap;
    function GetCount: Integer;
    function GetFileNames(Index: Integer): string;
    function LoadIcon(AIndex: Integer): Boolean;
    function LoadImageNames: Boolean;
    function MergeIcon(AIndex: Integer): Boolean;
  public
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property FileNames[Index: Integer]: string read GetFileNames;

    function Exec(PrintMsg: TPrintProc): Boolean; override;
  end;

  TMergeSrv = class
  private
    FLog: TStringList;
    FDataFile: TParams;
    procedure PrintHelp;
    procedure PrintLog;
    procedure PrintMsg(const Afmt: string; const Args: array of const); overload;
    procedure PrintMsg(const AVal: string); overload;
    function  SaveResMap(ASource: TBitmap): Boolean;
    procedure DoOnAddLog(const AMsg: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Exec;
  end;

constructor TMergeSrv.Create;
begin
  FDataFile := TParams.Create;
  FLog := TStringList.Create;
end;

destructor TMergeSrv.Destroy;
begin
  FDataFile.free;
  FLog.free;
  inherited;
end;

procedure TMergeSrv.DoOnAddLog(const AMsg: string);
begin
  FLog.Add(AMsg);
end;

procedure TMergeSrv.Exec;
var
  cConvert: TConvertRes;
begin
  if FDataFile.ReadFileName then
  begin

    case FDataFile.Kind of
      dtIconMerge : cConvert := TMergeIcons.Create(FDataFile);
      dtPngPack   : cConvert := TPngPack.Create(FDataFile);
      else          cConvert := nil;
    end;

    if cConvert <> nil then
    begin
      cConvert.OnLogEvent := DoOnAddLog;
      try
        PrintMsg('');
        PrintMsg('Start %s', [FDataFile.FileName]);
        //PrintMsg('_______________________________________');
        if cConvert.Exec(PrintMsg) then
          if SaveResMap(cConvert.FIconMap) then
          begin
            PrintMsg('_______________________________________');
            PrintMsg(format('Finish: %s',[ChangeFileExt(FDataFile.OutFileName, '.IconPack')]));
            PrintLog;
          end;
      finally
        cConvert.Free;
      end;
    end
    else
      PrintMsg('Err: ' + MSG_NONAMES);
  end
  else
    PrintHelp;
end;

procedure TMergeSrv.PrintHelp;
begin
  // TODO -cMM: TMergeSrv.PrintHelp default body inserted
end;

procedure TMergeSrv.PrintLog;
var
  I: Integer;
begin
  for I := 0 to FLog.Count - 1 do
    PrintMsg(FLog[i]);
end;

procedure TMergeSrv.PrintMsg(const Afmt: string; const Args: array of const);
begin
  PrintMsg(format(Afmt, Args));
end;

procedure TMergeSrv.PrintMsg(const AVal: string);
begin
  Writeln(AVal);
end;

function TMergeSrv.SaveResMap(ASource: TBitmap): Boolean;
var
  cData: TMemoryStream;
  cPack: TZCompressionStream;
begin
  Result := False;
  if ASource = nil then
    Exit;

  if not DirectoryExists(ExtractFilePath(FDataFile.OutFileName)) then
    if not CreateDir(ExtractFilePath(FDataFile.OutFileName)) then
      Exit;


  cData := TMemoryStream.Create;
  try
    ASource.SaveToStream(cData);

    cData.SaveToFile(FDataFile.OutFileName);
    cData.Clear;

    cPack := TZCompressionStream.Create(clMax, cData);
    try
      ASource.SaveToStream(cPack);
    finally
      cPack.free;
    end;
    cData.SaveToFile(ChangeFileExt(FDataFile.OutFileName, '.IconPack'));

  finally
    cData.Free;
  end;
  Result := True;
end;

function TParams.ReadFileName: Boolean;
var
  sFileName: string;
  sPath: string;
begin
  FileName := '';

  sFileName := ChangeFileExt(ParamStr(0), '.lst');
  if ParamCount >= 1 then
    sFileName := Trim(ParamStr(1));
  if FileExists(sFileName) then
    FileName := sFileName;

  OutFileName := ChangeFileExt(FileName, '.bmp');
  if ParamCount >= 2 then
  begin
    sFileName := Trim(ParamStr(2));
    if (sFileName <> '') then
    begin
      if (sFileName[Length(sFileName)] = '\') then
        OutFileName := Format('%s%s',[sFileName, ExtractFileName(OutFileName)])
      else
      begin
        OutFileName := sFileName;
        if not DirectoryExists(ExtractFilePath(sFileName)) then
          if not CreateDir(ExtractFilePath(sFileName)) then
            OutFileName := '';
      end;
    end;
  end;

  if OutFileName <> '' then
    OutFileName := ExpandFileName(OutFileName);

  /// 设置当前处理目录
  if FileName <> '' then
  begin
    sPath := ExtractFilePath(FileName);
    SetCurrentDir(sPath);
    FileName := ExtractFileName(FileName);
  end;

  if SameText(ExtractFileExt(FileName), '.lst') then
    Kind := dtIconMerge
  else
    Kind := dtPngPack;

  Result := (FileName <> '') and (OutFileName <> '');
end;

procedure TMergeIcons.BuildResMap;
var
  bExists: Boolean;
  I: Integer;
begin
  // 预读文件尺寸
  FIcon := TPngImage.Create;
  bExists := False;
  for I := 0 to Count - 1 do
  begin
    bExists := LoadIcon(0);
    if bExists then
      Break;
  end;

  if not bExists then
    Exit;

  FColCnt := 10;
  FRowCnt := Count div FColCnt;
  if Count mod FColCnt > 0 then
    inc(FRowCnt);

  FWidth := FIcon.Width;
  FHeight:= FIcon.Height;

  BuildMap(FWidth * FColCnt, FHeight * FRowCnt);
end;

destructor TMergeIcons.Destroy;
begin
  if FFiles <> nil then FFiles.Free;
  if FIcon <> nil then  FIcon.free;
  inherited;
end;

function TMergeIcons.Exec(PrintMsg: TPrintProc): Boolean;
var
  I: Integer;
  iErrCnt: integer;
begin
  Result := False;
  iErrCnt := 0;
  if LoadImageNames then
  begin
    BuildResMap;

    for I := 0 to Count - 1 do
    begin
      if LoadIcon(i) then
      begin
        MergeIcon(i);
        PrintMsg(format('ok：并入资源（%d）%s', [i, FileNames[i]]));
      end
      else
      begin
        PrintMsg(format('Err: 无法加载 (%d)%s 文件', [i, FileNames[i]]));
        AddLog(format('Err: 无法加载 (%d)%s 文件', [i, FileNames[i]]));
        inc(iErrCnt);
      end;
    end;

    if iErrCnt > 0 then
      AddLog(format('合并：%d ,%d 个文件无法正常合并', [Count, iErrCnt]));

    Result := True;
  end
  else
    PrintMsg('Err: ' + MSG_NONAMES);
end;

function TMergeIcons.GetCount: Integer;
begin
  Result := FFiles.Count;
end;

function TMergeIcons.GetFileNames(Index: Integer): string;
begin
  Result := FFiles[Index];
end;

function TMergeIcons.LoadIcon(AIndex: Integer): Boolean;
begin
  try
    Result := False;
    if FileExists(FileNames[AIndex]) then
    begin
      FIcon.LoadFromFile(FileNames[AIndex]);
      Result := not FIcon.Empty;
    end;
  except
    Result := False;
  end;
end;

function TMergeIcons.LoadImageNames: Boolean;
var
  I: Integer;
  sVal: string;
  cDatas: TStringList;
  iCommentPos: Integer;
  idx: Integer;
begin
  if not Assigned(FFiles) then
    FFiles := TStringList.Create;

  cDatas := TStringList.Create;
  try
    cDatas.LoadFromFile(SourceFile);
    for I := 0 to cDatas.Count - 1 do
    begin
      sVal := Trim(cDatas[i]);

      // 去除注释行和空白行
      if (sVal = '') or (sVal[1] = ';') or (sVal[1] = '/') then
        sVal := '';

      /// 清除后面的注释和空格
      iCommentPos := 0;
      if sVal <> '' then
        for idx := 1 to Length(sVal) do
          if CharInSet(sVal[idx], [#9, ',', ';']) then
          begin
            iCommentPos := idx;
            Break;
          end;
      if iCommentPos > 0 then
      begin
        while (iCommentPos - 1 > 0) and (sVal[iCommentPos - 1] = ' ') do
          dec(iCommentPos);
        Delete(sVal, iCommentPos, Length(sVal));
      end;

      if sVal <> '' then
        FFiles.Add(sVal);
    end;

  finally
    cDatas.Free;
  end;
  Result := FFiles.Count > 0;
end;

function TMergeIcons.MergeIcon(AIndex: Integer): Boolean;
var
  iCol: Integer;
  iRow: Integer;
begin
  Result := True;
  iRow := AIndex div FColCnt;
  iCol := AIndex mod FColCnt;

  FIconMap.Canvas.Draw(FWidth * iCol, FHeight * iRow, FIcon);
end;

var
  cSrv: TMergeSrv;

{ TPngPack }

function TPngPack.Exec(PrintMsg: TPrintProc): Boolean;
var
  cSrc: TPngImage;
begin
  Result := False;
  cSrc := TPngImage.Create;
  try
    cSrc.LoadFromFile(SourceFile);
    if not cSrc.Empty then
    begin
      BuildMap(cSrc.Width, cSrc.Height);
      ResMap.Canvas.Draw(0, 0, cSrc);
      Result := True;
    end;
  finally
    cSrc.Free
  end;
end;

{ TConvertRes }

procedure TConvertRes.AddLog(const AMsg: string);
begin
  if Assigned(FOnLogEvent) then
    FOnLogEvent(AMsg);
end;

procedure TConvertRes.BuildMap(w, h:Integer);
begin
  FIconMap := TBitmap.Create;
  FIconMap.PixelFormat := pf32bit;
  FIconMap.alphaFormat := afIgnored;
  FIconMap.SetSize(w, h);
  // Alpha 透明化
  FIconMap.Canvas.Brush.Color := clBlack;
  FIconMap.Canvas.FillRect(Rect(0, 0, FIconMap.Width, FIconMap.Height));
end;

constructor TConvertRes.Create(AFiles: TParams);
begin
  FParams := AFiles;
end;

destructor TConvertRes.Destroy;
begin
  if FIconMap <> nil then
    FIconMap.Free;
  inherited;
end;

function TConvertRes.GetSourceFile: string;
begin
  Result := FParams.FileName;
end;

begin
  ReportMemoryLeaksOnShutdown := True;
  cSrv := TMergeSrv.Create;
  try
    cSrv.Exec;
  finally
    cSrv.Free;
  end;
end.
