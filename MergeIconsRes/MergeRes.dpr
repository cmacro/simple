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
    function GetSourceFile: string;
    procedure BuildMap(w, h:Integer);
  public
    destructor Destroy; override;
    constructor Create(AFiles: TParams); virtual;

    function Exec(PrintMsg: TPrintProc): Boolean; virtual; abstract;

    property SourceFile: string read GetSourceFile;
    property ResMap: TBitmap read FIconMap;
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
    FDataFile: TParams;
    procedure PrintHelp;
    procedure PrintMsg(const AVal: string);
    function  SaveResMap(ASource: TBitmap): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Exec;
  end;

constructor TMergeSrv.Create;
begin
  FDataFile := TParams.Create;
end;

destructor TMergeSrv.Destroy;
begin
  FDataFile.free;
  inherited;
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
      try
        if cConvert.Exec(PrintMsg) then
          if SaveResMap(cConvert.FIconMap) then
            PrintMsg(format('Finish: %s',[ChangeFileExt(FDataFile.OutFileName, '.IconPack')]));
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

  // 把资源压缩到内存流中
  cData := TMemoryStream.Create;
  try
    // 生成一份对照Bitmap文件，用户检测合并文件是否有问题。
    ASource.SaveToStream(cData);
    cData.SaveToFile(FDataFile.OutFileName);
    cData.Clear;

    // 生成资源使用的压缩包文件
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
  Result := False;
  FileName := '';

  // 从参数读取资源图标维护列表
  sFileName := ChangeFileExt(ParamStr(0), '.lst');
  if ParamCount >= 1 then
    sFileName := Trim(ParamStr(1));
  if FileExists(sFileName) then
    FileName := sFileName;

  // 从第二个参数中读取需要输出的资源包名称
  // 情景：1、没有第二个参数，默认使用配置文件名
  //       2、第二个参数是个路径，作为输出路径，文件名同配置名。
  //       3、有明确输出文件名，直接使用。
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

  // 把输出文件变成完整路径，为简化后续PNG资源的加载
  if OutFileName <> '' then
    OutFileName := ExpandFileName(OutFileName);

  /// 设置当前处理目录，为简化后续图标资源的加载
  if FileName <> '' then
  begin
    sPath := ExtractFilePath(FileName);
    SetCurrentDir(sPath);
    FileName := ExtractFileName(FileName);
  end;

  // 
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
  // 预读图标文件尺寸
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

  // 设置图标拼接行列数
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
begin
  Result := False;
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
        PrintMsg(format('Err: 无法加载 (%d)%s 文件', [i, FileNames[i]]));
    end;

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
begin
  FFiles := TStringList.Create;
  FFiles.LoadFromFile(SourceFile);
  for I := FFiles.Count - 1 downto 0 do
  begin
    sVal := Trim(FFiles[i]);
    if (sVal = '') or (sVal[1] = ';') or (sVal[1] = '/') then
      FFiles.Delete(i)
    else
      FFiles[i] := sVal;
  end;

  Result := FFiles.Count > 0;
end;

function TMergeIcons.MergeIcon(AIndex: Integer): Boolean;
var
  iCol: Integer;
  iRow: Integer;
begin
  Result := True;
  // 按照索引进行偏移并入
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
