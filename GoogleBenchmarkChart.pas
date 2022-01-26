unit GoogleBenchmarkChart;

interface

uses
  System.Classes,
  FMXTee.Engine,
  FMXTee.Series,
  FMXTee.Chart;

const
  GoogleBenchmark_CSV='.csv';
  GoogleBenchmark_Text='.txt';

  BenchmarkField_Time = 0;
  BenchmarkField_CPU = 1;

procedure ClearChart(const AChart: TChart);
procedure GetCategoryRange(const AChart: TChart; out AMin, AMax:Double);

procedure LoadChart(const AChart: TChart; const AFile: String;
                    const ValueField: Integer;
                    const NumericCategory: Boolean);

procedure SaveChartImage(const AChart: TChart; const AStream: TStream; const AFormat: String); overload;
procedure SaveChartImage(const AChart: TChart; const AFile: String); overload;

implementation

uses
  System.IOUtils,
  FMX.Graphics,
  FMX.Surfaces,
  System.SysUtils;

procedure ClearChart(const AChart: TChart);
begin
  AChart.FreeAllSeries;  // remove all series and data from chart
  AChart.Title.Caption:= '';
end;

procedure LoadChart(const AChart: TChart; const AFile: String;
                    const ValueField: Integer;
                    const NumericCategory: Boolean);

  function FindSeries(const ATitle: String):TChartSeries;
  begin
    for var S in AChart.SeriesList do
        if S.Title=ATitle then
           Exit(S);

    result:=nil;
  end;

  function CreateNewSeries(const ATitle:String): TChartSeries;
  begin
    result:= AChart.AddSeries(TLineSeries);
    result.Title:= ATitle;

    // cosmetics
    (result as TCustomSeries).Pointer.Style:= TSeriesPointerStyle.psCircle;

    result.Marks.Style:= TSeriesMarksStyle.smsValue;
    result.Marks.Transparent:= True;

    result.VertAxis:= TVertAxis.aBothVertAxis;

    // Do not use line width to expand axes, as when moving the mouse over a Series
    // its width is changed.
    (result as TCustomSeries).InflateMargins:= False;
  end;

  procedure AddNewValue(const ATitle, ASize: String; const AValue: Double);
  var Series : TChartSeries;
  begin
    Series:= FindSeries(ATitle);  // BM_FillChar_CurRTL_A

    if Series=nil then
       Series:= CreateNewSeries(ATitle);

    if NumericCategory then
       Series.AddXY(StrToFloat(ASize), AValue)
    else
       Series.Add(AValue, ASize);
  end;

  // Example: S= FillChar/0/123
  // Output : Name= FillChart
  //          Category= 0/123
  procedure SplitCategory(const S: String; out Name,Category: String);
  var i : Integer;
  begin
    i:=Pos('/',S);

    if i>0 then
    begin
      Name:=Copy(S,1,i-1);
      Category:=Copy(S,i+1);
    end
    else
    begin
      // error ?
      Name:=S;
      Category:='';
    end;
  end;

  procedure ProcessCSV(const ALine: String);
  var Name, Category : String;
      Items : TArray<String>;

      Value : Double;
  begin
    Items:= ALine.Split([';'], TStringSplitOptions.ExcludeEmpty);

    // "FillChar/0";407272727;1,67494495645933;1,68805803684468;ns;0;;;;

    SplitCategory(Items[0].DequotedString('"'),Name,Category);   // "FillChar/0"

    if ValueField = BenchmarkField_Time then
       Value:= StrToFloat( Items[2] )
    else
       Value:= StrToFloat( Items[3] );

    AddNewValue(Name, Category, Value);
  end;

  procedure LoadCSV(const S:TStrings);
  begin
    // Skip 0 row (header)
    for var t:=1 to S.Count-1 do
        ProcessCSV(S[t]);
  end;

  procedure ProcessText(const ALine: String);
  var Name, Category : String;
      Items : TArray<String>;

      Size : String;
      Value : Double;
  begin
    Items:= ALine.Split([' '], TStringSplitOptions.ExcludeEmpty);

    // 'BM_FillChar_CurRTL_A/0', '1,64', 'ns', '1,64', 'ns', '448000000'

    if Length(Items)=6 then
    begin
      SplitCategory(Items[0], Name, Category);   // 'BM_FillChar_CurRTL_A/0'

      if ValueField = BenchmarkField_Time then
         Value:= StrToFloat( Items[1] )
      else
         Value:= StrToFloat( Items[3] );

      AddNewValue(Name, Category, Value);
    end;
  end;

  procedure LoadText(const S: TStrings);
  const
    Dashes = '----------';
  begin
    var DashesCount:=0;

    for var Line in S do
        if Copy(Line,1,10)= Dashes then
           Inc(DashesCount)
        else
        if DashesCount>1 then  // skip to second occurrence of dashed line
           ProcessText(Line);
  end;

  // Loop all string lines from AFile text file
  procedure AddValues;
  var S : TStrings;
  begin
    S:=TStringList.Create;
    try
      S.LoadFromFile(AFile);

      if TPath.GetExtension(AFile).ToLower = GoogleBenchmark_CSV then
         LoadCSV(S)
      else
         LoadText(S);
    finally
      S.Free;
    end;
  end;

  procedure SetChartTitles;
  var S: String;
  begin
    // TODO: Obtain time units (ns, msec etc) from file content

    if ValueField=BenchmarkField_Time then
       S:= 'Time ns'
    else
       S:= 'CPU ns';

    AChart.Axes.Left.Title.Text:= S;

    AChart.Title.Caption:= ExtractFileName(AFile);
  end;

begin
  ClearChart(AChart);

  AddValues;

  SetChartTitles;
end;

// Saves AChart as image into a stream.  AFormat is the extension: .png .jpg .bmp  ...
procedure SaveChartImage(const AChart: TChart; const AStream: TStream; const AFormat: String);
var S: TBitmapSurface;
    Bitmap: TBitmap;
begin
  Bitmap:= AChart.TeeCreateBitmap;
  try
    S:= TBitmapSurface.Create;
    try
      S.Assign(Bitmap);
      TBitmapCodecManager.SaveToStream(AStream, S, AFormat, nil {SaveParams});
    finally
      S.Free;
    end;
  finally
    Bitmap.Free;
  end;
end;

// Saves AChart as image into a file. AFile extension must be supported: .png .jpg .bmp  ...
procedure SaveChartImage(const AChart: TChart; const AFile: String);
var S : TFileStream;
begin
  S:= TFileStream.Create(AFile, fmCreate);
  try
    SaveChartImage(AChart, S, ExtractFileExt(AFile));
  finally
    S.Free;
  end;
end;

// Returns the minimum and maximum category values for all Series in AChart
procedure GetCategoryRange(const AChart: TChart; out AMin, AMax:Double);
var t : Integer;
    S : TChartSeries;
begin
  if AChart.SeriesCount=0 then
  begin
    AMax:= 0;
    AMin:= 0;
  end
  else
  begin
    AMax:= AChart[0].MaxXValue;
    AMin:= AChart[0].MinXValue;

    for t:= 1 to AChart.SeriesCount-1 do
    begin
      S:= AChart[t];

      if S.MaxXValue > AMax then
         AMax:= S.MaxXValue;

      if S.MinXValue < AMin then
        AMin:= S.MinXValue;
    end;
  end;
end;

end.
