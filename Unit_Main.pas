unit Unit_Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  FMXTee.Chart,

  { To support runtime chart editing:

    1) Project Options -> Packages -> Runtime Packages -> Link with runtime packages : TRUE
    2) Add FMXTeeUI to runtime packages list
    3) Define CHARTEDITOR below
  }

  {.$DEFINE CHARTEDITOR} // <-- remove "." to enable

  {$IFDEF CHARTEDITOR}
  FMXTee.Editor.Chart,
  {$ENDIF}

  FMX.ListBox, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Menus,
  FMX.Objects;

type
  TFormViewer = class(TForm)
    Layout1: TLayout;
    ComboBoxFiles: TComboBox;
    ComboValue: TComboBox;
    SaveDialog1: TSaveDialog;
    ButtonEditor: TButton;
    MainMenu1: TMainMenu;
    OpenDialog1: TOpenDialog;
    MenuView: TMenuItem;
    MenuRefresh: TMenuItem;
    MenuFile: TMenuItem;
    MenuOpenFile: TMenuItem;
    MenuOpenFolder: TMenuItem;
    MenuSeparator: TMenuItem;
    MenuExit: TMenuItem;
    MenuSave: TMenuItem;
    MenuPointers: TMenuItem;
    MenuMarks: TMenuItem;
    MenuAsNumbers: TMenuItem;
    MenuViewSeparator: TMenuItem;
    TrackZoom: TTrackBar;
    TextZoom: TText;
    MenuAbout: TMenuItem;
    Text1: TText;
    ZoomReset: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboBoxFilesChange(Sender: TObject);
    procedure ComboValueChange(Sender: TObject);
    procedure ButtonEditorClick(Sender: TObject);
    procedure MenuPointersClick(Sender: TObject);
    procedure MenuMarksClick(Sender: TObject);
    procedure MenuAsNumbersClick(Sender: TObject);
    procedure MenuRefreshClick(Sender: TObject);
    procedure MenuSaveClick(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure MenuOpenFolderClick(Sender: TObject);
    procedure MenuOpenFileClick(Sender: TObject);
    procedure TrackZoomTracking(Sender: TObject);
    procedure MenuAboutClick(Sender: TObject);
    procedure ZoomResetClick(Sender: TObject);
  private
    { Private declarations }

    Chart : TChart;

    LastOpened,
    ResultsPath : String;

    procedure ChartMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Single);
    procedure FillTextFiles(const APath:String);
    procedure RefreshChart;
    procedure RefreshLastOpened;
    procedure RefreshMarks;
    procedure RefreshPointers;
  public
    { Public declarations }
  end;

var
  FormViewer: TFormViewer;

implementation

{$R *.fmx}

(*
  Usage:

  GoogleBenchmarkChart folder_or_single_file

  Supported formats:

    .txt files:

31/8/2021 12:37:19
Running C:\Users\david\Downloads\FillCharTest\FillCharTest.exe
Run on (8 X 4200,00 MHz CPU s)
CPU Caches:
  L1 Data 32 K (x4)
  L1 Instruction 32 K (x4)
  L2 Unified 256 K (x4)
  L3 Unified 8192 K (x1)
--------------------------------------------------------------------
Benchmark                          Time             CPU   Iterations
--------------------------------------------------------------------
BM_FillChar_CurRTL_U/0          1,62 ns         1,61 ns    407272727
BM_FillChar_Better_U/0          1,65 ns         1,61 ns    407272727
BM_FillChar_CurRTL_U/96         8,30 ns         8,37 ns     74666667
BM_FillChar_Better_U/96         3,59 ns         3,61 ns    194782609
.....

    .csv files:

name;iterations;real_time;cpu_time;time_unit;bytes_per_second;items_per_second;label;error_occurred;error_message
"FillChar/0";407272727;1,67494495645933;1,68805803684468;ns;0;;;;
"FillChar/1";373333333;1,76208750161731;1,75781250156948;ns;568888888,380952;;;;
"FillChar/2";373333333;1,80307821592457;1,79966518017827;ns;1111317828,46512;;;;
.....

*)

uses
  System.IOUtils,
  FMXTee.Engine,
  FMXTee.Series,
  GoogleBenchmarkChart;

procedure TFormViewer.ButtonEditorClick(Sender: TObject);
begin
  {$IFDEF CHARTEDITOR}
  TChartEditForm.Edit(Self, Chart);
  {$ENDIF}
end;

procedure TFormViewer.ComboBoxFilesChange(Sender: TObject);
begin
  RefreshChart;
end;

procedure TFormViewer.RefreshChart;
begin
  if ComboBoxFiles.Selected=nil then
     Exit;

  var FileName:= TPath.Combine(ResultsPath, ComboBoxFiles.Selected.Text);

  LoadChart(Chart, FileName, ComboValue.ItemIndex, MenuAsNumbers.IsChecked);  // <-- fill chart with data

  // cosmetics
  Chart.Axes.Bottom.Title.Text:= 'Size';  // <-- name of tested variation

  RefreshPointers;
  RefreshMarks;

  TrackZoom.Value:= 0;
end;

procedure TFormViewer.FormCreate(Sender: TObject);

  procedure SetupOpenDialog;
  begin
    OpenDialog1.Title:= 'Open Google Benchmark results file';
    OpenDialog1.Filter:= 'Text files (*.txt)|*.txt|CSV files (*.csv)|*.csv|All files (*.txt;*.csv)|*.txt;*.csv';
    OpenDialog1.DefaultExt:='*.csv';
  end;

  procedure SetupSaveDialog;
  begin
    SaveDialog1.Title:= 'Save Chart';
    SaveDialog1.Options:= SaveDialog1.Options + [TOpenOption.ofOverwritePrompt];

    SaveDialog1.Filter:= TBitmapCodecManager.GetFilterString;
  end;

begin
  // Create chart control
  Chart:= TChart.Create(Self);
  Chart.Align:= TAlignLayout.Client;
  Chart.Parent:= Self;

  // cosmetics
  Chart.Color:= TAlphaColorRec.White;
  Chart.View3D:= False;
  Chart.Legend.Alignment:= TLegendAlignment.laBottom;

  Chart.OnMouseMove:= ChartMouseMove;

  SetupOpenDialog;
  SetupSaveDialog;

  ButtonEditor.Visible:=False;
end;

procedure TFormViewer.ComboValueChange(Sender: TObject);
begin
  RefreshChart;
end;

// Add all text files found in APath, into ComboBoxFiles
procedure TFormViewer.FillTextFiles(const APath:String);

  procedure TryAddFile(const AFile:String);
  begin
    var Extension:= TPath.GetExtension(AFile).ToLower;

    if (Extension=GoogleBenchmark_Text) or (Extension=GoogleBenchmark_CSV) then
       ComboBoxFiles.Items.Add( TPath.GetFileName(AFile) );
  end;

  procedure AddFolder(const AFolder:String);
  begin
    for var S in TDirectory.GetFiles(AFolder) do
        TryAddFile(S);
  end;

begin
  ComboBoxFiles.BeginUpdate;
  try
    ComboBoxFiles.Clear;

    if TDirectory.Exists(APath) then
    begin
      ResultsPath:= APath;
      AddFolder(APath);
    end
    else
    if TFile.Exists(APath) then
    begin
      ResultsPath:= TPath.GetDirectoryName(APath);
      TryAddFile(APath);
    end
    else
      ResultsPath:='';

  finally
    ComboBoxFiles.EndUpdate;
  end;
end;

procedure TFormViewer.FormShow(Sender: TObject);
begin
  if ParamCount>0 then
     LastOpened:=ParamStr(1)
  else
     LastOpened:=TDirectory.GetCurrentDirectory;

  RefreshLastOpened;

  {$IFDEF CHARTEDITOR}
  ButtonEditor.Visible:=True;
  {$ENDIF}
end;

procedure TFormViewer.MenuAboutClick(Sender: TObject);
const CRLF=#13#10#13#10;
begin
  ShowMessage('Usage: GoogleBenchmarkViewer.exe [folder or file]'+CRLF+
              '  If no parameter is specified, the current folder is scanned.'+CRLF+CRLF+
              'Tips:'+CRLF+
              '       Drag mouse right button to scroll.'+CRLF+
              'Sources:'+CRLF+
              '        https://github.com/davidberneda/GoogleBenchmarkViewer')
end;

procedure TFormViewer.MenuAsNumbersClick(Sender: TObject);
begin
  RefreshChart;
end;

procedure TFormViewer.MenuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormViewer.RefreshMarks;
begin
  for var S in Chart.SeriesList do
      S.Marks.Visible:= MenuMarks.IsChecked;
end;

procedure TFormViewer.MenuMarksClick(Sender: TObject);
begin
  RefreshMarks;
end;

procedure TFormViewer.MenuOpenFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    LastOpened:=OpenDialog1.FileName;
    RefreshLastOpened;
  end;
end;

procedure TFormViewer.MenuOpenFolderClick(Sender: TObject);
var Current,
    Directory : String;
begin
  Directory:='';

  if TDirectory.Exists(LastOpened) then
     Current:= LastOpened
  else
     Current:= '';

  if SelectDirectory('Open folder with Google Benchmark result files',Current,Directory) then
  begin
    LastOpened:=Directory;
    RefreshLastOpened;
  end;
end;

procedure TFormViewer.RefreshPointers;
begin
  for var S in Chart.SeriesList do
      if S is TCustomSeries then
         TCustomSeries(S).Pointer.Visible:= MenuPointers.IsChecked;
end;

procedure TFormViewer.TrackZoomTracking(Sender: TObject);

  procedure ApplyZoom(const Range: Double);
  var CurrentMin : Double;
  begin
    CurrentMin:= Chart.Axes.Bottom.Minimum;
    Chart.Axes.Bottom.SetMinMax(CurrentMin, CurrentMin+ Range)
  end;

var Zoom : Integer;
    Range,
    Min, Max : Double;
begin
  Zoom:= Round(TrackZoom.Value);
  TextZoom.Text:= Zoom.ToString+' %';

  GetCategoryRange(Chart, Min, Max);

  if (Zoom>0) and (Max<>0) then
  begin
    Range:= (Max-Min) / ((0.01*Zoom)+1);
    ApplyZoom(Range);
  end
  else
  begin
    Chart.UndoZoom;
    Chart.Axes.Bottom.Automatic:= True;
  end;

  ZoomReset.Enabled:= Zoom>0;
end;

procedure TFormViewer.ZoomResetClick(Sender: TObject);
begin
  TrackZoom.Value:= 0;
end;

procedure TFormViewer.MenuPointersClick(Sender: TObject);
begin
  RefreshPointers;
end;

procedure TFormViewer.RefreshLastOpened;

  function FindItem(const AText:String):Integer;
  begin
    for var t:= 0 to ComboBoxFiles.Count-1 do
        if SameText(ComboBoxFiles.Items[t],AText) then
           Exit(t);

    result:= -1;
  end;

var Old : String;
begin
  ClearChart(Chart);

  if ComboBoxFiles.Selected=nil then
     Old:= ''
  else
     Old:= ComboBoxFiles.Selected.Text;

  FillTextFiles(LastOpened);

  // Select previous file
  ComboBoxFiles.ItemIndex:= FindItem(Old);

  // Select first file if any selected
  if ComboBoxFiles.ItemIndex = -1 then
     if ComboBoxFiles.Count>0 then
        ComboBoxFiles.ItemIndex:=0;

  ComboValue.Enabled:= ComboBoxFiles.ItemIndex <> -1;
end;

procedure TFormViewer.MenuRefreshClick(Sender: TObject);
begin
  if LastOpened<>'' then
     RefreshLastOpened;
end;

procedure TFormViewer.MenuSaveClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
     SaveChartImage(Chart,SaveDialog1.FileName);
end;

// Show tooltips (hints)
procedure TFormViewer.ChartMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
var Index : Integer;
    S : TChartSeries;
begin
  S:= Chart.SeriesList.Clicked(Round(X), Round(Y), Index);

  if S=nil then
     Chart.Hint:=''
  else
     Chart.Hint:= S.YValue[Index].ToString;
end;

end.
