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

  FMX.ListBox, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TFormViewer = class(TForm)
    Layout1: TLayout;
    ComboBoxFiles: TComboBox;
    ComboValue: TComboBox;
    ButtonSave: TButton;
    SaveDialog1: TSaveDialog;
    ShowPointers: TCheckBox;
    ShowMarks: TCheckBox;
    NumericCategory: TCheckBox;
    ButtonEditor: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ComboBoxFilesChange(Sender: TObject);
    procedure ComboValueChange(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure ShowPointersChange(Sender: TObject);
    procedure ShowMarksChange(Sender: TObject);
    procedure NumericCategoryChange(Sender: TObject);
    procedure ButtonEditorClick(Sender: TObject);
  private
    { Private declarations }

    Chart : TChart;
    ResultsPath : String;

    procedure ChartMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Single);
    procedure FillTextFiles(const APath:String);
    procedure RefreshChart;
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

procedure TFormViewer.ButtonSaveClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
     SaveChartImage(Chart,SaveDialog1.FileName);
end;

procedure TFormViewer.ComboBoxFilesChange(Sender: TObject);
begin
  RefreshChart;
end;

procedure TFormViewer.RefreshChart;
begin
  var FileName:= TPath.Combine(ResultsPath, ComboBoxFiles.Selected.Text);

  LoadChart(Chart, FileName, ComboValue.ItemIndex, NumericCategory.IsChecked);  // <-- fill chart with data

  // cosmetics
  Chart.Axes.Bottom.Title.Text:= 'Size';  // <-- name of tested variation

  ShowPointersChange(Self);
  ShowMarksChange(Self);
end;

procedure TFormViewer.FormCreate(Sender: TObject);

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
    begin
      ResultsPath:= TPath.GetDirectoryName(APath);
      TryAddFile(APath);
    end;

  finally
    ComboBoxFiles.EndUpdate;
  end;
end;

procedure TFormViewer.FormShow(Sender: TObject);
begin
  if ParamCount>0 then
  begin
    FillTextFiles(ParamStr(1));

    // Select first file
    if ComboBoxFiles.Count>0 then
       ComboBoxFiles.ItemIndex:=0;
  end;

  {$IFDEF CHARTEDITOR}
  ButtonEditor.Visible:=True;
  {$ENDIF}
end;

procedure TFormViewer.NumericCategoryChange(Sender: TObject);
begin
  RefreshChart;
end;

procedure TFormViewer.ShowMarksChange(Sender: TObject);
begin
  for var S in Chart.SeriesList do
      S.Marks.Visible:= ShowMarks.IsChecked;
end;

procedure TFormViewer.ShowPointersChange(Sender: TObject);
begin
  for var S in Chart.SeriesList do
      if S is TCustomSeries then
         TCustomSeries(S).Pointer.Visible:= ShowPointers.IsChecked;
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
