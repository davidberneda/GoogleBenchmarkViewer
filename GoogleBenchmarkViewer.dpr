program GoogleBenchmarkViewer;

uses
  System.StartUpCopy,
  FMX.Forms,
  Unit_Main in 'Unit_Main.pas' {FormViewer},
  GoogleBenchmarkChart in 'GoogleBenchmarkChart.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormViewer, FormViewer);
  Application.Run;
end.
