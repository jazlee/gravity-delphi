program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Gravity.GVClasses in '..\source\Gravity.GVClasses.pas',
  Gravity.GVSysUtils in '..\source\Gravity.GVSysUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
