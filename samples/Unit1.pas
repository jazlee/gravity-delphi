unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  Gravity.GVIntf;

{$R *.dfm}

var
  gravity_class_win: Pgravity_class_t = nil;
  reff_class_win: integer = 0;

procedure CreateOptWinClass(AIntf: IGVInterface);
var
  AModName: AnsiString;
  meta: Pgravity_class_t;
begin
  with AIntf do
  begin
    if not Assigned(gravity_class_win) then
      gravity_class_win := gravity_class_new_pair(nil, 'Win', nil, 0, 0);
      meta := gravity_class_get_meta(gravity_class_win);
  end;
end;

procedure DestroyOptWinClass(AIntf: IGVInterface);
begin

end;

procedure TForm1.Button1Click(Sender: TObject);
var
  delegate: gravity_delegate_t;
  compiler: Pgravity_compiler_t;
  closure: Pgravity_closure_t;
  vm: Pgravity_vm;
  val: gravity_value_t;
  ASrc: AnsiString;
  AOutput: Array[0..1024] of AnsiChar;
  AIntf: IGVInterface;
begin
  ASrc := 'func main() {var a = 54; var b = 27; return a * b}';
  FillChar(delegate, SizeOf(delegate), 0);
  AIntf := GravityEng;
  with AIntf do
  begin
    CheckLibraryLoaded;
    compiler := gravity_compiler_create(@delegate);
    closure  := gravity_compiler_run(compiler, Asrc, length(Asrc), 0, True, True);
    vm := gravity_vm_new(@delegate);
    gravity_compiler_transfer(compiler, vm);
    gravity_compiler_free(compiler);
    if (gravity_vm_runmain(vm, closure)) then
    begin
      val := gravity_vm_result(vm);
      gravity_value_dump(vm, val, @AOutput, SizeOf(AOutput));
      ShowMessage(Format('RESULT: %s %d',[AOutput, val.f2.n]));
    end;
    gravity_vm_free(vm);
    gravity_core_free;
  end;
end;

end.
