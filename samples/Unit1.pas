unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
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
    APtrList: Array of PAnsiChar;

function gravity_ShowMessage(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  value: gravity_value_t;
begin
  with GravityEng do
  begin
    value := Pgravity_value_t(NativeUInt(args) + 1 * SizeOf(gravity_value_t))^;
    Form1.Memo1.Lines.Add(String(AnsiString(VALUE_AS_STRING(@value)^.s)));
    Result := True;
  end;
end;

procedure CreateOptWinClass(AIntf: IGVInterface; vm: Pgravity_vm);
var
  meta: Pgravity_class_t;
begin
  with AIntf do
  begin
    if not Assigned(gravity_class_win) then
      gravity_class_win := gravity_class_new_pair(nil, 'Win', nil, 0, 0);
    meta := gravity_class_get_meta(gravity_class_win);
    gravity_class_bind(meta, 'ShowMessage', NEW_CLOSURE_VALUE(gravity_ShowMessage));
    meta.is_inited := True;
    gravity_vm_setvalue(vm, 'Win', VALUE_FROM_OBJECT(gravity_class_win));
  end;
end;

procedure DestroyOptWinClass(AIntf: IGVInterface; vm: Pgravity_vm);
var
  meta: Pgravity_class_t;
begin
  if Assigned(gravity_class_win) then
  with AIntf do
  begin
    meta := gravity_class_get_meta(gravity_class_win);
    gravity_class_free_core(nil, meta);
    gravity_class_free_core(nil, gravity_class_win);
    gravity_class_win := nil;
  end;
end;

function OptClassCallBack(xdata: Pointer): PPAnsiChar; cdecl;
var
  AFuncList: Array of AnsiString;
  I: integer;
begin
  SetLength(AFuncList, 1);
  AFuncList[0] :=  'Win';
  SetLength(APtrList, Length(AFuncList));
  for I := 0 to Length(AFuncList) - 1 do
    APtrList[I] := PAnsiChar(AFuncList[I]);
  Result := @APtrList[0];
end;

procedure ErrorCallback(vm: Pgravity_vm; error_type: error_type_t;
  const description: PAnsiChar; error_desc: error_desc_t; xdata: Pointer); cdecl;
begin
  Form1.Memo1.Lines.Add(String(AnsiString(description)))
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  delegate: gravity_delegate_t;
  compiler: Pgravity_compiler_t;
  closure, fclosure: Pgravity_closure_t;
  vm: Pgravity_vm;
  val: gravity_value_t;
  ASrc: AnsiString;
  AOutput: Array[0..1024] of AnsiChar;
  AIntf: IGVInterface;
  AMulFunc, AOut: gravity_value_t;
  AParams: gravity_value_t_array;
begin
  ASrc := 'func mul(a, b) {return a * b} func main() {var a = 54; var b = 27; '+
    'Win.ShowMessage("Hello World, this is from gravity"); '+
    'return Math.abs((a * b) * -1)}';
  FillChar(delegate, SizeOf(delegate), 0);
  AIntf := GravityEng;
  with AIntf do
  begin
    CheckLibraryLoaded;
    delegate.error_callback := ErrorCallback;
    delegate.optional_classes := OptClassCallBack;
    vm := gravity_vm_new(@delegate);
    CreateOptWinClass(AIntf, vm);
    compiler := gravity_compiler_create(@delegate);
    gravity_class_win := nil;
    closure  := gravity_compiler_run(compiler, Asrc, length(Asrc), 0, True, True);
    gravity_compiler_transfer(compiler, vm);
    gravity_compiler_free(compiler);
    gravity_vm_loadclosure(vm, closure);

    // call gravity function from native
    AMulFunc := gravity_vm_getvalue(vm, 'mul', length('mul'));
    if VALUE_ISA_CLOSURE(AMulFunc) then
    begin
      fclosure := VALUE_AS_CLOSURE(@AMulFunc);
      SetLength(AParams, 2);
      AParams[0] := VALUE_FROM_INT(30);
      AParams[1] := VALUE_FROM_INT(40);
      try
        if gravity_vm_runclosure(vm, fclosure, VALUE_FROM_NULL, @AParams[0], 2) then
        begin
          val := gravity_vm_result(vm);
          gravity_value_dump(vm, val, @AOutput, SizeOf(AOutput));
          Memo1.Lines.Add(Format('RESULT: %s %d',[AOutput, val.f2.n]));
        end;
      finally
        SetLength(AParams, 0);
      end;
    end;

    // call native function from gravity
    if (gravity_vm_runmain(vm, closure)) then
    begin
      val := gravity_vm_result(vm);
      gravity_value_dump(vm, val, @AOutput, SizeOf(AOutput));
      Memo1.Lines.Add(Format('RESULT: %s %d',[AOutput, val.f2.n]));
    end;
    DestroyOptWinClass(AIntf, vm);
    gravity_vm_free(vm);
    gravity_core_free;
  end;
end;

end.
