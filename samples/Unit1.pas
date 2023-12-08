unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, System.Rtti, Vcl.Menus;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FVM: Pointer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses
  Gravity.GVIntf, Gravity.GVClasses, Contnrs;

{$R *.dfm}

type
  TExampleBase = class(TGravityWrapperBase)
  protected
    function ProceedInternal(const a: integer; const b: integer): integer; virtual; abstract;
  published
    function Proceed(const a: integer; const b: integer): integer;
  end;

  TExampleMul = class(TExampleBase)
  protected
    function ProceedInternal(const a: integer; const b: integer): integer; override;
  published
    function mul(const a: integer; const b: integer): integer;
  end;

  TExampleDec = class(TExampleBase)
  protected
    function ProceedInternal(const a: integer; const b: integer): integer; override;
  published
    function dec(const a: integer; const b: integer): integer;
  end;

var
  ADelegate: gravity_delegate_t;
  gravity_class_win: Pgravity_class_t = nil;
  reff_class_win: integer = 0;
  APtrList: Array of PAnsiChar;

function gravity_LogMesage(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  orig,
  value: gravity_value_t;
begin
  with GravityEng do
  begin
    // value := Pgravity_value_t(NativeUInt(args) + 1 * SizeOf(gravity_value_t))^;
    orig := GET_VALUE(args, 0);
    value := GET_VALUE(args, 1);
    Form1.Memo1.Lines.Add(String(AnsiString(VALUE_AS_STRING(value)^.s)));
    Result := True;
  end;
end;

procedure CreateOptWinClass(AIntf: IGVInterface; vm: Pgravity_vm);
var
  meta: Pgravity_class_t;
begin
  // here we create a simple foreign/native class:
  //
  // class Win {
  //    static func ShowMessage(msg) ...
  // }
  //
  // func main() {
  //    Win.ShowMessage("Hello World");
  // }
  with AIntf do
  begin
    if not Assigned(gravity_class_win) then
      gravity_class_win := gravity_class_new_pair(nil, 'Win', nil, 0, 0);
    RegisterGravityName('Win');
    meta := gravity_class_get_meta(gravity_class_win);
    gravity_class_bind(meta, 'LogMessage', NEW_CLOSURE_VALUE(gravity_LogMesage));
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
  SetLength(AFuncList, 2);
  AFuncList[0] :=  'Win';
  AFuncList[1] :=  'decobj';
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
  delegate: Pgravity_delegate_t;
  compiler: Pgravity_compiler_t;
  closure, fclosure: Pgravity_closure_t;
  val: gravity_value_t;
  ASrc: AnsiString;
  AOutput: Array[0..1024] of AnsiChar;
  AIntf: IGVInterface;
  AFunc, AOut: gravity_value_t;
  AParams: gravity_value_t_array;
  ADecObj: TExampleDec;
begin
(*
  ASrc := 'func mul(a, b) {return a * b} func main() {var a = 54; var b = 27; '+
    'Win.LogMessage("Hello World, this is from gravity"); '+
    'return Math.abs((a * b) * -1)}';
*)
  AIntf := GravityEng;
  with AIntf do
  begin
    delegate := gravity_vm_delegate(FVM);
    ASrc := 'func mul(a, b) {return a * b}';
    compiler := gravity_compiler_create(delegate);
    closure  := gravity_compiler_run(compiler, Asrc, length(Asrc), 0, True, True);
    gravity_compiler_transfer(compiler, FVM);
    gravity_compiler_free(compiler);
    gravity_vm_loadclosure(FVM, closure);

    // call gravity function from native
    AFunc := gravity_vm_getvalue(FVM, 'mul', length('mul'));
    if VALUE_ISA_CLOSURE(AFunc) then
    begin
      fclosure := VALUE_AS_CLOSURE(AFunc);
      SetLength(AParams, 2);
      AParams[0] := VALUE_FROM_INT(30);
      AParams[1] := VALUE_FROM_INT(40);
      try
        if gravity_vm_runclosure(FVM, fclosure, VALUE_FROM_NULL, @AParams[0], 2) then
        begin
          val := gravity_vm_result(FVM);
          gravity_value_dump(FVM, val, @AOutput, SizeOf(AOutput));
          Memo1.Lines.Add(Format('RESULT: %s %d',[AOutput, val.f2.n]));
        end;
      finally
        SetLength(AParams, 0);
      end;
    end;

    ASrc := 'func main() {var a = 54; var b = 27; '+
    'Win.LogMessage("Hello World, this is from gravity"); '+
    'return Math.abs((a * b) * -1)}';
    compiler := gravity_compiler_create(delegate);
    closure  := gravity_compiler_run(compiler, Asrc, length(Asrc), 0, True, True);
    gravity_compiler_transfer(compiler, FVM);
    gravity_compiler_free(compiler);
    gravity_vm_loadclosure(FVM, closure);

    // call native function from gravity
    if (gravity_vm_runmain(FVM, closure)) then
    begin
      val := gravity_vm_result(FVM);
      gravity_value_dump(FVM, val, @AOutput, SizeOf(AOutput));
      Memo1.Lines.Add(Format('RESULT: %s %d',[AOutput, val.f2.n]));
    end;

    ADecObj := TExampleDec.Create;
    DefineNativeObject(FVM, 'decobj', ADecObj);
    ASrc := 'func test_dec(a, b) {return decobj.Proceed(a, b);}';
    compiler := gravity_compiler_create(delegate);
    closure  := gravity_compiler_run(compiler, Asrc, length(Asrc), 0, True, True);
    gravity_compiler_transfer(compiler, FVM);
    gravity_compiler_free(compiler);
    gravity_vm_loadclosure(FVM, closure);

    // call gravity function from native
    AFunc := gravity_vm_getvalue(FVM, 'test_dec', length('test_dec'));
    if VALUE_ISA_CLOSURE(AFunc) then
    begin
      fclosure := VALUE_AS_CLOSURE(AFunc);
      SetLength(AParams, 2);
      AParams[0] := VALUE_FROM_INT(800);
      AParams[1] := VALUE_FROM_INT(725);
      try
        if gravity_vm_runclosure(FVM, fclosure, VALUE_FROM_NULL, @AParams[0], 2) then
        begin
          val := gravity_vm_result(FVM);
          gravity_value_dump(FVM, val, @AOutput, SizeOf(AOutput));
          Memo1.Lines.Add(Format('RESULT: %s %d',[AOutput, val.f2.n]));
        end;
      finally
        SetLength(AParams, 0);
      end;
    end;

    AFunc := gravity_vm_getvalue(FVM, 'mul', length('mul'));
    if VALUE_ISA_CLOSURE(AFunc) then
    begin
      fclosure := VALUE_AS_CLOSURE(AFunc);
      SetLength(AParams, 2);
      AParams[0] := VALUE_FROM_INT(25);
      AParams[1] := VALUE_FROM_INT(75);
      try
        if gravity_vm_runclosure(FVM, fclosure, VALUE_FROM_NULL, @AParams[0], 2) then
        begin
          val := gravity_vm_result(FVM);
          gravity_value_dump(FVM, val, @AOutput, SizeOf(AOutput));
          Memo1.Lines.Add(Format('RESULT: %s %d',[AOutput, val.f2.n]));
        end;
      finally
        SetLength(AParams, 0);
      end;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
//
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  AIntf: IGVInterface;
begin
  AIntf := GravityEng;
  with AIntf do
  begin
    CheckLibraryLoaded;
    FillChar(ADelegate, SizeOf(ADelegate), 0);
    ADelegate.error_callback := ErrorCallback;
    // ADelegate.optional_classes := OptClassCallBack;
    FVM := gravity_vm_new(@ADelegate);
    BindNativeClass(FVM);
    gravity_class_win := nil;
    CreateOptWinClass(AIntf, FVM);
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  AIntf: IGVInterface;
begin
  AIntf := GravityEng;
  with AIntf do
  begin
    UnbindNativeClass(FVM);
    DestroyOptWinClass(AIntf, Fvm);
    gravity_vm_free(Fvm);
    gravity_core_free;
  end;
end;

{ TExample }

function TExampleMul.mul(const a, b: integer): integer;
begin
  Result := a * b;
end;



{ TExampleBase }

function TExampleBase.Proceed(const a, b: integer): integer;
begin
  Result := ProceedInternal(a, b);
end;

function TExampleMul.ProceedInternal(const a, b: integer): integer;
begin
  Result := mul(a, b);
end;

{ TExampleDec }

function TExampleDec.dec(const a, b: integer): integer;
begin
  Result := a - b;
end;

function TExampleDec.ProceedInternal(const a, b: integer): integer;
begin
  Result := Dec(a, b);
end;

initialization
  RegisterGravityClasses([TExampleMul, TExampleDec]);

finalization
  UnRegisterGravityClasses([TExampleMul, TExampleDec]);

end.
