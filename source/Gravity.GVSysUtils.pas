unit Gravity.GVSysUtils;

interface
uses
  System.SysUtils, Gravity.GVIntf, Gravity.GVClasses;


procedure GravitySysUtilsInit;
procedure GravitySysUtilsRegister(vm: Pgravity_vm);
procedure GravitySysUtilsFree;


function convert_value2DateTime(vm: Pgravity_vm; v: gravity_value_t): gravity_value_t;
function convert_value2Date(vm: Pgravity_vm; v: gravity_value_t): gravity_value_t;
function convert_value2Time(vm: Pgravity_vm; v: gravity_value_t): gravity_value_t;

implementation
uses
  System.AnsiStrings;

var
  GravitySysUtilsClass: Pgravity_class_t;

  GravityDateTimeClass: Pgravity_class_t;
  GravityTimeClass: Pgravity_class_t;
  GravityDateClass: Pgravity_class_t;

  reff_count: integer = 0;
  sysutils_initialized: boolean = False;


const
  GravitySysUtilsClassName: AnsiString = 'SysUtils';
  GravityDateTimeClassName: AnsiString = 'TDateTime';
  GravityDateClassName: AnsiString = 'TDate';
  GravityTimeClassName: AnsiString = 'TTime';


function VALUE_ISA_DATETIME(v: gravity_value_t): Boolean;
begin
  result := v.isa = GravityDateTimeClass;
end;

function VALUE_ISA_DATE(v: gravity_value_t): Boolean;
begin
  result := v.isa = GravityDateClass;
end;

function VALUE_ISA_TIME(v: gravity_value_t): Boolean;
begin
  result := v.isa = GravityTimeClass;
end;

function VALUE_FROM_DATETIME(v: TDateTime): gravity_value_t;
begin
  Result.isa := GravityDateTimeClass;
  Result.f2.f := v;
end;

function VALUE_FROM_DATE(v: TDateTime): gravity_value_t;
begin
  Result.isa := GravityDateClass;
  Result.f2.f := v;
end;

function VALUE_FROM_TIME(v: TDateTime): gravity_value_t;
begin
  Result.isa := GravityTimeClass;
  Result.f2.f := v;
end;

function convert_object_datetime(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AVal: gravity_value_t;
begin
  with GravityEng do
  begin
    AVal := convert_value2DateTime(vm, GET_VALUE(args, 0));
    if VALUE_ISA_NOTVALID(AVal) then
    begin
      gravity_fiber_seterror(gravity_vm_fiber(vm), PAnsiChar(AnsiString('Unable to convert object to DateTime')));
      gravity_vm_setslot(vm, VALUE_FROM_NULL, rindex);
      Result := False
    end else
    begin
      gravity_vm_setslot(vm, AVal, rindex);
      Result := True;
    end;
  end;
end;

function convert_object_date(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AVal: gravity_value_t;
begin
  with GravityEng do
  begin
    AVal := convert_value2Date(vm, GET_VALUE(args, 0));
    if VALUE_ISA_NOTVALID(AVal) then
    begin
      gravity_fiber_seterror(gravity_vm_fiber(vm), PAnsiChar(AnsiString('Unable to convert object to Date')));
      gravity_vm_setslot(vm, VALUE_FROM_NULL, rindex);
      Result := False
    end else
    begin
      gravity_vm_setslot(vm, AVal, rindex);
      Result := True;
    end;
  end;
end;

function convert_object_time(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AVal: gravity_value_t;
begin
  with GravityEng do
  begin
    AVal := convert_value2Time(vm, GET_VALUE(args, 0));
    if VALUE_ISA_NOTVALID(AVal) then
    begin
      gravity_fiber_seterror(gravity_vm_fiber(vm), PAnsiChar(AnsiString('Unable to convert object to Time')));
      gravity_vm_setslot(vm, VALUE_FROM_NULL, rindex);
      Result := False
    end else
    begin
      gravity_vm_setslot(vm, AVal, rindex);
      Result := True;
    end;
  end;
end;

function convert_value2DateTime(vm: Pgravity_vm; v: gravity_value_t): gravity_value_t;
var
  AClosure: Pgravity_closure_t;
begin
  with GravityEng do
  begin
    if VALUE_ISA_DATETIME(v) then
      Result := v
    else if VALUE_ISA_DATE(v) then
      Result := VALUE_FROM_DATE(v.f2.f)
    else if VALUE_ISA_TIME(v) then
      Result := VALUE_FROM_DATE(v.f2.f)
    else if VALUE_ISA_FLOAT(v) then
      Result := VALUE_FROM_DATETIME(v.f2.f)
    else if VALUE_ISA_INT(v) then
      Result := VALUE_FROM_DATETIME(v.f2.n)
    else if VALUE_ISA_NULL(v) then
      Result := VALUE_FROM_DATETIME(0)
    else if VALUE_ISA_UNDEFINED(v) then
      Result := VALUE_FROM_DATETIME(0)
    else if VALUE_ISA_STRING(v) then
      Result := convert_value2float(vm, v);

    AClosure := gravity_class_lookup_closure(gravity_value_getclass(v),
      VALUE_FROM_STRING(vm, GravityDateTimeClassName, Length(GravityDateTimeClassName)));
    if (AClosure = nil) or ((AClosure^.f^.tag = EXEC_TYPE_INTERNAL) and (@AClosure^.f^.f10.internal = @convert_object_datetime)) then
    begin
      Result := VALUE_FROM_ERROR(nil);
      exit;
    end;
    if gravity_vm_runclosure(vm, AClosure, v, nil, 0) then
    begin
      Result := gravity_vm_result(vm);
      exit;
    end;
    Result := VALUE_FROM_ERROR(nil);
  end;
end;

function convert_value2Date(vm: Pgravity_vm; v: gravity_value_t): gravity_value_t;
var
  AClosure: Pgravity_closure_t;
begin
  with GravityEng do
  begin
    if VALUE_ISA_DATE(v) then
      Result := v
    else if VALUE_ISA_DATETIME(v) then
      Result := VALUE_FROM_DATE(v.f2.f)
    else if VALUE_ISA_TIME(v) then
      Result := VALUE_FROM_DATE(v.f2.f)
    else if VALUE_ISA_FLOAT(v) then
      Result := VALUE_FROM_DATE(v.f2.f)
    else if VALUE_ISA_INT(v) then
      Result := VALUE_FROM_DATE(v.f2.n)
    else if VALUE_ISA_NULL(v) then
      Result := VALUE_FROM_DATE(0)
    else if VALUE_ISA_UNDEFINED(v) then
      Result := VALUE_FROM_DATE(0)
    else if VALUE_ISA_STRING(v) then
      Result := convert_value2float(vm, v);

    AClosure := gravity_class_lookup_closure(gravity_value_getclass(v),
      VALUE_FROM_STRING(vm, GravityDateClassName, Length(GravityDateClassName)));
    if (AClosure = nil) or ((AClosure^.f^.tag = EXEC_TYPE_INTERNAL) and (@AClosure^.f^.f10.internal = @convert_object_date)) then
    begin
      Result := VALUE_FROM_ERROR(nil);
      exit;
    end;
    if gravity_vm_runclosure(vm, AClosure, v, nil, 0) then
    begin
      Result := gravity_vm_result(vm);
      exit;
    end;
    Result := VALUE_FROM_ERROR(nil);
  end;
end;

function convert_value2Time(vm: Pgravity_vm; v: gravity_value_t): gravity_value_t;
var
  AClosure: Pgravity_closure_t;
begin
  with GravityEng do
  begin
    if VALUE_ISA_TIME(v) then
      Result := v
    else if VALUE_ISA_DATE(v) then
      Result := VALUE_FROM_DATE(v.f2.f)
    else if VALUE_ISA_DATETIME(v) then
      Result := VALUE_FROM_DATE(v.f2.f)
    else if VALUE_ISA_FLOAT(v) then
      Result := VALUE_FROM_TIME(v.f2.f)
    else if VALUE_ISA_INT(v) then
      Result := VALUE_FROM_TIME(v.f2.n)
    else if VALUE_ISA_NULL(v) then
      Result := VALUE_FROM_TIME(0)
    else if VALUE_ISA_UNDEFINED(v) then
      Result := VALUE_FROM_TIME(0)
    else if VALUE_ISA_STRING(v) then
      Result := convert_value2float(vm, v);

    AClosure := gravity_class_lookup_closure(gravity_value_getclass(v),
      VALUE_FROM_STRING(vm, GravityTimeClassName, Length(GravityTimeClassName)));
    if (AClosure = nil) or ((AClosure^.f^.tag = EXEC_TYPE_INTERNAL) and (@AClosure^.f^.f10.internal = @convert_object_time)) then
    begin
      Result := VALUE_FROM_ERROR(nil);
      exit;
    end;
    if gravity_vm_runclosure(vm, AClosure, v, nil, 0) then
    begin
      Result := gravity_vm_result(vm);
      exit;
    end;
    Result := VALUE_FROM_ERROR(nil);
  end;
end;

function gravity_tdatetime_exec(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AVal : gravity_value_t;
  ABuf: AnsiString;
begin
  with GravityEng do
  begin
    if nargs <> 2 then
    begin
      gravity_fiber_seterror(gravity_vm_fiber(vm), PAnsiChar(AnsiString('A single argument is expected in DateTime casting.')));
      gravity_vm_setslot(vm, VALUE_FROM_NULL, rindex);
      Result := False
    end;
    AVal := convert_value2DateTime(vm, GET_VALUE(args, 1));
    if (VALUE_ISA_NOTVALID(AVal)) then
    begin
      gravity_fiber_seterror(gravity_vm_fiber(vm), PAnsiChar(AnsiString('Unable to convert object to DateTime')));
      gravity_vm_setslot(vm, VALUE_FROM_NULL, rindex);
      Result := False
    end else
    begin
      gravity_vm_setslot(vm, AVal, rindex);
      Result := True;
    end;
  end;
end;

function gravity_tdate_exec(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AVal : gravity_value_t;
  ABuf: AnsiString;
begin
  with GravityEng do
  begin
    if nargs <> 2 then
    begin
      gravity_fiber_seterror(gravity_vm_fiber(vm), PAnsiChar(AnsiString('A single argument is expected in Date casting.')));
      gravity_vm_setslot(vm, VALUE_FROM_NULL, rindex);
      Result := False
    end;
    AVal := convert_value2Date(vm, GET_VALUE(args, 1));
    if (VALUE_ISA_NOTVALID(AVal)) then
    begin
      gravity_fiber_seterror(gravity_vm_fiber(vm), PAnsiChar(AnsiString('Unable to convert object to Date')));
      gravity_vm_setslot(vm, VALUE_FROM_NULL, rindex);
      Result := False
    end else
    begin
      gravity_vm_setslot(vm, AVal, rindex);
      Result := True;
    end;
  end;
end;

function gravity_ttime_exec(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AVal : gravity_value_t;
  ABuf: AnsiString;
begin
  with GravityEng do
  begin
    if nargs <> 2 then
    begin
      gravity_fiber_seterror(gravity_vm_fiber(vm), PAnsiChar(AnsiString('A single argument is expected in Time casting.')));
      gravity_vm_setslot(vm, VALUE_FROM_NULL, rindex);
      Result := False
    end;
    AVal := convert_value2Time(vm, GET_VALUE(args, 1));
    if (VALUE_ISA_NOTVALID(AVal)) then
    begin
      gravity_fiber_seterror(gravity_vm_fiber(vm), PAnsiChar(AnsiString('Unable to convert object to Time')));
      gravity_vm_setslot(vm, VALUE_FROM_NULL, rindex);
      Result := False
    end else
    begin
      gravity_vm_setslot(vm, AVal, rindex);
      Result := True;
    end;
  end;
end;

function gravity_date(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
begin
  with GravityEng do
    Result := True;
end;

function gravity_time(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
begin
  with GravityEng do
    Result := True;
end;

function gravity_now(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
begin
  with GravityEng do
    Result := True;
end;

function gravity_encodedate(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
begin
  with GravityEng do
    Result := True;
end;

function gravity_encodetime(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
begin
  with GravityEng do
    Result := True;
end;

function gravity_decodedate(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
begin
  with GravityEng do
    Result := True;
end;

function gravity_decodetime(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
begin
  with GravityEng do
    Result := True;
end;


procedure GravitySysUtilsInit;
var
  AParentClass, AMeta: Pgravity_class_t;
begin
  if sysutils_initialized then
    exit;
  sysutils_initialized := True;
  with GravityEng do
  begin
    AParentClass := gravity_class_float;
    GravitySysUtilsClass := gravity_class_new_pair(nil, GravitySysUtilsClassName, nil, 0, 0);

    GravityDateTimeClass := gravity_class_new_pair(nil, GravityDateTimeClassName, AParentClass, 0, 0);
    AMeta := gravity_class_get_meta(GravityDateTimeClass);
    gravity_class_bind(AMeta, GRAVITY_INTERNAL_EXEC_NAME, NEW_CLOSURE_VALUE(gravity_tdatetime_exec));

    GravityDateClass := gravity_class_new_pair(nil, GravityDateClassName, GravityDateTimeClass, 0, 0);
    AMeta := gravity_class_get_meta(GravityDateClass);
    gravity_class_bind(AMeta, GRAVITY_INTERNAL_EXEC_NAME, NEW_CLOSURE_VALUE(gravity_tdate_exec));

    GravityTimeClass := gravity_class_new_pair(nil, GravityTimeClassName, GravityDateTimeClass, 0, 0);
    AMeta := gravity_class_get_meta(GravityTimeClass);
    gravity_class_bind(AMeta, GRAVITY_INTERNAL_EXEC_NAME, NEW_CLOSURE_VALUE(gravity_ttime_exec));

    AMeta := gravity_class_get_meta(GravitySysUtilsClass);
    gravity_class_bind(AMeta, 'Date', NEW_CLOSURE_VALUE(gravity_date));
    gravity_class_bind(AMeta, 'Time', NEW_CLOSURE_VALUE(gravity_time));
    gravity_class_bind(AMeta, 'Now', NEW_CLOSURE_VALUE(gravity_now));
    gravity_class_bind(AMeta, 'EncodeDate', NEW_CLOSURE_VALUE(gravity_encodedate));
    gravity_class_bind(AMeta, 'EncodeTime', NEW_CLOSURE_VALUE(gravity_encodetime));
    gravity_class_bind(AMeta, 'DecodeDate', NEW_CLOSURE_VALUE(gravity_decodedate));
    gravity_class_bind(AMeta, 'DecodeTime', NEW_CLOSURE_VALUE(gravity_decodetime));
    SETMETA_INITED(GravitySysUtilsClass);

    RegisterGravityName(GravitySysUtilsClassName);
    RegisterGravityName(GravityDateTimeClassName);
    RegisterGravityName(GravityDateClassName);
    RegisterGravityName(GravityTimeClassName);
  end;
end;

procedure GravitySysUtilsRegister(vm: Pgravity_vm);
begin
  GravitySysUtilsInit;
  Inc(reff_count);
  if vm <> nil then
  with GravityEng do
  begin
    if gravity_vm_ismini(vm) then
      exit;
    gravity_vm_setvalue(vm, GravitySysUtilsClassName, VALUE_FROM_OBJECT(GravitySysUtilsClass));
    gravity_vm_setvalue(vm, GravityDateTimeClassName, VALUE_FROM_OBJECT(GravityDateTimeClass));
    gravity_vm_setvalue(vm, GravityDateClassName, VALUE_FROM_OBJECT(GravityDateClass));
    gravity_vm_setvalue(vm, GravityTimeClassName, VALUE_FROM_OBJECT(GravityTimeClass));
  end;
end;

procedure GravitySysUtilsFree;
begin
  if not sysutils_initialized then
    exit;
  Dec(reff_count);
  if reff_count <> 0 then
    exit;
  with GravityEng do
  begin
    gravity_class_free(nil, gravity_class_get_meta(GravityTimeClass));
    gravity_class_free(nil, GravityTimeClass);
    gravity_class_free(nil, gravity_class_get_meta(GravityDateClass));
    gravity_class_free(nil, GravityDateClass);
    gravity_class_free(nil, gravity_class_get_meta(GravityDateTimeClass));
    gravity_class_free(nil, GravityDateTimeClass);
    gravity_class_free(nil, gravity_class_get_meta(GravitySysUtilsClass));
    gravity_class_free(nil, GravitySysUtilsClass);

    GravityTimeClass := nil;
    GravityDateClass := nil;
    GravityDateTimeClass := nil;
    GravitySysUtilsClass := nil;
  end;
  sysutils_initialized := False;
end;



end.
