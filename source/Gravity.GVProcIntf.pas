unit Gravity.GVProcIntf;

interface
uses
  Gravity.GVIntf;

function gravity_module_new(vm: Pgravity_vm; const identifier: AnsiString)
  : Pgravity_module_t;
procedure gravity_module_free(vm: Pgravity_vm; m: Pgravity_module_t);
procedure gravity_module_blacken(vm: Pgravity_vm; m: Pgravity_module_t);
function gravity_module_size(vm: Pgravity_vm; m: Pgravity_module_t): UInt32;

function gravity_bytecode_deserialize(const buffer: AnsiString;
  len: NativeUInt; ninst: PUInt32): PUInt32;

procedure gravity_function_blacken(vm: Pgravity_vm; f: Pgravity_function_t);
function gravity_function_cpool_add(vm: Pgravity_vm; f: Pgravity_function_t;
  v: gravity_value_t): UInt16;
function gravity_function_cpool_get(f: Pgravity_function_t; i: UInt16)
  : gravity_value_t;
function gravity_function_deserialize(vm: Pgravity_vm; json: Pjson_value)
  : Pgravity_function_t;
procedure gravity_function_dump(f: Pgravity_function_t;
  codef: code_dump_function);
procedure gravity_function_free(vm: Pgravity_vm; f: Pgravity_function_t);
function gravity_function_new(vm: Pgravity_vm; const identifier: AnsiString;
  nparams: UInt16; nlocals: UInt16; ntemps: UInt16; code: Pointer)
  : Pgravity_function_t;
function gravity_function_new_bridged(vm: Pgravity_vm;
  const identifier: AnsiString; xdata: Pointer): Pgravity_function_t;
function gravity_function_new_internal(vm: Pgravity_vm;
  const identifier: AnsiString; exec: gravity_c_internal; nparams: UInt16)
  : Pgravity_function_t;
function gravity_function_new_special(vm: Pgravity_vm;
  const identifier: AnsiString; index: UInt16; getter: Pointer;
  setter: Pointer): Pgravity_function_t;
procedure gravity_function_serialize(f: Pgravity_function_t; json: Pjson_t);
procedure gravity_function_setxdata(f: Pgravity_function_t; xdata: Pointer);
function gravity_function_size(vm: Pgravity_vm;
  f: Pgravity_function_t): UInt32;

procedure gravity_closure_blacken(vm: Pgravity_vm;
  closure: Pgravity_closure_t);
procedure gravity_closure_dec_refcount(vm: Pgravity_vm;
  closure: Pgravity_closure_t);
procedure gravity_closure_inc_refcount(vm: Pgravity_vm;
  closure: Pgravity_closure_t);
procedure gravity_closure_free(vm: Pgravity_vm;
  closure: Pgravity_closure_t);
function gravity_closure_size(vm: Pgravity_vm;
  closure: Pgravity_closure_t): UInt32;
function gravity_closure_new(vm: Pgravity_vm; f: Pgravity_function_t)
  : Pgravity_closure_t;

procedure gravity_upvalue_blacken(vm: Pgravity_vm;
  upvalue: Pgravity_upvalue_t);
procedure gravity_upvalue_free(vm: Pgravity_vm;
  upvalue: Pgravity_upvalue_t);
function gravity_upvalue_new(vm: Pgravity_vm; value: Pgravity_value_t)
  : Pgravity_upvalue_t;
function gravity_upvalue_size(vm: Pgravity_vm;
  upvalue: Pgravity_upvalue_t): UInt32;

procedure gravity_class_blacken(vm: Pgravity_vm; c: Pgravity_class_t);
function gravity_class_add_ivar(c: Pgravity_class_t;
  const identifier: AnsiString): Int16;
procedure gravity_class_bind(c: Pgravity_class_t; const key: AnsiString;
  value: gravity_value_t);
function gravity_class_count_ivars(c: Pgravity_class_t): UInt32;
function gravity_class_deserialize(vm: Pgravity_vm; json: Pjson_value)
  : Pgravity_class_t;
procedure gravity_class_dump(c: Pgravity_class_t);
procedure gravity_class_free(vm: Pgravity_vm; c: Pgravity_class_t);
procedure gravity_class_free_core(vm: Pgravity_vm; c: Pgravity_class_t);
function gravity_class_get_meta(c: Pgravity_class_t): Pgravity_class_t;
function gravity_class_getsuper(c: Pgravity_class_t): Pgravity_class_t;
function gravity_class_grow(c: Pgravity_class_t; n: UInt32): Boolean;
function gravity_class_is_meta(c: Pgravity_class_t): Boolean;
function gravity_class_lookup(c: Pgravity_class_t; key: gravity_value_t)
  : Pgravity_object_t;
function gravity_class_lookup_closure(c: Pgravity_class_t;
  key: gravity_value_t): Pgravity_closure_t;
function gravity_class_lookup_constructor(c: Pgravity_class_t;
  nparams: UInt32): Pgravity_closure_t;
function gravity_class_new_pair(vm: Pgravity_vm;
  const identifier: AnsiString; superclass: Pgravity_class_t; nivar: UInt32;
  nsvar: UInt32): Pgravity_class_t;
function gravity_class_new_single(vm: Pgravity_vm;
  const identifier: AnsiString; nfields: UInt32): Pgravity_class_t;
procedure gravity_class_serialize(c: Pgravity_class_t; json: Pjson_t);
function gravity_class_setsuper(subclass: Pgravity_class_t;
  superclass: Pgravity_class_t): Boolean;
procedure gravity_class_setxdata(c: Pgravity_class_t; xdata: Pointer);
function gravity_class_size(vm: Pgravity_vm; c: Pgravity_class_t): UInt32;

procedure gravity_fiber_blacken(vm: Pgravity_vm; fiber: Pgravity_fiber_t);
procedure gravity_fiber_free(vm: Pgravity_vm; fiber: Pgravity_fiber_t);
function gravity_fiber_new(vm: Pgravity_vm; closure: Pgravity_closure_t;
  nstack: UInt32; nframes: UInt32): Pgravity_fiber_t;
procedure gravity_fiber_reassign(fiber: Pgravity_fiber_t;
  closure: Pgravity_closure_t; nargs: UInt16);
procedure gravity_fiber_seterror(fiber: Pgravity_fiber_t;
  const error: AnsiString);
function gravity_fiber_size(vm: Pgravity_vm;
  fiber: Pgravity_fiber_t): UInt32;

procedure gravity_instance_blacken(vm: Pgravity_vm; i: Pgravity_instance_t);
function gravity_instance_clone(vm: Pgravity_vm;
  src_instance: Pgravity_instance_t): Pgravity_instance_t;
procedure gravity_instance_free(vm: Pgravity_vm; i: Pgravity_instance_t);
function gravity_instance_lookup_event(i: Pgravity_instance_t;
  const name: AnsiString): Pgravity_closure_t;
function gravity_instance_new(vm: Pgravity_vm; c: Pgravity_class_t)
  : Pgravity_instance_t;
procedure gravity_instance_setivar(instance: Pgravity_instance_t;
  idx: UInt32; value: gravity_value_t);
procedure gravity_instance_setxdata(i: Pgravity_instance_t; xdata: Pointer);
function gravity_instance_size(vm: Pgravity_vm;
  i: Pgravity_instance_t): UInt32;

procedure gravity_value_dump(vm: Pgravity_vm; v: gravity_value_t;
  buffer: PAnsiChar; len: UInt16);
function gravity_value_equals(v1: gravity_value_t;
  v2: gravity_value_t): Boolean;
procedure gravity_value_free(vm: Pgravity_vm; v: gravity_value_t);
function gravity_value_getclass(v: gravity_value_t): Pgravity_class_t;
function gravity_value_getsuper(v: gravity_value_t): Pgravity_class_t;
function gravity_value_hash(value: gravity_value_t): UInt32;
function gravity_value_isobject(v: gravity_value_t): Boolean;
procedure gravity_value_serialize(const key: AnsiString; v: gravity_value_t;
  json: Pjson_t);
function gravity_value_size(vm: Pgravity_vm; v: gravity_value_t): UInt32;
function gravity_value_xdata(value: gravity_value_t): Pointer;

procedure gravity_object_blacken(vm: Pgravity_vm; obj: Pgravity_object_t);
function gravity_object_debug(obj: Pgravity_object_t; is_free: Boolean)
  : PAnsiChar;
function gravity_object_deserialize(vm: Pgravity_vm; entry: Pjson_value)
  : Pgravity_object_t;
procedure gravity_object_free(vm: Pgravity_vm; obj: Pgravity_object_t);
procedure gravity_object_serialize(obj: Pgravity_object_t; json: Pjson_t);
function gravity_object_size(vm: Pgravity_vm;
  obj: Pgravity_object_t): UInt32;

procedure gravity_list_append_list(vm: Pgravity_vm; list1: Pgravity_list_t;
  list2: Pgravity_list_t);
procedure gravity_list_blacken(vm: Pgravity_vm; list: Pgravity_list_t);
procedure gravity_list_free(vm: Pgravity_vm; list: Pgravity_list_t);
function gravity_list_from_array(vm: Pgravity_vm; n: UInt32;
  p: Pgravity_value_t): Pgravity_list_t;
function gravity_list_new(vm: Pgravity_vm; n: UInt32): Pgravity_list_t;
function gravity_list_size(vm: Pgravity_vm; list: Pgravity_list_t): UInt32;

procedure gravity_map_blacken(vm: Pgravity_vm; map: Pgravity_map_t);
procedure gravity_map_append_map(vm: Pgravity_vm; map1: Pgravity_map_t;
  map2: Pgravity_map_t);
procedure gravity_map_free(vm: Pgravity_vm; map: Pgravity_map_t);
procedure gravity_map_insert(vm: Pgravity_vm; map: Pgravity_map_t;
  key: gravity_value_t; value: gravity_value_t);
function gravity_map_new(vm: Pgravity_vm; n: UInt32): Pgravity_map_t;
function gravity_map_size(vm: Pgravity_vm; map: Pgravity_map_t): UInt32;

procedure gravity_range_blacken(vm: Pgravity_vm; range: Pgravity_range_t);
procedure gravity_range_free(vm: Pgravity_vm; range: Pgravity_range_t);
function gravity_range_new(vm: Pgravity_vm; from: gravity_int_t;
  _to: gravity_int_t; inclusive: Boolean): Pgravity_range_t;
function gravity_range_size(vm: Pgravity_vm;
  range: Pgravity_range_t): UInt32;

/// MARK: - STRING -
procedure gravity_string_blacken(vm: Pgravity_vm;
  _string: Pgravity_string_t);
procedure gravity_string_free(vm: Pgravity_vm; value: Pgravity_string_t);
function gravity_string_new(vm: Pgravity_vm; s: PAnsiChar; len: UInt32;
  alloc: UInt32): Pgravity_string_t;
procedure gravity_string_set(obj: Pgravity_string_t; s: PAnsiChar;
  len: UInt32);
function gravity_string_size(vm: Pgravity_vm;
  _string: Pgravity_string_t): UInt32;
function gravity_string_to_value(vm: Pgravity_vm; const s: AnsiString;
  len: UInt32): gravity_value_t;

function gravity_compiler_create(delegate: Pgravity_delegate_t)
  : Pgravity_compiler_t;
function gravity_compiler_run(compiler: Pgravity_compiler_t;
  const source: AnsiString; len: NativeUInt; fileid: UInt32;
  is_static: Boolean; add_debug: Boolean): Pgravity_closure_t;
function gravity_compiler_ast(compiler: Pgravity_compiler_t): Pgnode_t;
procedure gravity_compiler_free(compiler: Pgravity_compiler_t);
function gravity_compiler_serialize(compiler: Pgravity_compiler_t;
  closure: Pgravity_closure_t): Pjson_t;
function gravity_compiler_serialize_infile(compiler: Pgravity_compiler_t;
  closure: Pgravity_closure_t; const path: AnsiString): Boolean;
procedure gravity_compiler_transfer(compiler: Pgravity_compiler_t;
  vm: Pgravity_vm);

function gravity_vm_delegate(vm: Pgravity_vm): Pgravity_delegate_t;
function gravity_vm_fiber(vm: Pgravity_vm): Pgravity_fiber_t;
procedure gravity_vm_free(vm: Pgravity_vm);
function gravity_vm_getvalue(vm: Pgravity_vm; const key: AnsiString;
  keylen: UInt32): gravity_value_t;
function gravity_vm_keyindex(vm: Pgravity_vm; index: UInt32)
  : gravity_value_t;
function gravity_vm_ismini(vm: Pgravity_vm): Boolean;
function gravity_vm_isaborted(vm: Pgravity_vm): Boolean;
procedure gravity_vm_loadclosure(vm: Pgravity_vm;
  closure: Pgravity_closure_t);
function gravity_vm_lookup(vm: Pgravity_vm; key: gravity_value_t)
  : gravity_value_t;
function gravity_vm_new(delegate: Pgravity_delegate_t): Pgravity_vm;
function gravity_vm_newmini(): Pgravity_vm;
function gravity_vm_result(vm: Pgravity_vm): gravity_value_t;
function gravity_vm_runclosure(vm: Pgravity_vm; closure: Pgravity_closure_t;
  sender: gravity_value_t; params: Pgravity_value_t;
  nparams: UInt16): Boolean;
function gravity_vm_runmain(vm: Pgravity_vm;
  closure: Pgravity_closure_t): Boolean;
procedure gravity_vm_set_callbacks(vm: Pgravity_vm;
  vm_transfer: vm_transfer_cb; vm_cleanup: vm_cleanup_cb);
procedure gravity_vm_setaborted(vm: Pgravity_vm);
procedure gravity_vm_seterror(vm: Pgravity_vm; const format: AnsiString);
procedure gravity_vm_seterror_string(vm: Pgravity_vm; const s: AnsiString);
procedure gravity_vm_setfiber(vm: Pgravity_vm; fiber: Pgravity_fiber_t);
procedure gravity_vm_setvalue(vm: Pgravity_vm; const key: AnsiString;
  value: gravity_value_t);
function gravity_vm_time(vm: Pgravity_vm): Double;

procedure gravity_vm_cleanup(vm: Pgravity_vm);
procedure gravity_vm_filter(vm: Pgravity_vm; cleanup_filter: vm_filter_cb);
procedure gravity_vm_transfer(vm: Pgravity_vm; obj: Pgravity_object_t);
procedure gravity_vm_initmodule(vm: Pgravity_vm; f: Pgravity_function_t);
function gravity_vm_loadbuffer(vm: Pgravity_vm; const buffer: AnsiString;
  len: NativeUInt): Pgravity_closure_t;
function gravity_vm_loadfile(vm: Pgravity_vm; const path: AnsiString)
  : Pgravity_closure_t;
function gravity_vm_fastlookup(vm: Pgravity_vm; c: Pgravity_class_t;
  index: Integer): Pgravity_closure_t;
function gravity_vm_getdata(vm: Pgravity_vm): Pointer;
function gravity_vm_getslot(vm: Pgravity_vm; index: UInt32)
  : gravity_value_t;
procedure gravity_vm_setdata(vm: Pgravity_vm; data: Pointer);
procedure gravity_vm_setslot(vm: Pgravity_vm; value: gravity_value_t;
  index: UInt32);
procedure gravity_vm_memupdate(vm: Pgravity_vm; value: gravity_int_t);
function gravity_vm_anonymous(vm: Pgravity_vm): PAnsiChar;
function gravity_vm_get(vm: Pgravity_vm; const key: AnsiString)
  : gravity_value_t;
function gravity_vm_set(vm: Pgravity_vm; const key: AnsiString;
  value: gravity_value_t): Boolean;

procedure gravity_gray_object(vm: Pgravity_vm; obj: Pgravity_object_t);
procedure gravity_gray_value(vm: Pgravity_vm; v: gravity_value_t);

procedure gravity_gc_setenabled(vm: Pgravity_vm; enabled: Boolean);
procedure gravity_gc_start(vm: Pgravity_vm);
procedure gravity_gc_temppop(vm: Pgravity_vm);
procedure gravity_gc_temppush(vm: Pgravity_vm; obj: Pgravity_object_t);

function gravity_core_class_from_name(const name: AnsiString)
  : Pgravity_class_t;
procedure gravity_core_free();
function gravity_core_identifiers(): PPAnsiChar;
procedure gravity_core_register(vm: Pgravity_vm);
function gravity_iscore_class(c: Pgravity_class_t): Boolean;

function convert_value2bool(vm: Pgravity_vm; v: gravity_value_t)
  : gravity_value_t;
function convert_value2float(vm: Pgravity_vm; v: gravity_value_t)
  : gravity_value_t;
function convert_value2int(vm: Pgravity_vm; v: gravity_value_t)
  : gravity_value_t;
function convert_value2string(vm: Pgravity_vm; v: gravity_value_t)
  : gravity_value_t;

function computed_property_create(vm: Pgravity_vm;
  getter_func: Pgravity_function_t; setter_func: Pgravity_function_t)
  : Pgravity_closure_t;
procedure computed_property_free(c: Pgravity_class_t;
  const name: AnsiString; remove_flag: Boolean);

function gravity_hash_create(size: UInt32; compute: gravity_hash_compute_fn;
  isequal: gravity_hash_isequal_fn; free: gravity_hash_iterate_fn;
  data: Pointer): Pgravity_hash_t;
procedure gravity_hash_free(hashtable: Pgravity_hash_t);
function gravity_hash_insert(hashtable: Pgravity_hash_t;
  key: gravity_value_t; value: gravity_value_t): Boolean;
function gravity_hash_isempty(hashtable: Pgravity_hash_t): Boolean;
function gravity_hash_lookup(hashtable: Pgravity_hash_t;
  key: gravity_value_t): Pgravity_value_t;
function gravity_hash_remove(hashtable: Pgravity_hash_t;
  key: gravity_value_t): Boolean;
procedure gravity_hash_append(hashtable1: Pgravity_hash_t;
  hashtable2: Pgravity_hash_t);
function gravity_hash_compute_buffer(const key: AnsiString;
  len: UInt32): UInt32;
function gravity_hash_compute_float(f: gravity_float_t): UInt32;
function gravity_hash_compute_int(n: gravity_int_t): UInt32;
function gravity_hash_count(hashtable: Pgravity_hash_t): UInt32;
procedure gravity_hash_dump(hashtable: Pgravity_hash_t);
procedure gravity_hash_iterate(hashtable: Pgravity_hash_t;
  iterate: gravity_hash_iterate_fn; data: Pointer);
procedure gravity_hash_iterate2(hashtable: Pgravity_hash_t;
  iterate: gravity_hash_iterate2_fn; data1: Pointer; data2: Pointer);
function gravity_hash_memsize(hashtable: Pgravity_hash_t): UInt32;
procedure gravity_hash_resetfree(hashtable: Pgravity_hash_t);
procedure gravity_hash_stat(hashtable: Pgravity_hash_t);
procedure gravity_hash_transform(hashtable: Pgravity_hash_t;
  iterate: gravity_hash_transform_fn; data: Pointer);
procedure gravity_hash_keyfree(table: Pgravity_hash_t; key: gravity_value_t;
  value: gravity_value_t; data: Pointer);
procedure gravity_hash_keyvaluefree(table: Pgravity_hash_t;
  key: gravity_value_t; value: gravity_value_t; data: Pointer);
procedure gravity_hash_valuefree(table: Pgravity_hash_t;
  key: gravity_value_t; value: gravity_value_t; data: Pointer);

function gravity_value_from_error(msg: PAnsiChar): gravity_value_t;
function gravity_value_from_object(obj: Pointer): gravity_value_t;
function gravity_value_from_int(n: gravity_int_t): gravity_value_t;
function gravity_value_from_float(f: gravity_float_t): gravity_value_t;
function gravity_value_from_null: gravity_value_t;
function gravity_value_from_undefined: gravity_value_t;
function gravity_value_from_bool(b: Boolean): gravity_value_t;

function VALUE_FROM_ERROR(msg: PAnsiChar): gravity_value_t;
function VALUE_NOT_VALID: gravity_value_t;
function VALUE_FROM_OBJECT(obj: Pointer): gravity_value_t;
function VALUE_FROM_STRING(vm: Pgravity_vm; const s: AnsiString;
  len: UInt32): gravity_value_t;
function VALUE_FROM_INT(n: gravity_int_t): gravity_value_t;
function VALUE_FROM_FLOAT(n: gravity_float_t): gravity_value_t;
function VALUE_FROM_NULL: gravity_value_t;
function VALUE_FROM_UNDEFINED: gravity_value_t;
function VALUE_FROM_BOOL(b: Boolean): gravity_value_t;
function VALUE_FROM_FALSE: gravity_value_t;
function VALUE_FROM_TRUE: gravity_value_t;

function VALUE_AS_OBJECT(val: Pgravity_value_t): Pgravity_object_t;
function VALUE_AS_STRING(val: Pgravity_value_t): Pgravity_string_t;
function VALUE_AS_FIBER(val: Pgravity_value_t): Pgravity_fiber_t;
function VALUE_AS_FUNCTION(val: Pgravity_value_t): Pgravity_function_t;
function VALUE_AS_CLOSURE(val: Pgravity_value_t): Pgravity_closure_t;
function VALUE_AS_CLASS(val: Pgravity_value_t): Pgravity_class_t;
function VALUE_AS_INSTANCE(val: Pgravity_value_t): Pgravity_instance_t;
function VALUE_AS_LIST(val: Pgravity_value_t): Pgravity_list_t;
function VALUE_AS_MAP(val: Pgravity_value_t): Pgravity_map_t;
function VALUE_AS_RANGE(val: Pgravity_value_t): Pgravity_range_t;
function VALUE_AS_ERROR(val: Pgravity_value_t): PAnsiChar;
function VALUE_AS_FLOAT(val: Pgravity_value_t): gravity_float_t;
function VALUE_AS_INT(val: Pgravity_value_t): gravity_int_t;
function VALUE_AS_BOOL(val: Pgravity_value_t): gravity_int_t;
function VALUE_AS_CSTRING(val: Pgravity_value_t): PAnsiChar;

function VALUE_ISA_FUNCTION(v: gravity_value_t): Boolean;
function VALUE_ISA_INSTANCE(v: gravity_value_t): Boolean;
function VALUE_ISA_CLOSURE(v: gravity_value_t): Boolean;
function VALUE_ISA_FIBER(v: gravity_value_t): Boolean;
function VALUE_ISA_CLASS(v: gravity_value_t): Boolean;
function VALUE_ISA_STRING(v: gravity_value_t): Boolean;
function VALUE_ISA_INT(v: gravity_value_t): Boolean;
function VALUE_ISA_FLOAT(v: gravity_value_t): Boolean;
function VALUE_ISA_BOOL(v: gravity_value_t): Boolean;
function VALUE_ISA_LIST(v: gravity_value_t): Boolean;
function VALUE_ISA_MAP(v: gravity_value_t): Boolean;
function VALUE_ISA_RANGE(v: gravity_value_t): Boolean;
function VALUE_ISA_BASIC_TYPE(v: gravity_value_t): Boolean;
function VALUE_ISA_NULLCLASS(v: gravity_value_t): Boolean;
function VALUE_ISA_NULL(v: gravity_value_t): Boolean;
function VALUE_ISA_UNDEFINED(v: gravity_value_t): Boolean;
function VALUE_ISA_CALLABLE(v: gravity_value_t): Boolean;
function VALUE_ISA_VALID(v: gravity_value_t): Boolean;
function VALUE_ISA_NOTVALID(v: gravity_value_t): Boolean;
function VALUE_ISA_ERROR(v: gravity_value_t): Boolean;

function NEW_FUNCTION_BRIDGED(identifier: AnsiString; xdata: pointer): Pgravity_function_t;
function NEW_CLOSURE_VALUE_BRIDGED(identifier: AnsiString; xdata: pointer): gravity_value_t;

function NEW_FUNCTION(fptr: gravity_c_internal): Pgravity_function_t;
function NEW_CLOSURE_VALUE(fptr: gravity_c_internal): gravity_value_t;

function GET_VALUE(args: Pgravity_value_t; ndx: UInt16): gravity_value_t;
procedure SETMETA_INITED(c: Pgravity_class_t);
function RETURN_VALUE(vm: Pgravity_vm; v: gravity_value_t; index: UInt32): Boolean;
function RETURN_CLOSURE(vm: Pgravity_vm; v: gravity_value_t; index: UInt32): Boolean;
function RETURN_FIBER: Boolean;
function RETURN_NOVALUE: Boolean;

implementation

function GET_VALUE(args: Pgravity_value_t;
  ndx: UInt16): gravity_value_t;
begin
  Result := Pgravity_value_t(NativeUInt(args) + ndx * SizeOf(gravity_value_t))^;
end;

function NEW_FUNCTION_BRIDGED(identifier: AnsiString; xdata: pointer): Pgravity_function_t;
begin
  result := GravityEng.NEW_FUNCTION_BRIDGED(identifier, xdata)
end;

function NEW_CLOSURE_VALUE_BRIDGED(identifier: AnsiString; xdata: pointer): gravity_value_t;
begin
  result := GravityEng.NEW_CLOSURE_VALUE_BRIDGED(identifier, xdata)
end;

function NEW_CLOSURE_VALUE(fptr: gravity_c_internal)
  : gravity_value_t;
begin
  result := GravityEng.NEW_CLOSURE_VALUE(fptr);
end;

function NEW_FUNCTION(fptr: gravity_c_internal): Pgravity_function_t;
begin
  result := GravityEng.NEW_FUNCTION(fptr);
end;

function RETURN_CLOSURE(vm: Pgravity_vm; v: gravity_value_t;
  index: UInt32): Boolean;
begin
  Result := GravityEng.RETURN_CLOSURE(vm, v, index);
end;

function RETURN_FIBER: Boolean;
begin
  Result := GravityEng.RETURN_FIBER;
end;

function RETURN_NOVALUE: Boolean;
begin
  Result := GravityEng.RETURN_NOVALUE;
end;

function RETURN_VALUE(vm: Pgravity_vm; v: gravity_value_t;
  index: UInt32): Boolean;
begin
  Result := GravityEng.RETURN_VALUE(vm, v, index);
end;

procedure SETMETA_INITED(c: Pgravity_class_t);
begin
  if Assigned(c) then
    c^.is_inited := True;
end;

function VALUE_AS_BOOL(val: Pgravity_value_t): gravity_int_t;
begin
  result := val^.f2.n;
end;

function VALUE_AS_CLASS(val: Pgravity_value_t): Pgravity_class_t;
begin
  result := Pgravity_class_t(VALUE_AS_OBJECT(val));
end;

function VALUE_AS_CLOSURE(val: Pgravity_value_t)
  : Pgravity_closure_t;
begin
  result := Pgravity_closure_t(VALUE_AS_OBJECT(val));
end;

function VALUE_AS_CSTRING(val: Pgravity_value_t): PAnsiChar;
begin
  result := VALUE_AS_STRING(val)^.s;
end;

function VALUE_AS_ERROR(val: Pgravity_value_t): PAnsiChar;
begin
  result := PAnsiChar(val^.f2.p);
end;

function VALUE_AS_FIBER(val: Pgravity_value_t): Pgravity_fiber_t;
begin
  result := Pgravity_fiber_t(VALUE_AS_OBJECT(val));
end;

function VALUE_AS_FLOAT(val: Pgravity_value_t): gravity_float_t;
begin
  result := val^.f2.f;
end;

function VALUE_AS_FUNCTION(val: Pgravity_value_t)
  : Pgravity_function_t;
begin
  result := Pgravity_function_t(VALUE_AS_OBJECT(val));
end;

function VALUE_AS_INSTANCE(val: Pgravity_value_t)
  : Pgravity_instance_t;
begin
  result := Pgravity_instance_t(VALUE_AS_OBJECT(val));
end;

function VALUE_AS_INT(val: Pgravity_value_t): gravity_int_t;
begin
  result := val^.f2.n;
end;

function VALUE_AS_LIST(val: Pgravity_value_t): Pgravity_list_t;
begin
  result := Pgravity_list_t(VALUE_AS_OBJECT(val));
end;

function VALUE_AS_MAP(val: Pgravity_value_t): Pgravity_map_t;
begin
  result := Pgravity_map_t(VALUE_AS_OBJECT(val));
end;

function VALUE_AS_OBJECT(val: Pgravity_value_t): Pgravity_object_t;
begin
  result := val^.f2.p;
end;

function VALUE_AS_RANGE(val: Pgravity_value_t): Pgravity_range_t;
begin
  result := Pgravity_range_t(VALUE_AS_OBJECT(val));
end;

function VALUE_AS_STRING(val: Pgravity_value_t): Pgravity_string_t;
begin
  result := Pgravity_string_t(VALUE_AS_OBJECT(val));
end;

function VALUE_FROM_BOOL(b: Boolean): gravity_value_t;
begin
  result := gravity_value_from_bool(b);
end;

function VALUE_FROM_ERROR(msg: PAnsiChar): gravity_value_t;
begin
  result := gravity_value_from_error(msg);
end;

function VALUE_FROM_FALSE: gravity_value_t;
begin
  result := VALUE_FROM_BOOL(False);
end;

function VALUE_FROM_FLOAT(n: gravity_float_t): gravity_value_t;
begin
  result := gravity_value_from_float(n);
end;

function VALUE_FROM_INT(n: gravity_int_t): gravity_value_t;
begin
  result := gravity_value_from_int(n);
end;

function VALUE_FROM_NULL: gravity_value_t;
begin
  result := gravity_value_from_null;
end;

function VALUE_FROM_OBJECT(obj: Pointer): gravity_value_t;
begin
  result := gravity_value_from_object(obj)
end;

function VALUE_FROM_STRING(vm: Pgravity_vm; const s: AnsiString;
  len: UInt32): gravity_value_t;
begin
  result := gravity_string_to_value(vm, s, len);
end;

function VALUE_FROM_TRUE: gravity_value_t;
begin
  result := VALUE_FROM_BOOL(True);
end;

function VALUE_FROM_UNDEFINED: gravity_value_t;
begin
  result := gravity_value_from_undefined;
end;

function VALUE_ISA_BASIC_TYPE(v: gravity_value_t): Boolean;
begin
  result := VALUE_ISA_STRING(v) or VALUE_ISA_INT(v) or VALUE_ISA_FLOAT(v) or
    VALUE_ISA_BOOL(v);
end;

function VALUE_ISA_BOOL(v: gravity_value_t): Boolean;
begin
  result := GravityEng.VALUE_ISA_BOOL(v);
end;

function VALUE_ISA_CALLABLE(v: gravity_value_t): Boolean;
begin
  result := GravityEng.VALUE_ISA_CALLABLE(v)
end;

function VALUE_ISA_CLASS(v: gravity_value_t): Boolean;
begin
  result := GravityEng.VALUE_ISA_CLASS(v)
end;

function VALUE_ISA_CLOSURE(v: gravity_value_t): Boolean;
begin
  result := GravityEng.VALUE_ISA_CLOSURE(v)
end;

function VALUE_ISA_ERROR(v: gravity_value_t): Boolean;
begin
  result := VALUE_ISA_NOTVALID(v);
end;

function VALUE_ISA_FIBER(v: gravity_value_t): Boolean;
begin
  result := GravityEng.VALUE_ISA_FIBER(v)
end;

function VALUE_ISA_FLOAT(v: gravity_value_t): Boolean;
begin
  result := GravityEng.VALUE_ISA_FLOAT(v)
end;

function VALUE_ISA_FUNCTION(v: gravity_value_t): Boolean;
begin
  result := GravityEng.VALUE_ISA_FUNCTION(v)
end;

function VALUE_ISA_INSTANCE(v: gravity_value_t): Boolean;
begin
  result := GravityEng.VALUE_ISA_INSTANCE(v)
end;

function VALUE_ISA_INT(v: gravity_value_t): Boolean;
begin
  result := GravityEng.VALUE_ISA_INT(v);
end;

function VALUE_ISA_LIST(v: gravity_value_t): Boolean;
begin
  result := GravityEng.VALUE_ISA_LIST(v)
end;

function VALUE_ISA_MAP(v: gravity_value_t): Boolean;
begin
  result := GravityEng.VALUE_ISA_MAP(v)
end;

function VALUE_ISA_NOTVALID(v: gravity_value_t): Boolean;
begin
  result := (not Assigned(v.isa)) or (v.isa = nil);
end;

function VALUE_ISA_NULL(v: gravity_value_t): Boolean;
begin
  result := GravityEng.VALUE_ISA_NULL(v)
end;

function VALUE_ISA_NULLCLASS(v: gravity_value_t): Boolean;
begin
  result := GravityEng.VALUE_ISA_NULLCLASS(v)
end;

function VALUE_ISA_RANGE(v: gravity_value_t): Boolean;
begin
  result := GravityEng.VALUE_ISA_RANGE(v)
end;

function VALUE_ISA_STRING(v: gravity_value_t): Boolean;
begin
  result := GravityEng.VALUE_ISA_STRING(v)
end;

function VALUE_ISA_UNDEFINED(v: gravity_value_t): Boolean;
begin
  result := GravityEng.VALUE_ISA_UNDEFINED(v)
end;

function VALUE_ISA_VALID(v: gravity_value_t): Boolean;
begin
  result := Assigned(v.isa) and (v.isa <> nil);
end;

function VALUE_NOT_VALID: gravity_value_t;
begin
  result := VALUE_FROM_ERROR(nil)
end;

function gravity_module_new(vm: Pgravity_vm;
  const identifier: AnsiString): Pgravity_module_t;
begin
  result := GravityEng.gravity_module_new(vm, identifier);
end;

procedure gravity_module_free(vm: Pgravity_vm;
  m: Pgravity_module_t);
begin
  GravityEng.gravity_module_free(vm, m);
end;

procedure gravity_module_blacken(vm: Pgravity_vm;
  m: Pgravity_module_t);
begin
  GravityEng.gravity_module_blacken(vm, m);
end;

function gravity_module_size(vm: Pgravity_vm;
  m: Pgravity_module_t): UInt32;
begin
  result := GravityEng.gravity_module_size(vm, m);
end;

function gravity_bytecode_deserialize(const buffer: AnsiString;
  len: NativeUInt; ninst: PUInt32): PUInt32;
begin
  result := GravityEng.gravity_bytecode_deserialize(buffer, len, ninst);
end;

procedure gravity_function_blacken(vm: Pgravity_vm;
  f: Pgravity_function_t);
begin
  GravityEng.gravity_function_blacken(vm, f);
end;

function gravity_function_cpool_add(vm: Pgravity_vm;
  f: Pgravity_function_t; v: gravity_value_t): UInt16;
begin
  result := GravityEng.gravity_function_cpool_add(vm, f, v);
end;

function gravity_function_cpool_get(f: Pgravity_function_t;
  i: UInt16): gravity_value_t;
begin
  result := GravityEng.gravity_function_cpool_get(f, i);
end;

function gravity_function_deserialize(vm: Pgravity_vm;
  json: Pjson_value): Pgravity_function_t;
begin
  result := GravityEng.gravity_function_deserialize(vm, json);
end;

procedure gravity_function_dump(f: Pgravity_function_t;
  codef: code_dump_function);
begin
  GravityEng.gravity_function_dump(f, codef);
end;

procedure gravity_function_free(vm: Pgravity_vm;
  f: Pgravity_function_t);
begin
  GravityEng.gravity_function_free(vm, f);
end;

function gravity_function_new(vm: Pgravity_vm;
  const identifier: AnsiString; nparams: UInt16; nlocals: UInt16;
  ntemps: UInt16; code: Pointer): Pgravity_function_t;
begin
  result := GravityEng.gravity_function_new(vm, identifier, nparams, nlocals,
    ntemps, code);
end;

function gravity_function_new_bridged(vm: Pgravity_vm;
  const identifier: AnsiString; xdata: Pointer): Pgravity_function_t;
begin
  result := GravityEng.gravity_function_new_bridged(vm, identifier, xdata);
end;

function gravity_function_new_internal(vm: Pgravity_vm;
  const identifier: AnsiString; exec: gravity_c_internal; nparams: UInt16)
  : Pgravity_function_t;
begin
  result := GravityEng.gravity_function_new_internal(vm, identifier, exec, nparams);
end;

function gravity_function_new_special(vm: Pgravity_vm;
  const identifier: AnsiString; index: UInt16; getter: Pointer; setter: Pointer)
  : Pgravity_function_t;
begin
  result := GravityEng.gravity_function_new_special(vm, identifier, index,
    getter, setter);
end;

procedure gravity_function_serialize(f: Pgravity_function_t;
  json: Pjson_t);
begin
  GravityEng.gravity_function_serialize(f, json);
end;

procedure gravity_function_setxdata(f: Pgravity_function_t;
  xdata: Pointer);
begin
  GravityEng.gravity_function_setxdata(f, xdata);
end;

function gravity_function_size(vm: Pgravity_vm;
  f: Pgravity_function_t): UInt32;
begin
  result := GravityEng.gravity_function_size(vm, f);
end;

procedure gravity_upvalue_blacken(vm: Pgravity_vm;
  upvalue: Pgravity_upvalue_t);
begin
  GravityEng.gravity_upvalue_blacken(vm, upvalue);
end;

procedure gravity_upvalue_free(vm: Pgravity_vm;
  upvalue: Pgravity_upvalue_t);
begin
  GravityEng.gravity_upvalue_free(vm, upvalue);
end;

function gravity_upvalue_new(vm: Pgravity_vm;
  value: Pgravity_value_t): Pgravity_upvalue_t;
begin
  result := GravityEng.gravity_upvalue_new(vm, value);
end;

function gravity_upvalue_size(vm: Pgravity_vm;
  upvalue: Pgravity_upvalue_t): UInt32;
begin
  result := GravityEng.gravity_upvalue_size(vm, upvalue);
end;

procedure gravity_class_blacken(vm: Pgravity_vm;
  c: Pgravity_class_t);
begin
  GravityEng.gravity_class_blacken(vm, c);
end;

function gravity_class_add_ivar(c: Pgravity_class_t;
  const identifier: AnsiString): Int16;
begin
  result := GravityEng.gravity_class_add_ivar(c, identifier);
end;

procedure gravity_class_bind(c: Pgravity_class_t;
  const key: AnsiString; value: gravity_value_t);
begin
  GravityEng.gravity_class_bind(c, key, value);
end;

function gravity_class_count_ivars(c: Pgravity_class_t): UInt32;
begin
  result := GravityEng.gravity_class_count_ivars(c);
end;

function gravity_class_deserialize(vm: Pgravity_vm;
  json: Pjson_value): Pgravity_class_t;
begin
  result := GravityEng.gravity_class_deserialize(vm, json);
end;

procedure gravity_class_dump(c: Pgravity_class_t);
begin
  GravityEng.gravity_class_dump(c);
end;

procedure gravity_class_free(vm: Pgravity_vm; c: Pgravity_class_t);
begin
  GravityEng.gravity_class_free(vm, c);
end;

procedure gravity_class_free_core(vm: Pgravity_vm;
  c: Pgravity_class_t);
begin
  GravityEng.gravity_class_free_core(vm, c);
end;

function gravity_class_get_meta(c: Pgravity_class_t)
  : Pgravity_class_t;
begin
  result := GravityEng.gravity_class_get_meta(c);
end;

function gravity_class_getsuper(c: Pgravity_class_t)
  : Pgravity_class_t;
begin
  result := GravityEng.gravity_class_getsuper(c);
end;

function gravity_class_grow(c: Pgravity_class_t;
  n: UInt32): Boolean;
begin
  result := GravityEng.gravity_class_grow(c, n);
end;

function gravity_class_is_meta(c: Pgravity_class_t): Boolean;
begin
  result := GravityEng.gravity_class_is_meta(c);
end;

function gravity_class_lookup(c: Pgravity_class_t;
  key: gravity_value_t): Pgravity_object_t;
begin
  result := GravityEng.gravity_class_lookup(c, key);
end;

function gravity_class_lookup_closure(c: Pgravity_class_t;
  key: gravity_value_t): Pgravity_closure_t;
begin
  result := GravityEng.gravity_class_lookup_closure(c, key);
end;

function gravity_class_lookup_constructor(c: Pgravity_class_t;
  nparams: UInt32): Pgravity_closure_t;
begin
  result := GravityEng.gravity_class_lookup_constructor(c, nparams);
end;

function gravity_class_new_pair(vm: Pgravity_vm;
  const identifier: AnsiString; superclass: Pgravity_class_t; nivar: UInt32;
  nsvar: UInt32): Pgravity_class_t;
begin
  result := GravityEng.gravity_class_new_pair(vm, identifier, superclass,
    nivar, nsvar);
end;

function gravity_class_new_single(vm: Pgravity_vm;
  const identifier: AnsiString; nfields: UInt32): Pgravity_class_t;
begin
  result := GravityEng.gravity_class_new_single(vm, identifier, nfields);
end;

procedure gravity_class_serialize(c: Pgravity_class_t;
  json: Pjson_t);
begin
  GravityEng.gravity_class_serialize(c, json);
end;

function gravity_class_setsuper(subclass: Pgravity_class_t;
  superclass: Pgravity_class_t): Boolean;
begin
  result := GravityEng.gravity_class_setsuper(subclass, superclass);
end;

procedure gravity_class_setxdata(c: Pgravity_class_t;
  xdata: Pointer);
begin
  GravityEng.gravity_class_setxdata(c, xdata);
end;

function gravity_class_size(vm: Pgravity_vm;
  c: Pgravity_class_t): UInt32;
begin
  result := GravityEng.gravity_class_size(vm, c);
end;

procedure gravity_closure_blacken(vm: Pgravity_vm;
  closure: Pgravity_closure_t);
begin
  GravityEng.gravity_closure_blacken(vm, closure);
end;

procedure gravity_closure_dec_refcount(vm: Pgravity_vm;
  closure: Pgravity_closure_t);
begin
  GravityEng.gravity_closure_dec_refcount(vm, closure);
end;

procedure gravity_closure_inc_refcount(vm: Pgravity_vm;
  closure: Pgravity_closure_t);
begin
  GravityEng.gravity_closure_inc_refcount(vm, closure);
end;

procedure gravity_closure_free(vm: Pgravity_vm;
  closure: Pgravity_closure_t);
begin
  GravityEng.gravity_closure_free(vm, closure);
end;

function gravity_closure_size(vm: Pgravity_vm;
  closure: Pgravity_closure_t): UInt32;
begin
  result := GravityEng.gravity_closure_size(vm, closure);
end;

function gravity_closure_new(vm: Pgravity_vm;
  f: Pgravity_function_t): Pgravity_closure_t;
begin
  result := GravityEng.gravity_closure_new(vm, f);
end;

procedure gravity_fiber_blacken(vm: Pgravity_vm;
  fiber: Pgravity_fiber_t);
begin
  GravityEng.gravity_fiber_blacken(vm, fiber);
end;

procedure gravity_fiber_free(vm: Pgravity_vm;
  fiber: Pgravity_fiber_t);
begin
  GravityEng.gravity_fiber_free(vm, fiber);
end;

function gravity_fiber_new(vm: Pgravity_vm;
  closure: Pgravity_closure_t; nstack: UInt32; nframes: UInt32)
  : Pgravity_fiber_t;
begin
  result := GravityEng.gravity_fiber_new(vm, closure, nstack, nframes);
end;

procedure gravity_fiber_reassign(fiber: Pgravity_fiber_t;
  closure: Pgravity_closure_t; nargs: UInt16);
begin
  GravityEng.gravity_fiber_reassign(fiber, closure, nargs);
end;

procedure gravity_fiber_seterror(fiber: Pgravity_fiber_t;
  const error: AnsiString);
begin
  GravityEng.gravity_fiber_seterror(fiber, error);
end;

function gravity_fiber_size(vm: Pgravity_vm;
  fiber: Pgravity_fiber_t): UInt32;
begin
  result := GravityEng.gravity_fiber_size(vm, fiber);
end;

procedure gravity_instance_blacken(vm: Pgravity_vm;
  i: Pgravity_instance_t);
begin
  GravityEng.gravity_instance_blacken(vm, i);
end;

function gravity_instance_clone(vm: Pgravity_vm;
  src_instance: Pgravity_instance_t): Pgravity_instance_t;
begin
  result := GravityEng.gravity_instance_clone(vm, src_instance);
end;

procedure gravity_instance_free(vm: Pgravity_vm;
  i: Pgravity_instance_t);
begin
  GravityEng.gravity_instance_free(vm, i);
end;

function gravity_instance_lookup_event(i: Pgravity_instance_t;
  const name: AnsiString): Pgravity_closure_t;
begin
  result := GravityEng.gravity_instance_lookup_event(i, name);
end;

function gravity_instance_new(vm: Pgravity_vm; c: Pgravity_class_t)
  : Pgravity_instance_t;
begin
  result := GravityEng.gravity_instance_new(vm, c);
end;

procedure gravity_instance_setivar(instance: Pgravity_instance_t;
  idx: UInt32; value: gravity_value_t);
begin
  GravityEng.gravity_instance_setivar(instance, idx, value);
end;

procedure gravity_instance_setxdata(i: Pgravity_instance_t;
  xdata: Pointer);
begin
  GravityEng.gravity_instance_setxdata(i, xdata);
end;

function gravity_instance_size(vm: Pgravity_vm;
  i: Pgravity_instance_t): UInt32;
begin
  result := GravityEng.gravity_instance_size(vm, i);
end;

procedure gravity_value_dump(vm: Pgravity_vm; v: gravity_value_t;
  buffer: PAnsiChar; len: UInt16);
begin
  GravityEng.gravity_value_dump(vm, v, buffer, len);
end;

function gravity_value_equals(v1: gravity_value_t;
  v2: gravity_value_t): Boolean;
begin
  result := GravityEng.gravity_value_equals(v1, v2);
end;

procedure gravity_value_free(vm: Pgravity_vm; v: gravity_value_t);
begin
  GravityEng.gravity_value_free(vm, v);
end;

function gravity_value_from_bool(b: Boolean): gravity_value_t;
begin
  result := GravityEng.gravity_value_from_bool(b);
end;

function gravity_value_from_error(msg: PAnsiChar): gravity_value_t;
begin
  result := GravityEng.gravity_value_from_error(msg);
end;

function gravity_value_from_float(f: gravity_float_t)
  : gravity_value_t;
begin
  result := GravityEng.gravity_value_from_float(f);
end;

function gravity_value_from_int(n: gravity_int_t): gravity_value_t;
begin
  result := GravityEng.gravity_value_from_int(n);
end;

function gravity_value_from_null(): gravity_value_t;
begin
  result := GravityEng.gravity_value_from_null();
end;

function gravity_value_from_object(obj: Pointer): gravity_value_t;
begin
  result := GravityEng.gravity_value_from_object(obj);
end;

function gravity_value_from_undefined(): gravity_value_t;
begin
  result := GravityEng.gravity_value_from_undefined();
end;

function gravity_value_getclass(v: gravity_value_t)
  : Pgravity_class_t;
begin
  result := GravityEng.gravity_value_getclass(v);
end;

function gravity_value_getsuper(v: gravity_value_t)
  : Pgravity_class_t;
begin
  result := GravityEng.gravity_value_getsuper(v);
end;

function gravity_value_hash(value: gravity_value_t): UInt32;
begin
  result := GravityEng.gravity_value_hash(value);
end;

function gravity_value_isobject(v: gravity_value_t): Boolean;
begin
  result := GravityEng.gravity_value_isobject(v);
end;

procedure gravity_value_serialize(const key: AnsiString;
  v: gravity_value_t; json: Pjson_t);
begin
  GravityEng.gravity_value_serialize(key, v, json);
end;

function gravity_value_size(vm: Pgravity_vm;
  v: gravity_value_t): UInt32;
begin
  result := GravityEng.gravity_value_size(vm, v);
end;

function gravity_value_xdata(value: gravity_value_t): Pointer;
begin
  result := GravityEng.gravity_value_xdata(value);
end;

procedure gravity_object_blacken(vm: Pgravity_vm;
  obj: Pgravity_object_t);
begin
  GravityEng.gravity_object_blacken(vm, obj);
end;

function gravity_object_debug(obj: Pgravity_object_t;
  is_free: Boolean): PAnsiChar;
begin
  result := GravityEng.gravity_object_debug(obj, is_free);
end;

function gravity_object_deserialize(vm: Pgravity_vm;
  entry: Pjson_value): Pgravity_object_t;
begin
  result := GravityEng.gravity_object_deserialize(vm, entry);
end;

procedure gravity_object_free(vm: Pgravity_vm;
  obj: Pgravity_object_t);
begin
  GravityEng.gravity_object_free(vm, obj);
end;

procedure gravity_object_serialize(obj: Pgravity_object_t;
  json: Pjson_t);
begin
  GravityEng.gravity_object_serialize(obj, json);
end;

function gravity_object_size(vm: Pgravity_vm;
  obj: Pgravity_object_t): UInt32;
begin
  result := GravityEng.gravity_object_size(vm, obj);
end;

procedure gravity_list_append_list(vm: Pgravity_vm;
  list1: Pgravity_list_t; list2: Pgravity_list_t);
begin
  GravityEng.gravity_list_append_list(vm, list1, list2);
end;

procedure gravity_list_blacken(vm: Pgravity_vm;
  list: Pgravity_list_t);
begin
  GravityEng.gravity_list_blacken(vm, list);
end;

procedure gravity_list_free(vm: Pgravity_vm;
  list: Pgravity_list_t);
begin
  GravityEng.gravity_list_free(vm, list);
end;

function gravity_list_from_array(vm: Pgravity_vm; n: UInt32;
  p: Pgravity_value_t): Pgravity_list_t;
begin
  result := GravityEng.gravity_list_from_array(vm, n, p);
end;

function gravity_list_new(vm: Pgravity_vm; n: UInt32)
  : Pgravity_list_t;
begin
  result := GravityEng.gravity_list_new(vm, n);
end;

function gravity_list_size(vm: Pgravity_vm;
  list: Pgravity_list_t): UInt32;
begin
  result := GravityEng.gravity_list_size(vm, list);
end;

procedure gravity_map_blacken(vm: Pgravity_vm;
  map: Pgravity_map_t);
begin
  GravityEng.gravity_map_blacken(vm, map);
end;

procedure gravity_map_append_map(vm: Pgravity_vm;
  map1: Pgravity_map_t; map2: Pgravity_map_t);
begin
  GravityEng.gravity_map_append_map(vm, map1, map2);
end;

procedure gravity_map_free(vm: Pgravity_vm; map: Pgravity_map_t);
begin
  GravityEng.gravity_map_free(vm, map);
end;

procedure gravity_map_insert(vm: Pgravity_vm; map: Pgravity_map_t;
  key: gravity_value_t; value: gravity_value_t);
begin
  GravityEng.gravity_map_insert(vm, map, key, value);
end;

function gravity_map_new(vm: Pgravity_vm; n: UInt32)
  : Pgravity_map_t;
begin
  result := GravityEng.gravity_map_new(vm, n);
end;

function gravity_map_size(vm: Pgravity_vm;
  map: Pgravity_map_t): UInt32;
begin
  result := GravityEng.gravity_map_size(vm, map);
end;

procedure gravity_range_blacken(vm: Pgravity_vm;
  range: Pgravity_range_t);
begin
  GravityEng.gravity_range_blacken(vm, range);
end;

procedure gravity_range_free(vm: Pgravity_vm;
  range: Pgravity_range_t);
begin
  GravityEng.gravity_range_free(vm, range);
end;

function gravity_range_new(vm: Pgravity_vm; from: gravity_int_t;
  _to: gravity_int_t; inclusive: Boolean): Pgravity_range_t;
begin
  result := GravityEng.gravity_range_new(vm, from, _to, inclusive);
end;

function gravity_range_size(vm: Pgravity_vm;
  range: Pgravity_range_t): UInt32;
begin
  result := GravityEng.gravity_range_size(vm, range);
end;

procedure gravity_string_blacken(vm: Pgravity_vm;
  _string: Pgravity_string_t);
begin
  GravityEng.gravity_string_blacken(vm, _string);
end;

procedure gravity_string_free(vm: Pgravity_vm;
  value: Pgravity_string_t);
begin
  GravityEng.gravity_string_free(vm, value);
end;

function gravity_string_new(vm: Pgravity_vm; s: PAnsiChar;
  len: UInt32; alloc: UInt32): Pgravity_string_t;
begin
  result := GravityEng.gravity_string_new(vm, s, len, alloc);
end;

procedure gravity_string_set(obj: Pgravity_string_t; s: PAnsiChar;
  len: UInt32);
begin
  GravityEng.gravity_string_set(obj, s, len);
end;

function gravity_string_size(vm: Pgravity_vm;
  _string: Pgravity_string_t): UInt32;
begin
  result := GravityEng.gravity_string_size(vm, _string);
end;

function gravity_string_to_value(vm: Pgravity_vm;
  const s: AnsiString; len: UInt32): gravity_value_t;
begin
  result := GravityEng.gravity_string_to_value(vm, s, len);
end;

function gravity_compiler_create(delegate: Pgravity_delegate_t)
  : Pgravity_compiler_t;
begin
  result := GravityEng.gravity_compiler_create(delegate);
end;

function gravity_compiler_run(compiler: Pgravity_compiler_t;
  const source: AnsiString; len: NativeUInt; fileid: UInt32; is_static: Boolean;
  add_debug: Boolean): Pgravity_closure_t;
begin
  result := GravityEng.gravity_compiler_run(compiler, source, len, fileid,
    is_static, add_debug);
end;

function gravity_compiler_ast(compiler: Pgravity_compiler_t)
  : Pgnode_t;
begin
  result := GravityEng.gravity_compiler_ast(compiler);
end;

procedure gravity_compiler_free(compiler: Pgravity_compiler_t);
begin
  GravityEng.gravity_compiler_free(compiler);
end;

function gravity_compiler_serialize(compiler: Pgravity_compiler_t;
  closure: Pgravity_closure_t): Pjson_t;
begin
  result := GravityEng.gravity_compiler_serialize(compiler, closure);
end;

function gravity_compiler_serialize_infile
  (compiler: Pgravity_compiler_t; closure: Pgravity_closure_t;
  const path: AnsiString): Boolean;
begin
  result := GravityEng.gravity_compiler_serialize_infile(compiler, closure, path);
end;

procedure gravity_compiler_transfer(compiler: Pgravity_compiler_t;
  vm: Pgravity_vm);
begin
  GravityEng.gravity_compiler_transfer(compiler, vm);
end;

function gravity_vm_delegate(vm: Pgravity_vm): Pgravity_delegate_t;
begin
  result := GravityEng.gravity_vm_delegate(vm);
end;

function gravity_vm_fiber(vm: Pgravity_vm): Pgravity_fiber_t;
begin
  result := GravityEng.gravity_vm_fiber(vm);
end;

procedure gravity_vm_free(vm: Pgravity_vm);
begin
  GravityEng.gravity_vm_free(vm);
end;

function gravity_vm_getvalue(vm: Pgravity_vm;
  const key: AnsiString; keylen: UInt32): gravity_value_t;
begin
  result := GravityEng.gravity_vm_getvalue(vm, key, keylen);
end;

function gravity_vm_keyindex(vm: Pgravity_vm; index: UInt32)
  : gravity_value_t;
begin
  result := GravityEng.gravity_vm_keyindex(vm, index);
end;

function gravity_vm_ismini(vm: Pgravity_vm): Boolean;
begin
  result := GravityEng.gravity_vm_ismini(vm);
end;

function gravity_vm_isaborted(vm: Pgravity_vm): Boolean;
begin
  result := GravityEng.gravity_vm_isaborted(vm);
end;

procedure gravity_vm_loadclosure(vm: Pgravity_vm;
  closure: Pgravity_closure_t);
begin
  GravityEng.gravity_vm_loadclosure(vm, closure);
end;

function gravity_vm_lookup(vm: Pgravity_vm; key: gravity_value_t)
  : gravity_value_t;
begin
  result := GravityEng.gravity_vm_lookup(vm, key);
end;

function gravity_vm_new(delegate: Pgravity_delegate_t)
  : Pgravity_vm;
begin
  result := GravityEng.gravity_vm_new(delegate);
end;

function gravity_vm_newmini(): Pgravity_vm;
begin
  result := GravityEng.gravity_vm_newmini();
end;

function gravity_vm_result(vm: Pgravity_vm): gravity_value_t;
begin
  result := GravityEng.gravity_vm_result(vm);
end;

function gravity_vm_runclosure(vm: Pgravity_vm;
  closure: Pgravity_closure_t; sender: gravity_value_t;
  params: Pgravity_value_t; nparams: UInt16): Boolean;
begin
  result := GravityEng.gravity_vm_runclosure(vm, closure, sender, params, nparams);
end;

function gravity_vm_runmain(vm: Pgravity_vm;
  closure: Pgravity_closure_t): Boolean;
begin
  result := GravityEng.gravity_vm_runmain(vm, closure);
end;

procedure gravity_vm_set_callbacks(vm: Pgravity_vm;
  vm_transfer: vm_transfer_cb; vm_cleanup: vm_cleanup_cb);
begin
  GravityEng.gravity_vm_set_callbacks(vm, vm_transfer, vm_cleanup);
end;

procedure gravity_vm_setaborted(vm: Pgravity_vm);
begin
  GravityEng.gravity_vm_setaborted(vm);
end;

procedure gravity_vm_seterror(vm: Pgravity_vm;
  const format: AnsiString);
begin
  GravityEng.gravity_vm_seterror(vm, format);
end;

procedure gravity_vm_seterror_string(vm: Pgravity_vm;
  const s: AnsiString);
begin
  GravityEng.gravity_vm_seterror_string(vm, s);
end;

procedure gravity_vm_setfiber(vm: Pgravity_vm;
  fiber: Pgravity_fiber_t);
begin
  GravityEng.gravity_vm_setfiber(vm, fiber);
end;

procedure gravity_vm_setvalue(vm: Pgravity_vm;
  const key: AnsiString; value: gravity_value_t);
begin
  GravityEng.gravity_vm_setvalue(vm, key, value);
end;

function gravity_vm_time(vm: Pgravity_vm): Double;
begin
  result := GravityEng.gravity_vm_time(vm);
end;

procedure gravity_vm_cleanup(vm: Pgravity_vm);
begin
  GravityEng.gravity_vm_cleanup(vm);
end;

procedure gravity_vm_filter(vm: Pgravity_vm;
  cleanup_filter: vm_filter_cb);
begin
  GravityEng.gravity_vm_filter(vm, cleanup_filter);
end;

procedure gravity_vm_transfer(vm: Pgravity_vm;
  obj: Pgravity_object_t);
begin
  GravityEng.gravity_vm_transfer(vm, obj);
end;

procedure gravity_vm_initmodule(vm: Pgravity_vm;
  f: Pgravity_function_t);
begin
  GravityEng.gravity_vm_initmodule(vm, f);
end;

function gravity_vm_loadbuffer(vm: Pgravity_vm;
  const buffer: AnsiString; len: NativeUInt): Pgravity_closure_t;
begin
  result := GravityEng.gravity_vm_loadbuffer(vm, buffer, len);
end;

function gravity_vm_loadfile(vm: Pgravity_vm;
  const path: AnsiString): Pgravity_closure_t;
begin
  result := GravityEng.gravity_vm_loadfile(vm, path);
end;

function gravity_vm_fastlookup(vm: Pgravity_vm;
  c: Pgravity_class_t; index: Integer): Pgravity_closure_t;
begin
  result := GravityEng.gravity_vm_fastlookup(vm, c, index);
end;

function gravity_vm_getdata(vm: Pgravity_vm): Pointer;
begin
  result := GravityEng.gravity_vm_getdata(vm);
end;

function gravity_vm_getslot(vm: Pgravity_vm; index: UInt32)
  : gravity_value_t;
begin
  result := GravityEng.gravity_vm_getslot(vm, index);
end;

procedure gravity_vm_setdata(vm: Pgravity_vm; data: Pointer);
begin
  GravityEng.gravity_vm_setdata(vm, data);
end;

procedure gravity_vm_setslot(vm: Pgravity_vm;
  value: gravity_value_t; index: UInt32);
begin
  GravityEng.gravity_vm_setslot(vm, value, index);
end;

procedure gravity_vm_memupdate(vm: Pgravity_vm;
  value: gravity_int_t);
begin
  GravityEng.gravity_vm_memupdate(vm, value);
end;

function gravity_vm_anonymous(vm: Pgravity_vm): PAnsiChar;
begin
  result := GravityEng.gravity_vm_anonymous(vm);
end;

function gravity_vm_get(vm: Pgravity_vm; const key: AnsiString)
  : gravity_value_t;
begin
  result := GravityEng.gravity_vm_get(vm, key);
end;

function gravity_vm_set(vm: Pgravity_vm; const key: AnsiString;
  value: gravity_value_t): Boolean;
begin
  result := GravityEng.gravity_vm_set(vm, key, value);
end;

procedure gravity_gray_object(vm: Pgravity_vm;
  obj: Pgravity_object_t);
begin
  GravityEng.gravity_gray_object(vm, obj);
end;

procedure gravity_gray_value(vm: Pgravity_vm; v: gravity_value_t);
begin
  GravityEng.gravity_gray_value(vm, v);
end;

procedure gravity_gc_setenabled(vm: Pgravity_vm; enabled: Boolean);
begin
  GravityEng.gravity_gc_setenabled(vm, enabled);
end;

procedure gravity_gc_start(vm: Pgravity_vm);
begin
  GravityEng.gravity_gc_start(vm);
end;

procedure gravity_gc_temppop(vm: Pgravity_vm);
begin
  GravityEng.gravity_gc_temppop(vm);
end;

procedure gravity_gc_temppush(vm: Pgravity_vm;
  obj: Pgravity_object_t);
begin
  GravityEng.gravity_gc_temppush(vm, obj);
end;

function gravity_core_class_from_name(const name: AnsiString)
  : Pgravity_class_t;
begin
  result := GravityEng.gravity_core_class_from_name(name);
end;

procedure gravity_core_free();
begin
  GravityEng.gravity_core_free();
end;

function gravity_core_identifiers(): PPAnsiChar;
begin
  result := GravityEng.gravity_core_identifiers();
end;

procedure gravity_core_register(vm: Pgravity_vm);
begin
  GravityEng.gravity_core_register(vm);
end;

function gravity_iscore_class(c: Pgravity_class_t): Boolean;
begin
  result := GravityEng.gravity_iscore_class(c);
end;

function computed_property_create(vm: Pgravity_vm;
  getter_func: Pgravity_function_t; setter_func: Pgravity_function_t)
  : Pgravity_closure_t;
begin
  result := GravityEng.computed_property_create(vm, getter_func, setter_func);
end;

procedure computed_property_free(c: Pgravity_class_t;
  const name: AnsiString; remove_flag: Boolean);
begin
  GravityEng.computed_property_free(c, name, remove_flag);
end;

function convert_value2bool(vm: Pgravity_vm; v: gravity_value_t)
  : gravity_value_t;
begin
  result := GravityEng.convert_value2bool(vm, v);
end;

function convert_value2float(vm: Pgravity_vm; v: gravity_value_t)
  : gravity_value_t;
begin
  result := GravityEng.convert_value2float(vm, v);
end;

function convert_value2int(vm: Pgravity_vm; v: gravity_value_t)
  : gravity_value_t;
begin
  result := GravityEng.convert_value2int(vm, v);
end;

function convert_value2string(vm: Pgravity_vm; v: gravity_value_t)
  : gravity_value_t;
begin
  result := GravityEng.convert_value2string(vm, v);
end;

function gravity_hash_create(size: UInt32;
  compute: gravity_hash_compute_fn; isequal: gravity_hash_isequal_fn;
  free: gravity_hash_iterate_fn; data: Pointer): Pgravity_hash_t;
begin
  result := GravityEng.gravity_hash_create(size, compute, isequal, free, data);
end;

procedure gravity_hash_free(hashtable: Pgravity_hash_t);
begin
  GravityEng.gravity_hash_free(hashtable);
end;

function gravity_hash_insert(hashtable: Pgravity_hash_t;
  key: gravity_value_t; value: gravity_value_t): Boolean;
begin
  result := GravityEng.gravity_hash_insert(hashtable, key, value);
end;

function gravity_hash_isempty(hashtable: Pgravity_hash_t): Boolean;
begin
  result := GravityEng.gravity_hash_isempty(hashtable);
end;

function gravity_hash_lookup(hashtable: Pgravity_hash_t;
  key: gravity_value_t): Pgravity_value_t;
begin
  result := GravityEng.gravity_hash_lookup(hashtable, key);
end;

function gravity_hash_remove(hashtable: Pgravity_hash_t;
  key: gravity_value_t): Boolean;
begin
  result := GravityEng.gravity_hash_remove(hashtable, key);
end;

procedure gravity_hash_append(hashtable1: Pgravity_hash_t;
  hashtable2: Pgravity_hash_t);
begin
  GravityEng.gravity_hash_append(hashtable1, hashtable2);
end;

function gravity_hash_compute_buffer(const key: AnsiString;
  len: UInt32): UInt32;
begin
  result := GravityEng.gravity_hash_compute_buffer(key, len);
end;

function gravity_hash_compute_float(f: gravity_float_t): UInt32;
begin
  result := GravityEng.gravity_hash_compute_float(f);
end;

function gravity_hash_compute_int(n: gravity_int_t): UInt32;
begin
  result := GravityEng.gravity_hash_compute_int(n);
end;

function gravity_hash_count(hashtable: Pgravity_hash_t): UInt32;
begin
  result := GravityEng.gravity_hash_count(hashtable);
end;

procedure gravity_hash_dump(hashtable: Pgravity_hash_t);
begin
  GravityEng.gravity_hash_dump(hashtable);
end;

procedure gravity_hash_iterate(hashtable: Pgravity_hash_t;
  iterate: gravity_hash_iterate_fn; data: Pointer);
begin
  GravityEng.gravity_hash_iterate(hashtable, iterate, data);
end;

procedure gravity_hash_iterate2(hashtable: Pgravity_hash_t;
  iterate: gravity_hash_iterate2_fn; data1: Pointer; data2: Pointer);
begin
  GravityEng.gravity_hash_iterate2(hashtable, iterate, data1, data2);
end;

function gravity_hash_memsize(hashtable: Pgravity_hash_t): UInt32;
begin
  result := GravityEng.gravity_hash_memsize(hashtable);
end;

procedure gravity_hash_resetfree(hashtable: Pgravity_hash_t);
begin
  GravityEng.gravity_hash_resetfree(hashtable);
end;

procedure gravity_hash_stat(hashtable: Pgravity_hash_t);
begin
  GravityEng.gravity_hash_stat(hashtable);
end;

procedure gravity_hash_transform(hashtable: Pgravity_hash_t;
  iterate: gravity_hash_transform_fn; data: Pointer);
begin
  GravityEng.gravity_hash_transform(hashtable, iterate, data);
end;

procedure gravity_hash_keyfree(table: Pgravity_hash_t;
  key: gravity_value_t; value: gravity_value_t; data: Pointer);
begin
  GravityEng.gravity_hash_keyfree(table, key, value, data);
end;

procedure gravity_hash_keyvaluefree(table: Pgravity_hash_t;
  key: gravity_value_t; value: gravity_value_t; data: Pointer);
begin
  GravityEng.gravity_hash_keyvaluefree(table, key, value, data);
end;

procedure gravity_hash_valuefree(table: Pgravity_hash_t;
  key: gravity_value_t; value: gravity_value_t; data: Pointer);
begin
  GravityEng.gravity_hash_valuefree(table, key, value, data);
end;

end.
