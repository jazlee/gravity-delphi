unit Gravity.GVIntf;

interface

{ .$DEFINE GRAVITY_STATIC_LINK }
uses
  System.Classes, System.SysUtils, System.Generics.Collections, Winapi.Windows;

const
{$IF Defined(WIN32)}
  LIBNAME_DLL = 'gravity.dll';
  _PU = '';
{$ELSEIF Defined(WIN64)}
  LIBNAME_DLL = 'gravity.dll';
  _PU = '';
{$ELSE}
{$MESSAGE Error 'Unsupported platform'}
{$ENDIF}

const
  GRAVITY_MEMORY_DEBUG = 0;
  GRAVITY_LEXEM_DEBUG = 0;
  GRAVITY_LEXER_DEGUB = 0;
  GRAVITY_PARSER_DEBUG = 0;
  GRAVITY_SEMA1_DEBUG = 0;
  GRAVITY_SEMA2_DEBUG = 0;
  GRAVITY_AST_DEBUG = 0;
  GRAVITY_LOOKUP_DEBUG = 0;
  GRAVITY_SYMTABLE_DEBUG = 0;
  GRAVITY_CODEGEN_DEBUG = 0;
  GRAVITY_OPCODE_DEBUG = 0;
  GRAVITY_BYTECODE_DEBUG = 0;
  GRAVITY_REGISTER_DEBUG = 0;
  GRAVITY_FREE_DEBUG = 0;
  GRAVITY_DESERIALIZE_DEBUG = 0;

  MARRAY_DEFAULT_SIZE = 8;

  GRAVITY_VERSION = '0.8.5';
  GRAVITY_VERSION_NUMBER = $000805;

  GRAVITY_ENABLE_DOUBLE = 1;
  GRAVITY_ENABLE_INT64 = 1;
  GRAVITY_COMPUTED_GOTO = 1;
  GRAVITY_NULL_SILENT = 1;
  GRAVITY_MAP_DOTSUGAR = 1;
  MAIN_FUNCTION = 'main';
  ITERATOR_INIT_FUNCTION = 'iterate';
  ITERATOR_NEXT_FUNCTION = 'next';
  INITMODULE_NAME = '$moduleinit';
  CLASS_INTERNAL_INIT_NAME = '$init';
  CLASS_CONSTRUCTOR_NAME = 'init';
  CLASS_DESTRUCTOR_NAME = 'deinit';
  SELF_PARAMETER_NAME = 'self';
  OUTER_IVAR_NAME = 'outer';
  GETTER_FUNCTION_NAME = 'get';
  SETTER_FUNCTION_NAME = 'set';
  SETTER_PARAMETER_NAME = 'value';

  GLOBALS_DEFAULT_SLOT = 4096;
  CPOOL_INDEX_MAX = 4096;
  CPOOL_VALUE_SUPER = CPOOL_INDEX_MAX + 1;
  CPOOL_VALUE_NULL = CPOOL_INDEX_MAX + 2;
  CPOOL_VALUE_UNDEFINED = CPOOL_INDEX_MAX + 3;
  CPOOL_VALUE_ARGUMENTS = CPOOL_INDEX_MAX + 4;
  CPOOL_VALUE_TRUE = CPOOL_INDEX_MAX + 5;
  CPOOL_VALUE_FALSE = CPOOL_INDEX_MAX + 6;
  CPOOL_VALUE_FUNC = CPOOL_INDEX_MAX + 7;
  MAX_INSTRUCTION_OPCODE = 64;
  MAX_REGISTERS = 256;
  MAX_LOCALS = 200;
  MAX_UPVALUES = 200;
  MAX_INLINE_INT = 131072;
  MAX_FIELDSxFLUSH = 64;
  MAX_IVARS = 768;
  MAX_ALLOCATION = 4194304;
  MAX_CCALLS = 100;
  MAX_MEMORY_BLOCK = 157286400;
  DEFAULT_CONTEXT_SIZE = 256;
  DEFAULT_MINSTRING_SIZE = 32;
  DEFAULT_MINSTACK_SIZE = 256;
  DEFAULT_MINCFRAME_SIZE = 32;
  DEFAULT_CG_THRESHOLD = 5 * 1024 * 1024;
  DEFAULT_CG_MINTHRESHOLD = 1024 * 1024;
  DEFAULT_CG_RATIO = 0.5;

  EPSILON = 0.000001;
  MIN_LIST_RESIZE = 12;
  UINT32_MAX = $FFFFFFFF;
  GRAVITY_DATA_REGISTER = UINT32_MAX;
  GRAVITY_FIBER_REGISTER = UINT32_MAX - 1;
  GRAVITY_MSG_REGISTER = UINT32_MAX - 2;
  UINT16_MAX = $FFFF;
  GRAVITY_BRIDGE_INDEX = UINT16_MAX;
  GRAVITY_COMPUTED_INDEX = UINT16_MAX - 1;
  DBL_MAX = 1.7976931348623158E+308;
  GRAVITY_FLOAT_MAX = DBL_MAX;
  DBL_MIN = 2.2250738585072014E-308;
  GRAVITY_FLOAT_MIN = DBL_MIN;
  FLOAT_MAX_DECIMALS = 16;
  FLOAT_EPSILON = 0.00001;
  GRAVITY_INT_MAX = 9223372036854775807;
  GRAVITY_INT_MIN = (-GRAVITY_INT_MAX - 1);

  GRAVITY_VM_GCENABLED = 'gcEnabled';
  GRAVITY_VM_GCMINTHRESHOLD = 'gcMinThreshold';
  GRAVITY_VM_GCTHRESHOLD = 'gcThreshold';
  GRAVITY_VM_GCRATIO = 'gcRatio';
  GRAVITY_VM_MAXCALLS = 'maxCCalls';
  GRAVITY_VM_MAXBLOCK = 'maxBlock';
  GRAVITY_VM_MAXRECURSION = 'maxRecursionDepth';
  GRAVITYHASH_ENABLE_STATS = 1;
  GRAVITYHASH_DEFAULT_SIZE = 32;
  GRAVITYHASH_THRESHOLD = 0.75;
  GRAVITYHASH_MAXENTRIES = 1073741824;

  GRAVITY_CLASS_INT_NAME = 'Int';
  GRAVITY_CLASS_FLOAT_NAME = 'Float';
  GRAVITY_CLASS_BOOL_NAME = 'Bool';
  GRAVITY_CLASS_STRING_NAME = 'String';
  GRAVITY_CLASS_OBJECT_NAME = 'Object';
  GRAVITY_CLASS_CLASS_NAME = 'Class';
  GRAVITY_CLASS_NULL_NAME = 'Null';
  GRAVITY_CLASS_FUNCTION_NAME = 'Func';
  GRAVITY_CLASS_FIBER_NAME = 'Fiber';
  GRAVITY_CLASS_INSTANCE_NAME = 'Instance';
  GRAVITY_CLASS_CLOSURE_NAME = 'Closure';
  GRAVITY_CLASS_LIST_NAME = 'List';
  GRAVITY_CLASS_MAP_NAME = 'Map';
  GRAVITY_CLASS_RANGE_NAME = 'Range';
  GRAVITY_CLASS_UPVALUE_NAME = 'Upvalue';
  GRAVITY_CLASS_SYSTEM_NAME = 'System';

type
  // Forward declarations
  PPAnsiChar = ^PAnsiChar;
  PUInt32 = ^UInt32;
  PUInt16 = ^UInt16;
  PNativeUInt = ^NativeUInt;
  Puint16_r = ^uint16_r;
  Puint32_r = ^uint32_r;
  Pvoid_r = ^void_r;
  Pcstring_r = ^cstring_r;
  Pgtoken_s = ^gtoken_s;
  Pgnode_t = ^gnode_t;
  PPgnode_t = ^Pgnode_t;
  Pgupvalue_t = ^gupvalue_t;
  PPgupvalue_t = ^Pgupvalue_t;
  Pgnode_r = ^gnode_r;
  Pgupvalue_r = ^gupvalue_r;
  Pgnode_location_t = ^gnode_location_t;
  Pgnode_compound_stmt_t = ^gnode_compound_stmt_t;
  Pgnode_label_stmt_t = ^gnode_label_stmt_t;
  Pgnode_flow_stmt_t = ^gnode_flow_stmt_t;
  Pgnode_loop_stmt_t = ^gnode_loop_stmt_t;
  Pgnode_jump_stmt_t = ^gnode_jump_stmt_t;
  Pgnode_function_decl_t = ^gnode_function_decl_t;
  Pgnode_variable_decl_t = ^gnode_variable_decl_t;
  Pgnode_var_t = ^gnode_var_t;
  Pgnode_enum_decl_t = ^gnode_enum_decl_t;
  Pgnode_class_decl_t = ^gnode_class_decl_t;
  Pgnode_module_decl_t = ^gnode_module_decl_t;
  Pgnode_binary_expr_t = ^gnode_binary_expr_t;
  Pgnode_unary_expr_t = ^gnode_unary_expr_t;
  Pgnode_file_expr_t = ^gnode_file_expr_t;
  Pgnode_literal_expr_t = ^gnode_literal_expr_t;
  Pgnode_identifier_expr_t = ^gnode_identifier_expr_t;
  Pgnode_keyword_expr_t = ^gnode_keyword_expr_t;
  Pgnode_postfix_expr_t = ^gnode_postfix_expr_t;
  Pgnode_postfix_subexpr_t = ^gnode_postfix_subexpr_t;
  Pgnode_list_expr_t = ^gnode_list_expr_t;
  Pjson_settings = ^json_settings;
  P_json_object_entry = ^_json_object_entry;
  P_json_value = ^_json_value;
  PP_json_value = ^P_json_value;
  Pgravity_value_t = ^gravity_value_t;
  Pgravity_value_r = ^gravity_value_r;
  Pgravity_gc_s = ^gravity_gc_s;
  Pgravity_function_t = ^gravity_function_t;
  PPgravity_function_t = ^Pgravity_function_t;
  Pupvalue_s = ^upvalue_s;
  Pgravity_closure_s = ^gravity_closure_s;
  Pgravity_list_t = ^gravity_list_t;
  Pgravity_map_t = ^gravity_map_t;
  Pgravity_callframe_t = ^gravity_callframe_t;
  Pfiber_s = ^fiber_s;
  Pgravity_class_s = ^gravity_class_s;
  Pgravity_module_t = ^gravity_module_t;
  Pgravity_instance_t = ^gravity_instance_t;
  Pgravity_string_t = ^gravity_string_t;
  Pgravity_range_t = ^gravity_range_t;
  Pgravity_function_r = ^gravity_function_r;
  Pgravity_class_r = ^gravity_class_r;
  Pgravity_object_r = ^gravity_object_r;
  Perror_desc_t = ^error_desc_t;
  Pgravity_delegate_t = ^gravity_delegate_t;

  Pgravity_vm = Pointer;
  PPgravity_vm = ^Pgravity_vm;
  mode_t = Integer;

  uint16_r = record
    n: NativeUInt;
    m: NativeUInt;
    p: PUInt16;
  end;

  uint32_r = record
    n: NativeUInt;
    m: NativeUInt;
    p: PUInt32;
  end;

  void_r = record
    n: NativeUInt;
    m: NativeUInt;
    p: PPointer;
  end;

  cstring_r = record
    n: NativeUInt;
    m: NativeUInt;
    p: PPAnsiChar;
  end;

  gtoken_t = (TOK_EOF = 0, TOK_ERROR = 1, TOK_COMMENT = 2, TOK_STRING = 3,
    TOK_NUMBER = 4, TOK_IDENTIFIER = 5, TOK_SPECIAL = 6, TOK_MACRO = 7,
    TOK_KEY_FUNC = 8, TOK_KEY_SUPER = 9, TOK_KEY_DEFAULT = 10,
    TOK_KEY_TRUE = 11, TOK_KEY_FALSE = 12, TOK_KEY_IF = 13, TOK_KEY_ELSE = 14,
    TOK_KEY_SWITCH = 15, TOK_KEY_BREAK = 16, TOK_KEY_CONTINUE = 17,
    TOK_KEY_RETURN = 18, TOK_KEY_WHILE = 19, TOK_KEY_REPEAT = 20,
    TOK_KEY_FOR = 21, TOK_KEY_IN = 22, TOK_KEY_ENUM = 23, TOK_KEY_CLASS = 24,
    TOK_KEY_STRUCT = 25, TOK_KEY_PRIVATE = 26, TOK_KEY_FILE = 27,
    TOK_KEY_INTERNAL = 28, TOK_KEY_PUBLIC = 29, TOK_KEY_STATIC = 30,
    TOK_KEY_EXTERN = 31, TOK_KEY_LAZY = 32, TOK_KEY_CONST = 33,
    TOK_KEY_VAR = 34, TOK_KEY_MODULE = 35, TOK_KEY_IMPORT = 36,
    TOK_KEY_CASE = 37, TOK_KEY_EVENT = 38, TOK_KEY_NULL = 39,
    TOK_KEY_UNDEFINED = 40, TOK_KEY_ISA = 41, TOK_KEY_CURRFUNC = 42,
    TOK_KEY_CURRARGS = 43, TOK_OP_SHIFT_LEFT = 44, TOK_OP_SHIFT_RIGHT = 45,
    TOK_OP_MUL = 46, TOK_OP_DIV = 47, TOK_OP_REM = 48, TOK_OP_BIT_AND = 49,
    TOK_OP_ADD = 50, TOK_OP_SUB = 51, TOK_OP_BIT_OR = 52, TOK_OP_BIT_XOR = 53,
    TOK_OP_BIT_NOT = 54, TOK_OP_RANGE_EXCLUDED = 55, TOK_OP_RANGE_INCLUDED = 56,
    TOK_OP_LESS = 57, TOK_OP_LESS_EQUAL = 58, TOK_OP_GREATER = 59,
    TOK_OP_GREATER_EQUAL = 60, TOK_OP_ISEQUAL = 61, TOK_OP_ISNOTEQUAL = 62,
    TOK_OP_ISIDENTICAL = 63, TOK_OP_ISNOTIDENTICAL = 64,
    TOK_OP_PATTERN_MATCH = 65, TOK_OP_AND = 66, TOK_OP_OR = 67,
    TOK_OP_TERNARY = 68, TOK_OP_ASSIGN = 69, TOK_OP_MUL_ASSIGN = 70,
    TOK_OP_DIV_ASSIGN = 71, TOK_OP_REM_ASSIGN = 72, TOK_OP_ADD_ASSIGN = 73,
    TOK_OP_SUB_ASSIGN = 74, TOK_OP_SHIFT_LEFT_ASSIGN = 75,
    TOK_OP_SHIFT_RIGHT_ASSIGN = 76, TOK_OP_BIT_AND_ASSIGN = 77,
    TOK_OP_BIT_OR_ASSIGN = 78, TOK_OP_BIT_XOR_ASSIGN = 79, TOK_OP_NOT = 80,
    TOK_OP_SEMICOLON = 81, TOK_OP_OPEN_PARENTHESIS = 82, TOK_OP_COLON = 83,
    TOK_OP_COMMA = 84, TOK_OP_DOT = 85, TOK_OP_CLOSED_PARENTHESIS = 86,
    TOK_OP_OPEN_SQUAREBRACKET = 87, TOK_OP_CLOSED_SQUAREBRACKET = 88,
    TOK_OP_OPEN_CURLYBRACE = 89, TOK_OP_CLOSED_CURLYBRACE = 90, TOK_END = 91);
  Pgtoken_t = ^gtoken_t;

  gliteral_t = (LITERAL_STRING = 0, LITERAL_FLOAT = 1, LITERAL_INT = 2,
    LITERAL_BOOL = 3, LITERAL_STRING_INTERPOLATED = 4);
  Pgliteral_t = ^gliteral_t;

  gbuiltin_t = (BUILTIN_NONE = 0, BUILTIN_LINE = 1, BUILTIN_COLUMN = 2,
    BUILTIN_FILE = 3, BUILTIN_FUNC = 4, BUILTIN_CLASS = 5);
  Pgbuiltin_t = ^gbuiltin_t;

  gtoken_s = record
    _type: gtoken_t;
    lineno: UInt32;
    colno: UInt32;
    position: UInt32;
    bytes: UInt32;
    length: UInt32;
    fileid: UInt32;
    builtin: gbuiltin_t;
    value: PAnsiChar;
  end;

  gnode_n = (NODE_LIST_STAT = 0, NODE_COMPOUND_STAT = 1, NODE_LABEL_STAT = 2,
    NODE_FLOW_STAT = 3, NODE_JUMP_STAT = 4, NODE_LOOP_STAT = 5,
    NODE_EMPTY_STAT = 6, NODE_ENUM_DECL = 7, NODE_FUNCTION_DECL = 8,
    NODE_VARIABLE_DECL = 9, NODE_CLASS_DECL = 10, NODE_MODULE_DECL = 11,
    NODE_VARIABLE = 12, NODE_BINARY_EXPR = 13, NODE_UNARY_EXPR = 14,
    NODE_FILE_EXPR = 15, NODE_LIST_EXPR = 16, NODE_LITERAL_EXPR = 17,
    NODE_IDENTIFIER_EXPR = 18, NODE_POSTFIX_EXPR = 19, NODE_KEYWORD_EXPR = 20,
    NODE_CALL_EXPR = 21, NODE_SUBSCRIPT_EXPR = 22, NODE_ACCESS_EXPR = 23);
  Pgnode_n = ^gnode_n;

  gnode_location_type = (LOCATION_LOCAL = 0, LOCATION_GLOBAL = 1,
    LOCATION_UPVALUE = 2, LOCATION_CLASS_IVAR_SAME = 3,
    LOCATION_CLASS_IVAR_OUTER = 4);
  Pgnode_location_type = ^gnode_location_type;

  gnode_t = record
    tag: gnode_n;
    refcount: UInt32;
    block_length: UInt32;
    token: gtoken_s;
    is_assignment: Boolean;
    decl: Pointer;
  end;

  gupvalue_t = record
    node: Pgnode_t;
    index: UInt32;
    selfindex: UInt32;
    is_direct: Boolean;
  end;

  gnode_r = record
    n: NativeUInt;
    m: NativeUInt;
    p: PPgnode_t;
  end;

  gupvalue_r = record
    n: NativeUInt;
    m: NativeUInt;
    p: PPgupvalue_t;
  end;

  Psymboltable_t = Pointer;
  PPsymboltable_t = ^Psymboltable_t;

  gnode_location_t = record
    _type: gnode_location_type;
    index: UInt16;
    nup: UInt16;
  end;

  gnode_compound_stmt_t = record
    base: gnode_t;
    symtable: Psymboltable_t;
    stmts: Pgnode_r;
    nclose: UInt32;
  end;

  gnode_list_stmt_t = gnode_compound_stmt_t;

  gnode_label_stmt_t = record
    base: gnode_t;
    expr: Pgnode_t;
    stmt: Pgnode_t;
    label_case: UInt32;
  end;

  gnode_flow_stmt_t = record
    base: gnode_t;
    cond: Pgnode_t;
    stmt: Pgnode_t;
    elsestmt: Pgnode_t;
  end;

  gnode_loop_stmt_t = record
    base: gnode_t;
    cond: Pgnode_t;
    stmt: Pgnode_t;
    expr: Pgnode_t;
    nclose: UInt32;
  end;

  gnode_jump_stmt_t = record
    base: gnode_t;
    expr: Pgnode_t;
  end;

  gnode_function_decl_t = record
    base: gnode_t;
    env: Pgnode_t;
    access: gtoken_t;
    storage: gtoken_t;
    symtable: Psymboltable_t;
    identifier: PAnsiChar;
    params: Pgnode_r;
    block: Pgnode_compound_stmt_t;
    nlocals: UInt16;
    nparams: UInt16;
    has_defaults: Boolean;
    is_closure: Boolean;
    uplist: Pgupvalue_r;
  end;

  gnode_function_expr_t = gnode_function_decl_t;

  gnode_variable_decl_t = record
    base: gnode_t;
    _type: gtoken_t;
    access: gtoken_t;
    storage: gtoken_t;
    decls: Pgnode_r;
  end;

  gnode_var_t = record
    base: gnode_t;
    env: Pgnode_t;
    identifier: PAnsiChar;
    annotation_type: PAnsiChar;
    expr: Pgnode_t;
    access: gtoken_t;
    index: UInt16;
    upvalue: Boolean;
    iscomputed: Boolean;
    vdecl: Pgnode_variable_decl_t;
  end;

  gnode_enum_decl_t = record
    base: gnode_t;
    env: Pgnode_t;
    access: gtoken_t;
    storage: gtoken_t;
    symtable: Psymboltable_t;
    identifier: PAnsiChar;
  end;

  gnode_class_decl_t = record
    base: gnode_t;
    bridge: Boolean;
    is_struct: Boolean;
    env: Pgnode_t;
    access: gtoken_t;
    storage: gtoken_t;
    identifier: PAnsiChar;
    superclass: Pgnode_t;
    super_extern: Boolean;
    protocols: Pgnode_r;
    decls: Pgnode_r;
    symtable: Psymboltable_t;
    data: Pointer;
    nivar: UInt32;
    nsvar: UInt32;
  end;

  gnode_module_decl_t = record
    base: gnode_t;
    env: Pgnode_t;
    access: gtoken_t;
    storage: gtoken_t;
    identifier: PAnsiChar;
    decls: Pgnode_r;
    symtable: Psymboltable_t;
  end;

  gnode_binary_expr_t = record
    base: gnode_t;
    op: gtoken_t;
    left: Pgnode_t;
    right: Pgnode_t;
  end;

  gnode_unary_expr_t = record
    base: gnode_t;
    op: gtoken_t;
    expr: Pgnode_t;
  end;

  gnode_file_expr_t = record
    base: gnode_t;
    identifiers: Pcstring_r;
    location: gnode_location_t;
  end;

  P_anonymous_type_1 = ^_anonymous_type_1;

  _anonymous_type_1 = record
    case Integer of
      0:
        (str: PAnsiChar);
      1:
        (d: Double);
      2:
        (n64: Int64);
      3:
        (r: Pgnode_r);
  end;

  gnode_literal_expr_t = record
    base: gnode_t;
    _type: gliteral_t;
    len: UInt32;
    value: _anonymous_type_1;
  end;

  gnode_identifier_expr_t = record
    base: gnode_t;
    value: PAnsiChar;
    value2: PAnsiChar;
    symbol: Pgnode_t;
    location: gnode_location_t;
    upvalue: Pgupvalue_t;
  end;

  gnode_keyword_expr_t = record
    base: gnode_t;
  end;

  gnode_empty_stmt_t = gnode_keyword_expr_t;
  gnode_base_t = gnode_keyword_expr_t;

  gnode_postfix_expr_t = record
    base: gnode_t;
    id: Pgnode_t;
    list: Pgnode_r;
  end;

  P_anonymous_type_2 = ^_anonymous_type_2;

  _anonymous_type_2 = record
    case Integer of
      0:
        (expr: Pgnode_t);
      1:
        (args: Pgnode_r);
  end;

  gnode_postfix_subexpr_t = record
    base: gnode_t;
    f2: _anonymous_type_2;
  end;

  gnode_list_expr_t = record
    base: gnode_t;
    ismap: Boolean;
    list1: Pgnode_r;
    list2: Pgnode_r;
  end;

  nanotime_t = UInt64;

  json_opt_mask = (json_opt_none = 0, json_opt_need_comma = 1,
    json_opt_prettify = 2, json_opt_no_maptype = 4, json_opt_no_undef = 8,
    json_opt_unused_1 = 16, json_opt_unused_2 = 32, json_opt_unused_3 = 64,
    json_opt_unused_4 = 128, json_opt_unused_5 = 256);
  Pjson_opt_mask = ^json_opt_mask;
  Pjson_t = Pointer;
  PPjson_t = ^Pjson_t;

  json_settings = record
    max_memory: Cardinal;
    settings: Integer;
    memory_alloc: function(p1: NativeUInt; zero: Integer; user_data: Pointer)
      : Pointer; cdecl;
    memory_free: procedure(p1: Pointer; user_data: Pointer); cdecl;
    user_data: Pointer;
    value_extra: NativeUInt;
  end;

  json_type = (json_none = 0, json_object = 1, json_array = 2, json_integer = 3,
    json_double = 4, json_string = 5, json_boolean = 6, json_null = 7);
  Pjson_type = ^json_type;

  _json_object_entry = record
    name: PAnsiChar;
    name_length: Cardinal;
    value: P_json_value;
  end;

  json_object_entry = _json_object_entry;
  Pjson_object_entry = ^json_object_entry;

  P_anonymous_type_3 = ^_anonymous_type_3;

  _anonymous_type_3 = record
    length: Cardinal;
    ptr: PAnsiChar;
  end;

  P_anonymous_type_4 = ^_anonymous_type_4;

  _anonymous_type_4 = record
    length: Cardinal;
    values: Pjson_object_entry;
  end;

  P_anonymous_type_5 = ^_anonymous_type_5;

  _anonymous_type_5 = record
    length: Cardinal;
    values: PP_json_value;
  end;

  P_anonymous_type_6 = ^_anonymous_type_6;

  _anonymous_type_6 = record
    case Integer of
      0:
        (_boolean: Integer);
      1:
        (_integer: Int64);
      2:
        (_double: Double);
      3:
        (_string: _anonymous_type_3);
      4:
        (_object: _anonymous_type_4);
      5:
        (_array: _anonymous_type_5);
  end;

  P_anonymous_type_7 = ^_anonymous_type_7;

  _anonymous_type_7 = record
    case Integer of
      0:
        (next_alloc: P_json_value);
      1:
        (object_mem: Pointer);
  end;

  _json_value = record
    parent: P_json_value;
    _type: json_type;
    u: _anonymous_type_6;
    _reserved: _anonymous_type_7;
  end;

  json_value = _json_value;
  Pjson_value = ^json_value;
  gravity_float_t = Double;
  gravity_int_t = Int64;
  // gravity_class_t = gravity_class_s;
  Pgravity_class_t = ^gravity_class_s;
  PPgravity_class_t = ^Pgravity_class_t;
  // gravity_object_t = gravity_class_s;
  Pgravity_object_t = ^gravity_class_s;
  PPgravity_object_t = ^Pgravity_object_t;

  P_anonymous_type_8 = ^_anonymous_type_8;

  _anonymous_type_8 = record
    case Integer of
      0:
        (n: gravity_int_t);
      1:
        (f: gravity_float_t);
      2:
        (p: Pgravity_object_t);
  end;

  gravity_value_t = record
    isa: Pgravity_class_t;
    f2: _anonymous_type_8;
  end;

  gravity_value_t_array = array of gravity_value_t;
  Pgravity_value_t_array = ^gravity_value_t_array;

  gravity_value_r = record
    n: NativeUInt;
    m: NativeUInt;
    p: Pgravity_value_t;
  end;

  Pgravity_hash_t = Pointer;
  PPgravity_hash_t = ^Pgravity_hash_t;

  gravity_c_internal = function(vm: Pgravity_vm; args: Pgravity_value_t;
    nargs: UInt16; rindex: UInt32): Boolean; cdecl;

  gravity_gc_callback = function(vm: Pgravity_vm; obj: Pgravity_object_t)
    : UInt32; cdecl;

  gravity_special_index = (EXEC_TYPE_SPECIAL_GETTER = 0,
    EXEC_TYPE_SPECIAL_SETTER = 1);
  Pgravity_special_index = ^gravity_special_index;

  gravity_exec_type = (EXEC_TYPE_NATIVE = 0, EXEC_TYPE_INTERNAL = 1,
    EXEC_TYPE_BRIDGED = 2, EXEC_TYPE_SPECIAL = 3);
  Pgravity_exec_type = ^gravity_exec_type;

  gravity_gc_s = record
    isdark: Boolean;
    visited: Boolean;
    free: gravity_gc_callback;
    size: gravity_gc_callback;
    blacken: gravity_gc_callback;
    next: Pgravity_object_t;
  end;

  gravity_gc_t = gravity_gc_s;

  P_anonymous_type_9 = ^_anonymous_type_9;

  _anonymous_type_9 = record
    cpool: gravity_value_r;
    pvalue: gravity_value_r;
    pname: gravity_value_r;
    ninsts: UInt32;
    bytecode: PUInt32;
    lineno: PUInt32;
    purity: Single;
    useargs: Boolean;
  end;

  P_anonymous_type_10 = ^_anonymous_type_10;

  _anonymous_type_10 = record
    index: UInt16;
    special: array [0 .. 1] of Pointer;
  end;

  P_anonymous_type_11 = ^_anonymous_type_11;

  _anonymous_type_11 = record
    case Integer of
      0:
        (f1: _anonymous_type_9);
      1:
        (internal: gravity_c_internal);
      2:
        (f3: _anonymous_type_10);
  end;

  gravity_function_t = record
    isa: Pgravity_class_t;
    gc: gravity_gc_t;
    xdata: Pointer;
    identifier: PAnsiChar;
    nparams: UInt16;
    nlocals: UInt16;
    ntemps: UInt16;
    nupvalues: UInt16;
    tag: gravity_exec_type;
    f10: _anonymous_type_11;
  end;

  upvalue_s = record
    isa: Pgravity_class_t;
    gc: gravity_gc_t;
    value: Pgravity_value_t;
    closed: gravity_value_t;
    next: Pupvalue_s;
  end;

  gravity_upvalue_t = upvalue_s;
  Pgravity_upvalue_t = ^gravity_upvalue_t;
  PPgravity_upvalue_t = ^Pgravity_upvalue_t;

  gravity_closure_s = record
    isa: Pgravity_class_t;
    gc: gravity_gc_t;
    vm: Pgravity_vm;
    f: Pgravity_function_t;
    context: Pgravity_object_t;
    upvalue: PPgravity_upvalue_t;
    refcount: UInt32;
  end;

  gravity_closure_t = gravity_closure_s;
  Pgravity_closure_t = ^gravity_closure_t;

  gravity_list_t = record
    isa: Pgravity_class_t;
    gc: gravity_gc_t;
    _array: gravity_value_r;
  end;

  gravity_map_t = record
    isa: Pgravity_class_t;
    gc: gravity_gc_t;
    hash: Pgravity_hash_t;
  end;

  gravity_callframe_t = record
    ip: PUInt32;
    dest: UInt32;
    nargs: UInt16;
    args: Pgravity_list_t;
    closure: Pgravity_closure_t;
    stackstart: Pgravity_value_t;
    outloop: Boolean;
  end;

  gravity_fiber_status = (FIBER_NEVER_EXECUTED = 0,
    FIBER_ABORTED_WITH_ERROR = 1, FIBER_TERMINATED = 2, FIBER_RUNNING = 3,
    FIBER_TRYING = 4);
  Pgravity_fiber_status = ^gravity_fiber_status;

  fiber_s = record
    isa: Pgravity_class_t;
    gc: gravity_gc_t;
    stack: Pgravity_value_t;
    stacktop: Pgravity_value_t;
    stackalloc: UInt32;
    frames: Pgravity_callframe_t;
    nframes: UInt32;
    framesalloc: UInt32;
    upvalues: Pgravity_upvalue_t;
    error: PAnsiChar;
    trying: Boolean;
    caller: Pfiber_s;
    result: gravity_value_t;
    status: gravity_fiber_status;
    lasttime: nanotime_t;
    timewait: gravity_float_t;
    elapsedtime: gravity_float_t;
  end;

  gravity_fiber_t = fiber_s;
  Pgravity_fiber_t = ^gravity_fiber_t;

  gravity_class_s = record
    isa: Pgravity_class_t;
    gc: gravity_gc_t;
    objclass: Pgravity_class_t;
    identifier: PAnsiChar;
    has_outer: Boolean;
    is_struct: Boolean;
    is_inited: Boolean;
    unused: Boolean;
    xdata: Pointer;
    superclass: Pgravity_class_s;
    superlook: PAnsiChar;
    htable: Pgravity_hash_t;
    nivars: UInt32;
    ivars: Pgravity_value_t;
  end;

  gravity_module_t = record
    isa: Pgravity_class_t;
    gc: gravity_gc_t;
    identifier: PAnsiChar;
    htable: Pgravity_hash_t;
  end;

  gravity_instance_t = record
    isa: Pgravity_class_t;
    gc: gravity_gc_t;
    objclass: Pgravity_class_t;
    xdata: Pointer;
    ivars: Pgravity_value_t;
  end;

  gravity_string_t = record
    isa: Pgravity_class_t;
    gc: gravity_gc_t;
    s: PAnsiChar;
    hash: UInt32;
    len: UInt32;
    alloc: UInt32;
  end;

  gravity_range_t = record
    isa: Pgravity_class_t;
    gc: gravity_gc_t;
    from: gravity_int_t;
    _to: gravity_int_t;
  end;

  gravity_function_r = record
    n: NativeUInt;
    m: NativeUInt;
    p: PPgravity_function_t;
  end;

  gravity_class_r = record
    n: NativeUInt;
    m: NativeUInt;
    p: PPgravity_class_t;
  end;

  gravity_object_r = record
    n: NativeUInt;
    m: NativeUInt;
    p: PPgravity_object_t;
  end;

  error_type_t = (GRAVITY_ERROR_NONE = 0, GRAVITY_ERROR_SYNTAX = 1,
    GRAVITY_ERROR_SEMANTIC = 2, GRAVITY_ERROR_RUNTIME = 3, GRAVITY_ERROR_IO = 4,
    GRAVITY_WARNING = 5);
  Perror_type_t = ^error_type_t;

  error_desc_t = record
    lineno: UInt32;
    colno: UInt32;
    fileid: UInt32;
    offset: UInt32;
  end;

  code_dump_function = procedure(code: Pointer); cdecl;

  gravity_error_callback = procedure(vm: Pgravity_vm; error_type: error_type_t;
    const description: PAnsiChar; error_desc: error_desc_t;
    xdata: Pointer); cdecl;
  gravity_log_callback = procedure(vm: Pgravity_vm; const msg: PAnsiChar;
    xdata: Pointer); cdecl;
  gravity_log_clear = procedure(vm: Pgravity_vm; xdata: Pointer); cdecl;
  gravity_unittest_callback = procedure(vm: Pgravity_vm;
    error_type: error_type_t; const desc: PAnsiChar; const note: PAnsiChar;
    value: gravity_value_t; row: Int32; col: Int32; xdata: Pointer); cdecl;
  gravity_filename_callback = function(fileid: UInt32; xdata: Pointer)
    : PAnsiChar; cdecl;
  gravity_loadfile_callback = function(const filename: PAnsiChar;
    size: PNativeUInt; fileid: PUInt32; xdata: Pointer; is_static: PBoolean)
    : PAnsiChar; cdecl;
  gravity_optclass_callback = function(xdata: Pointer): PPAnsiChar; cdecl;
  gravity_parser_callback = procedure(token: Pointer; xdata: Pointer); cdecl;
  gravity_precode_callback = function(xdata: Pointer): PAnsiChar; cdecl;
  gravity_type_callback = procedure(token: Pointer; const _type: PAnsiChar;
    xdata: Pointer); cdecl;
  gravity_bridge_blacken = procedure(vm: Pgravity_vm; xdata: Pointer); cdecl;
  gravity_bridge_clone = function(vm: Pgravity_vm; xdata: Pointer)
    : Pointer; cdecl;
  gravity_bridge_equals = function(vm: Pgravity_vm; obj1: Pointer;
    obj2: Pointer): Boolean; cdecl;
  gravity_bridge_execute = function(vm: Pgravity_vm; xdata: Pointer;
    ctx: gravity_value_t; args: Pgravity_value_t; nargs: Int16; vindex: UInt32)
    : Boolean; cdecl;
  gravity_bridge_free = procedure(vm: Pgravity_vm;
    obj: Pgravity_object_t); cdecl;
  gravity_bridge_getundef = function(vm: Pgravity_vm; xdata: Pointer;
    target: gravity_value_t; const key: PAnsiChar; vindex: UInt32)
    : Boolean; cdecl;
  gravity_bridge_getvalue = function(vm: Pgravity_vm; xdata: Pointer;
    target: gravity_value_t; const key: PAnsiChar; vindex: UInt32)
    : Boolean; cdecl;
  gravity_bridge_initinstance = function(vm: Pgravity_vm; xdata: Pointer;
    ctx: gravity_value_t; instance: Pgravity_instance_t; args: Pgravity_value_t;
    nargs: Int16): Boolean; cdecl;
  gravity_bridge_setvalue = function(vm: Pgravity_vm; xdata: Pointer;
    target: gravity_value_t; const key: PAnsiChar; value: gravity_value_t)
    : Boolean; cdecl;
  gravity_bridge_setundef = function(vm: Pgravity_vm; xdata: Pointer;
    target: gravity_value_t; const key: PAnsiChar; value: gravity_value_t)
    : Boolean; cdecl;
  gravity_bridge_size = function(vm: Pgravity_vm; obj: Pgravity_object_t)
    : UInt32; cdecl;
  gravity_bridge_string = function(vm: Pgravity_vm; xdata: Pointer;
    len: PUInt32): PAnsiChar; cdecl;

  vm_cleanup_cb = procedure(vm: Pgravity_vm); cdecl;
  vm_filter_cb = function(obj: Pgravity_object_t): Boolean; cdecl;
  vm_transfer_cb = procedure(vm: Pgravity_vm; obj: Pgravity_object_t); cdecl;

  gravity_hash_compute_fn = function(key: gravity_value_t): UInt32; cdecl;
  gravity_hash_isequal_fn = function(v1: gravity_value_t; v2: gravity_value_t)
    : Boolean; cdecl;
  gravity_hash_iterate_fn = procedure(hashtable: Pgravity_hash_t;
    key: gravity_value_t; value: gravity_value_t; data: Pointer); cdecl;
  gravity_hash_iterate2_fn = procedure(hashtable: Pgravity_hash_t;
    key: gravity_value_t; value: gravity_value_t; data1: Pointer;
    data2: Pointer); cdecl;
  gravity_hash_transform_fn = procedure(hashtable: Pgravity_hash_t;
    key: gravity_value_t; value: Pgravity_value_t; data: Pointer); cdecl;

  gravity_delegate_t = record
    xdata: Pointer;
    report_null_errors: Boolean;
    disable_gccheck_1: Boolean;
    log_callback: gravity_log_callback;
    log_clear: gravity_log_clear;
    error_callback: gravity_error_callback;
    unittest_callback: gravity_unittest_callback;
    parser_callback: gravity_parser_callback;
    type_callback: gravity_type_callback;
    precode_callback: gravity_precode_callback;
    loadfile_callback: gravity_loadfile_callback;
    filename_callback: gravity_filename_callback;
    optional_classes: gravity_optclass_callback;

    bridge_initinstance: gravity_bridge_initinstance;
    bridge_setvalue: gravity_bridge_setvalue;
    bridge_getvalue: gravity_bridge_getvalue;
    bridge_setundef: gravity_bridge_setundef;
    bridge_getundef: gravity_bridge_getundef;
    bridge_execute: gravity_bridge_execute;
    bridge_blacken: gravity_bridge_blacken;
    bridge_string: gravity_bridge_string;
    bridge_equals: gravity_bridge_equals;
    bridge_clone: gravity_bridge_clone;
    bridge_size: gravity_bridge_size;
    bridge_free: gravity_bridge_free;
  end;

  Pgravity_compiler_t = ^gravity_compiler_t;
  PPgravity_compiler_t = ^Pgravity_compiler_t;

  gravity_compiler_t = record
    parser: Pointer;
    delegate: Pgravity_delegate_t;
    storage: Pointer;
    ast: Pointer;
    objects: Pointer;
  end;

  Tgravity_module_new = function(vm: Pgravity_vm; const identifier: PAnsiChar)
    : Pgravity_module_t; cdecl;
  Tgravity_module_free = procedure(vm: Pgravity_vm;
    m: Pgravity_module_t); cdecl;
  Tgravity_module_blacken = procedure(vm: Pgravity_vm;
    m: Pgravity_module_t); cdecl;
  Tgravity_module_size = function(vm: Pgravity_vm; m: Pgravity_module_t)
    : UInt32; cdecl;

  Tgravity_bytecode_deserialize = function(const buffer: PAnsiChar;
    len: NativeUInt; ninst: PUInt32): PUInt32; cdecl;

  Tgravity_function_blacken = procedure(vm: Pgravity_vm;
    f: Pgravity_function_t); cdecl;
  Tgravity_function_cpool_add = function(vm: Pgravity_vm;
    f: Pgravity_function_t; v: gravity_value_t): UInt16; cdecl;
  Tgravity_function_cpool_get = function(f: Pgravity_function_t; i: UInt16)
    : gravity_value_t; cdecl;
  Tgravity_function_deserialize = function(vm: Pgravity_vm; json: Pjson_value)
    : Pgravity_function_t; cdecl;
  Tgravity_function_dump = procedure(f: Pgravity_function_t;
    codef: code_dump_function); cdecl;
  Tgravity_function_free = procedure(vm: Pgravity_vm;
    f: Pgravity_function_t); cdecl;
  Tgravity_function_new = function(vm: Pgravity_vm; const identifier: PAnsiChar;
    nparams: UInt16; nlocals: UInt16; ntemps: UInt16; code: Pointer)
    : Pgravity_function_t; cdecl;
  Tgravity_function_new_bridged = function(vm: Pgravity_vm;
    const identifier: PAnsiChar; xdata: Pointer): Pgravity_function_t; cdecl;
  Tgravity_function_new_internal = function(vm: Pgravity_vm;
    const identifier: PAnsiChar; exec: gravity_c_internal; nparams: UInt16)
    : Pgravity_function_t; cdecl;
  Tgravity_function_new_special = function(vm: Pgravity_vm;
    const identifier: PAnsiChar; index: UInt16; getter: Pointer;
    setter: Pointer): Pgravity_function_t; cdecl;
  Tgravity_function_serialize = procedure(f: Pgravity_function_t;
    json: Pjson_t); cdecl;
  Tgravity_function_setxdata = procedure(f: Pgravity_function_t;
    xdata: Pointer); cdecl;
  Tgravity_function_size = function(vm: Pgravity_vm; f: Pgravity_function_t)
    : UInt32; cdecl;

  Tgravity_closure_blacken = procedure(vm: Pgravity_vm;
    closure: Pgravity_closure_t); cdecl;
  Tgravity_closure_dec_refcount = procedure(vm: Pgravity_vm;
    closure: Pgravity_closure_t); cdecl;
  Tgravity_closure_inc_refcount = procedure(vm: Pgravity_vm;
    closure: Pgravity_closure_t); cdecl;
  Tgravity_closure_free = procedure(vm: Pgravity_vm;
    closure: Pgravity_closure_t); cdecl;
  Tgravity_closure_size = function(vm: Pgravity_vm; closure: Pgravity_closure_t)
    : UInt32; cdecl;
  Tgravity_closure_new = function(vm: Pgravity_vm; f: Pgravity_function_t)
    : Pgravity_closure_t; cdecl;

  Tgravity_upvalue_blacken = procedure(vm: Pgravity_vm;
    upvalue: Pgravity_upvalue_t); cdecl;
  Tgravity_upvalue_free = procedure(vm: Pgravity_vm;
    upvalue: Pgravity_upvalue_t); cdecl;
  Tgravity_upvalue_new = function(vm: Pgravity_vm; value: Pgravity_value_t)
    : Pgravity_upvalue_t; cdecl;
  Tgravity_upvalue_size = function(vm: Pgravity_vm; upvalue: Pgravity_upvalue_t)
    : UInt32; cdecl;

  Tgravity_class_blacken = procedure(vm: Pgravity_vm;
    c: Pgravity_class_t); cdecl;
  Tgravity_class_add_ivar = function(c: Pgravity_class_t;
    const identifier: PAnsiChar): Int16; cdecl;
  Tgravity_class_bind = procedure(c: Pgravity_class_t; const key: PAnsiChar;
    value: gravity_value_t); cdecl;
  Tgravity_class_count_ivars = function(c: Pgravity_class_t): UInt32; cdecl;
  Tgravity_class_deserialize = function(vm: Pgravity_vm; json: Pjson_value)
    : Pgravity_class_t; cdecl;
  Tgravity_class_dump = procedure(c: Pgravity_class_t); cdecl;
  Tgravity_class_free = procedure(vm: Pgravity_vm; c: Pgravity_class_t); cdecl;
  Tgravity_class_free_core = procedure(vm: Pgravity_vm;
    c: Pgravity_class_t); cdecl;
  Tgravity_class_get_meta = function(c: Pgravity_class_t)
    : Pgravity_class_t; cdecl;
  Tgravity_class_getsuper = function(c: Pgravity_class_t)
    : Pgravity_class_t; cdecl;
  Tgravity_class_grow = function(c: Pgravity_class_t; n: UInt32)
    : Boolean; cdecl;
  Tgravity_class_is_meta = function(c: Pgravity_class_t): Boolean; cdecl;
  Tgravity_class_lookup = function(c: Pgravity_class_t; key: gravity_value_t)
    : Pgravity_object_t; cdecl;
  Tgravity_class_lookup_closure = function(c: Pgravity_class_t;
    key: gravity_value_t): Pgravity_closure_t; cdecl;
  Tgravity_class_lookup_constructor = function(c: Pgravity_class_t;
    nparams: UInt32): Pgravity_closure_t; cdecl;
  Tgravity_class_new_pair = function(vm: Pgravity_vm;
    const identifier: PAnsiChar; superclass: Pgravity_class_t; nivar: UInt32;
    nsvar: UInt32): Pgravity_class_t; cdecl;
  Tgravity_class_new_single = function(vm: Pgravity_vm;
    const identifier: PAnsiChar; nfields: UInt32): Pgravity_class_t; cdecl;
  Tgravity_class_serialize = procedure(c: Pgravity_class_t;
    json: Pjson_t); cdecl;
  Tgravity_class_setsuper = function(subclass: Pgravity_class_t;
    superclass: Pgravity_class_t): Boolean; cdecl;
  Tgravity_class_setxdata = procedure(c: Pgravity_class_t;
    xdata: Pointer); cdecl;
  Tgravity_class_size = function(vm: Pgravity_vm; c: Pgravity_class_t)
    : UInt32; cdecl;

  Tgravity_fiber_blacken = procedure(vm: Pgravity_vm;
    fiber: Pgravity_fiber_t); cdecl;
  Tgravity_fiber_free = procedure(vm: Pgravity_vm;
    fiber: Pgravity_fiber_t); cdecl;
  Tgravity_fiber_new = function(vm: Pgravity_vm; closure: Pgravity_closure_t;
    nstack: UInt32; nframes: UInt32): Pgravity_fiber_t; cdecl;
  Tgravity_fiber_reassign = procedure(fiber: Pgravity_fiber_t;
    closure: Pgravity_closure_t; nargs: UInt16); cdecl;
  Tgravity_fiber_seterror = procedure(fiber: Pgravity_fiber_t;
    const error: PAnsiChar); cdecl;
  Tgravity_fiber_size = function(vm: Pgravity_vm; fiber: Pgravity_fiber_t)
    : UInt32; cdecl;

  Tgravity_instance_blacken = procedure(vm: Pgravity_vm;
    i: Pgravity_instance_t); cdecl;
  Tgravity_instance_clone = function(vm: Pgravity_vm;
    src_instance: Pgravity_instance_t): Pgravity_instance_t; cdecl;
  Tgravity_instance_free = procedure(vm: Pgravity_vm;
    i: Pgravity_instance_t); cdecl;
  Tgravity_instance_lookup_event = function(i: Pgravity_instance_t;
    const name: PAnsiChar): Pgravity_closure_t; cdecl;
  Tgravity_instance_new = function(vm: Pgravity_vm; c: Pgravity_class_t)
    : Pgravity_instance_t; cdecl;
  Tgravity_instance_setivar = procedure(instance: Pgravity_instance_t;
    idx: UInt32; value: gravity_value_t); cdecl;
  Tgravity_instance_setxdata = procedure(i: Pgravity_instance_t;
    xdata: Pointer); cdecl;
  Tgravity_instance_size = function(vm: Pgravity_vm; i: Pgravity_instance_t)
    : UInt32; cdecl;

  Tgravity_value_dump = procedure(vm: Pgravity_vm; v: gravity_value_t;
    buffer: PAnsiChar; len: UInt16); cdecl;
  Tgravity_value_equals = function(v1: gravity_value_t; v2: gravity_value_t)
    : Boolean; cdecl;
  Tgravity_value_free = procedure(vm: Pgravity_vm; v: gravity_value_t); cdecl;
  Tgravity_value_getclass = function(v: gravity_value_t)
    : Pgravity_class_t; cdecl;
  Tgravity_value_getsuper = function(v: gravity_value_t)
    : Pgravity_class_t; cdecl;
  Tgravity_value_hash = function(value: gravity_value_t): UInt32; cdecl;
  Tgravity_value_isobject = function(v: gravity_value_t): Boolean; cdecl;
  Tgravity_value_serialize = procedure(const key: PAnsiChar; v: gravity_value_t;
    json: Pjson_t); cdecl;
  Tgravity_value_size = function(vm: Pgravity_vm; v: gravity_value_t)
    : UInt32; cdecl;
  Tgravity_value_xdata = function(value: gravity_value_t): Pointer; cdecl;

  Tgravity_value_from_bool = function(b: Boolean): gravity_value_t; cdecl;
  Tgravity_value_from_error = function(const msg: PAnsiChar)
    : gravity_value_t; cdecl;
  Tgravity_value_from_float = function(f: gravity_float_t)
    : gravity_value_t; cdecl;
  Tgravity_value_from_int = function(n: gravity_int_t): gravity_value_t; cdecl;
  Tgravity_value_from_null = function(): gravity_value_t; cdecl;
  Tgravity_value_from_object = function(obj: Pointer): gravity_value_t; cdecl;
  Tgravity_value_from_undefined = function(): gravity_value_t; cdecl;

  Tgravity_object_blacken = procedure(vm: Pgravity_vm;
    obj: Pgravity_object_t); cdecl;
  Tgravity_object_debug = function(obj: Pgravity_object_t; is_free: Boolean)
    : PAnsiChar; cdecl;
  Tgravity_object_deserialize = function(vm: Pgravity_vm; entry: Pjson_value)
    : Pgravity_object_t; cdecl;
  Tgravity_object_free = procedure(vm: Pgravity_vm;
    obj: Pgravity_object_t); cdecl;
  Tgravity_object_serialize = procedure(obj: Pgravity_object_t;
    json: Pjson_t); cdecl;
  Tgravity_object_size = function(vm: Pgravity_vm; obj: Pgravity_object_t)
    : UInt32; cdecl;

  Tgravity_list_append_list = procedure(vm: Pgravity_vm; list1: Pgravity_list_t;
    list2: Pgravity_list_t); cdecl;
  Tgravity_list_blacken = procedure(vm: Pgravity_vm;
    list: Pgravity_list_t); cdecl;
  Tgravity_list_free = procedure(vm: Pgravity_vm; list: Pgravity_list_t); cdecl;
  Tgravity_list_from_array = function(vm: Pgravity_vm; n: UInt32;
    p: Pgravity_value_t): Pgravity_list_t; cdecl;
  Tgravity_list_new = function(vm: Pgravity_vm; n: UInt32)
    : Pgravity_list_t; cdecl;
  Tgravity_list_size = function(vm: Pgravity_vm; list: Pgravity_list_t)
    : UInt32; cdecl;

  Tgravity_map_blacken = procedure(vm: Pgravity_vm; map: Pgravity_map_t); cdecl;
  Tgravity_map_append_map = procedure(vm: Pgravity_vm; map1: Pgravity_map_t;
    map2: Pgravity_map_t); cdecl;
  Tgravity_map_free = procedure(vm: Pgravity_vm; map: Pgravity_map_t); cdecl;
  Tgravity_map_insert = procedure(vm: Pgravity_vm; map: Pgravity_map_t;
    key: gravity_value_t; value: gravity_value_t); cdecl;
  Tgravity_map_new = function(vm: Pgravity_vm; n: UInt32)
    : Pgravity_map_t; cdecl;
  Tgravity_map_size = function(vm: Pgravity_vm; map: Pgravity_map_t)
    : UInt32; cdecl;

  Tgravity_range_blacken = procedure(vm: Pgravity_vm;
    range: Pgravity_range_t); cdecl;
  Tgravity_range_free = procedure(vm: Pgravity_vm;
    range: Pgravity_range_t); cdecl;
  Tgravity_range_new = function(vm: Pgravity_vm; rnfr: gravity_int_t;
    rnto: gravity_int_t; inclusive: Boolean): Pgravity_range_t; cdecl;
  Tgravity_range_size = function(vm: Pgravity_vm; range: Pgravity_range_t)
    : UInt32; cdecl;

  /// MARK: - STRING -
  Tgravity_string_blacken = procedure(vm: Pgravity_vm;
    str: Pgravity_string_t); cdecl;
  Tgravity_string_free = procedure(vm: Pgravity_vm;
    value: Pgravity_string_t); cdecl;
  Tgravity_string_new = function(vm: Pgravity_vm; s: PAnsiChar; len: UInt32;
    alloc: UInt32): Pgravity_string_t; cdecl;
  Tgravity_string_set = procedure(obj: Pgravity_string_t; s: PAnsiChar;
    len: UInt32); cdecl;
  Tgravity_string_size = function(vm: Pgravity_vm; str: Pgravity_string_t)
    : UInt32; cdecl;
  Tgravity_string_to_value = function(vm: Pgravity_vm; const s: PAnsiChar;
    len: UInt32): gravity_value_t; cdecl;

  Tgravity_compiler_create = function(delegate: Pgravity_delegate_t)
    : Pgravity_compiler_t; cdecl;
  Tgravity_compiler_run = function(compiler: Pgravity_compiler_t;
    const source: PAnsiChar; len: NativeUInt; fileid: UInt32;
    is_static: Boolean; add_debug: Boolean): Pgravity_closure_t; cdecl;
  Tgravity_compiler_ast = function(compiler: Pgravity_compiler_t)
    : Pgnode_t; cdecl;
  Tgravity_compiler_free = procedure(compiler: Pgravity_compiler_t); cdecl;
  Tgravity_compiler_serialize = function(compiler: Pgravity_compiler_t;
    closure: Pgravity_closure_t): Pjson_t; cdecl;
  Tgravity_compiler_serialize_infile = function(compiler: Pgravity_compiler_t;
    closure: Pgravity_closure_t; const path: PAnsiChar): Boolean; cdecl;
  Tgravity_compiler_transfer = procedure(compiler: Pgravity_compiler_t;
    vm: Pgravity_vm); cdecl;

  Tgravity_vm_delegate = function(vm: Pgravity_vm): Pgravity_delegate_t; cdecl;
  Tgravity_vm_fiber = function(vm: Pgravity_vm): Pgravity_fiber_t; cdecl;
  Tgravity_vm_free = procedure(vm: Pgravity_vm); cdecl;
  Tgravity_vm_getvalue = function(vm: Pgravity_vm; const key: PAnsiChar;
    keylen: UInt32): gravity_value_t; cdecl;
  Tgravity_vm_keyindex = function(vm: Pgravity_vm; index: UInt32)
    : gravity_value_t; cdecl;
  Tgravity_vm_ismini = function(vm: Pgravity_vm): Boolean; cdecl;
  Tgravity_vm_isaborted = function(vm: Pgravity_vm): Boolean; cdecl;
  Tgravity_vm_loadclosure = procedure(vm: Pgravity_vm;
    closure: Pgravity_closure_t); cdecl;
  Tgravity_vm_lookup = function(vm: Pgravity_vm; key: gravity_value_t)
    : gravity_value_t; cdecl;
  Tgravity_vm_new = function(delegate: Pgravity_delegate_t): Pgravity_vm; cdecl;
  Tgravity_vm_newmini = function(): Pgravity_vm; cdecl;
  Tgravity_vm_result = function(vm: Pgravity_vm): gravity_value_t; cdecl;
  Tgravity_vm_runclosure = function(vm: Pgravity_vm;
    closure: Pgravity_closure_t; sender: gravity_value_t;
    params: Pgravity_value_t; nparams: UInt16): Boolean; cdecl;
  Tgravity_vm_runmain = function(vm: Pgravity_vm; closure: Pgravity_closure_t)
    : Boolean; cdecl;
  Tgravity_vm_set_callbacks = procedure(vm: Pgravity_vm;
    vm_transfer: vm_transfer_cb; vm_cleanup: vm_cleanup_cb); cdecl;
  Tgravity_vm_setaborted = procedure(vm: Pgravity_vm); cdecl;
  Tgravity_vm_seterror = procedure(vm: Pgravity_vm; const format: PAnsiChar)
    varargs; cdecl;
  Tgravity_vm_seterror_string = procedure(vm: Pgravity_vm;
    const s: PAnsiChar); cdecl;
  Tgravity_vm_setfiber = procedure(vm: Pgravity_vm;
    fiber: Pgravity_fiber_t); cdecl;
  Tgravity_vm_setvalue = procedure(vm: Pgravity_vm; const key: PAnsiChar;
    value: gravity_value_t); cdecl;
  Tgravity_vm_time = function(vm: Pgravity_vm): Double; cdecl;

  Tgravity_vm_cleanup = procedure(vm: Pgravity_vm); cdecl;
  Tgravity_vm_filter = procedure(vm: Pgravity_vm;
    cleanup_filter: vm_filter_cb); cdecl;
  Tgravity_vm_transfer = procedure(vm: Pgravity_vm;
    obj: Pgravity_object_t); cdecl;
  Tgravity_vm_initmodule = procedure(vm: Pgravity_vm;
    f: Pgravity_function_t); cdecl;
  Tgravity_vm_loadbuffer = function(vm: Pgravity_vm; const buffer: PAnsiChar;
    len: NativeUInt): Pgravity_closure_t; cdecl;
  Tgravity_vm_loadfile = function(vm: Pgravity_vm; const path: PAnsiChar)
    : Pgravity_closure_t; cdecl;
  Tgravity_vm_fastlookup = function(vm: Pgravity_vm; c: Pgravity_class_t;
    index: Integer): Pgravity_closure_t; cdecl;
  Tgravity_vm_getdata = function(vm: Pgravity_vm): Pointer; cdecl;
  Tgravity_vm_getslot = function(vm: Pgravity_vm; index: UInt32)
    : gravity_value_t; cdecl;
  Tgravity_vm_setdata = procedure(vm: Pgravity_vm; data: Pointer); cdecl;
  Tgravity_vm_setslot = procedure(vm: Pgravity_vm; value: gravity_value_t;
    index: UInt32); cdecl;
  Tgravity_vm_memupdate = procedure(vm: Pgravity_vm;
    value: gravity_int_t); cdecl;
  Tgravity_vm_anonymous = function(vm: Pgravity_vm): PAnsiChar; cdecl;
  Tgravity_vm_get = function(vm: Pgravity_vm; const key: PAnsiChar)
    : gravity_value_t; cdecl;
  Tgravity_vm_set = function(vm: Pgravity_vm; const key: PAnsiChar;
    value: gravity_value_t): Boolean; cdecl;

  Tgravity_gray_object = procedure(vm: Pgravity_vm;
    obj: Pgravity_object_t); cdecl;
  Tgravity_gray_value = procedure(vm: Pgravity_vm; v: gravity_value_t); cdecl;

  Tgravity_gc_setenabled = procedure(vm: Pgravity_vm; enabled: Boolean); cdecl;
  Tgravity_gc_start = procedure(vm: Pgravity_vm); cdecl;
  Tgravity_gc_temppop = procedure(vm: Pgravity_vm); cdecl;
  Tgravity_gc_temppush = procedure(vm: Pgravity_vm;
    obj: Pgravity_object_t); cdecl;

  Tgravity_core_class_from_name = function(const name: PAnsiChar)
    : Pgravity_class_t; cdecl;
  Tgravity_core_free = procedure(); cdecl;
  Tgravity_core_identifiers = function(): PPAnsiChar; cdecl;
  Tgravity_core_register = procedure(vm: Pgravity_vm); cdecl;
  Tgravity_iscore_class = function(c: Pgravity_class_t): Boolean; cdecl;

  Tconvert_value2bool = function(vm: Pgravity_vm; v: gravity_value_t)
    : gravity_value_t; cdecl;
  Tconvert_value2float = function(vm: Pgravity_vm; v: gravity_value_t)
    : gravity_value_t; cdecl;
  Tconvert_value2int = function(vm: Pgravity_vm; v: gravity_value_t)
    : gravity_value_t; cdecl;
  Tconvert_value2string = function(vm: Pgravity_vm; v: gravity_value_t)
    : gravity_value_t; cdecl;

  Tcomputed_property_create = function(vm: Pgravity_vm;
    getter_func: Pgravity_function_t; setter_func: Pgravity_function_t)
    : Pgravity_closure_t; cdecl;
  Tcomputed_property_free = procedure(c: Pgravity_class_t;
    const name: PAnsiChar; remove_flag: Boolean); cdecl;

  Tgravity_hash_create = function(size: UInt32;
    compute: gravity_hash_compute_fn; isequal: gravity_hash_isequal_fn;
    free: gravity_hash_iterate_fn; data: Pointer): Pgravity_hash_t; cdecl;
  Tgravity_hash_free = procedure(hashtable: Pgravity_hash_t); cdecl;
  Tgravity_hash_insert = function(hashtable: Pgravity_hash_t;
    key: gravity_value_t; value: gravity_value_t): Boolean; cdecl;
  Tgravity_hash_isempty = function(hashtable: Pgravity_hash_t): Boolean; cdecl;
  Tgravity_hash_lookup = function(hashtable: Pgravity_hash_t;
    key: gravity_value_t): Pgravity_value_t; cdecl;
  Tgravity_hash_remove = function(hashtable: Pgravity_hash_t;
    key: gravity_value_t): Boolean; cdecl;
  Tgravity_hash_append = procedure(hashtable1: Pgravity_hash_t;
    hashtable2: Pgravity_hash_t); cdecl;
  Tgravity_hash_compute_buffer = function(const key: PAnsiChar; len: UInt32)
    : UInt32; cdecl;
  Tgravity_hash_compute_float = function(f: gravity_float_t): UInt32; cdecl;
  Tgravity_hash_compute_int = function(n: gravity_int_t): UInt32; cdecl;
  Tgravity_hash_count = function(hashtable: Pgravity_hash_t): UInt32; cdecl;
  Tgravity_hash_dump = procedure(hashtable: Pgravity_hash_t); cdecl;
  Tgravity_hash_iterate = procedure(hashtable: Pgravity_hash_t;
    iterate: gravity_hash_iterate_fn; data: Pointer); cdecl;
  Tgravity_hash_iterate2 = procedure(hashtable: Pgravity_hash_t;
    iterate: gravity_hash_iterate2_fn; data1: Pointer; data2: Pointer); cdecl;
  Tgravity_hash_memsize = function(hashtable: Pgravity_hash_t): UInt32; cdecl;
  Tgravity_hash_resetfree = procedure(hashtable: Pgravity_hash_t); cdecl;
  Tgravity_hash_stat = procedure(hashtable: Pgravity_hash_t); cdecl;
  Tgravity_hash_transform = procedure(hashtable: Pgravity_hash_t;
    iterate: gravity_hash_transform_fn; data: Pointer); cdecl;
  Tgravity_hash_keyfree = procedure(table: Pgravity_hash_t;
    key: gravity_value_t; value: gravity_value_t; data: Pointer); cdecl;
  Tgravity_hash_keyvaluefree = procedure(table: Pgravity_hash_t;
    key: gravity_value_t; value: gravity_value_t; data: Pointer); cdecl;
  Tgravity_hash_valuefree = procedure(table: Pgravity_hash_t;
    key: gravity_value_t; value: gravity_value_t; data: Pointer); cdecl;

  IGVInterface = interface
    ['{1CDB9D20-F45B-41D7-94E9-8E7D6D6852E1}']
    function LibraryName: String;
    procedure LoadGVLibrary;
    procedure FreeGVLibrary;
    function TryLoadLibrary: Boolean;
    procedure CheckLibraryLoaded;

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

    function VALUE_AS_OBJECT(val: gravity_value_t): Pgravity_object_t;
    function VALUE_AS_STRING(val: gravity_value_t): Pgravity_string_t;
    function VALUE_AS_FIBER(val: gravity_value_t): Pgravity_fiber_t;
    function VALUE_AS_FUNCTION(val: gravity_value_t): Pgravity_function_t;
    function VALUE_AS_CLOSURE(val: gravity_value_t): Pgravity_closure_t;
    function VALUE_AS_CLASS(val: gravity_value_t): Pgravity_class_t;
    function VALUE_AS_INSTANCE(val: gravity_value_t): Pgravity_instance_t;
    function VALUE_AS_LIST(val: gravity_value_t): Pgravity_list_t;
    function VALUE_AS_MAP(val: gravity_value_t): Pgravity_map_t;
    function VALUE_AS_RANGE(val: gravity_value_t): Pgravity_range_t;
    function VALUE_AS_ERROR(val: gravity_value_t): PAnsiChar;
    function VALUE_AS_FLOAT(val: gravity_value_t): gravity_float_t;
    function VALUE_AS_INT(val: gravity_value_t): gravity_int_t;
    function VALUE_AS_BOOL(val: gravity_value_t): gravity_int_t;
    function VALUE_AS_CSTRING(val: gravity_value_t): PAnsiChar;


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

    function OBJECT_ISA_FUNCTION(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_INSTANCE(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_CLOSURE(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_FIBER(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_CLASS(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_STRING(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_INT(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_FLOAT(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_BOOL(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_LIST(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_MAP(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_RANGE(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_NULL(v: Pgravity_object_t): Boolean;
    function OBJECT_IS_VALID(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_UPVALUE(v: Pgravity_object_t): Boolean;

    function NEW_FUNCTION_BRIDGED(identifier: AnsiString; xdata: pointer): Pgravity_function_t;
    function NEW_FUNCTION_SPECIAL(identifier: AnsiString; index: UInt16; getter, setter: pointer): Pgravity_function_t;
    function NEW_CLOSURE_VALUE_BRIDGED(identifier: AnsiString; xdata: pointer): gravity_value_t;
    function NEW_CLOSURE_VALUE_SPECIAL(identifier: AnsiString;
      index: Uint16; getter, setter: pointer): gravity_value_t;

    function NEW_FUNCTION(fptr: gravity_c_internal): Pgravity_function_t;
    function NEW_CLOSURE_VALUE(fptr: gravity_c_internal): gravity_value_t;

    function GET_VALUE(args: Pgravity_value_t; ndx: UInt16): gravity_value_t;
    procedure SETMETA_INITED(c: Pgravity_class_t);

    function RETURN_VALUE(vm: Pgravity_vm; v: gravity_value_t; index: UInt32): Boolean;
    function RETURN_CLOSURE(vm: Pgravity_vm; v: gravity_value_t; index: UInt32): Boolean;
    function RETURN_FIBER: Boolean;
    function RETURN_NOVALUE: Boolean;
  end;

  TRegisterGVInterface = function: IGVInterface;

procedure RegisterGVInterfaceFactory(ARegisterGVIntf: TRegisterGVInterface);
function GravityEng: IGVInterface;

implementation

var
  FRegisterGVInterface: TRegisterGVInterface;

type
  TGVInterface = class(TInterfacedObject, IGVInterface)
  class var
    DefaultIntf: IGVInterface;
  private
    FGVLibrary: HMODULE;

    Fgravity_class_int: Pgravity_class_t;
    Fgravity_class_float: Pgravity_class_t;
    Fgravity_class_bool: Pgravity_class_t;
    Fgravity_class_null: Pgravity_class_t;

    Fgravity_class_string: Pgravity_class_t;
    Fgravity_class_object: Pgravity_class_t;
    Fgravity_class_function: Pgravity_class_t;
    Fgravity_class_closure: Pgravity_class_t;
    Fgravity_class_fiber: Pgravity_class_t;
    Fgravity_class_class: Pgravity_class_t;
    Fgravity_class_instance: Pgravity_class_t;
    Fgravity_class_list: Pgravity_class_t;
    Fgravity_class_map: Pgravity_class_t;
    Fgravity_class_range: Pgravity_class_t;
    Fgravity_class_upvalue: Pgravity_class_t;
    Fgravity_class_system: Pgravity_class_t;

    Fgravity_module_new: Tgravity_module_new;
    Fgravity_module_free: Tgravity_module_free;
    Fgravity_module_blacken: Tgravity_module_blacken;
    Fgravity_module_size: Tgravity_module_size;
    Fgravity_bytecode_deserialize: Tgravity_bytecode_deserialize;
    Fgravity_function_blacken: Tgravity_function_blacken;
    Fgravity_function_cpool_add: Tgravity_function_cpool_add;
    Fgravity_function_cpool_get: Tgravity_function_cpool_get;
    Fgravity_function_deserialize: Tgravity_function_deserialize;
    Fgravity_function_dump: Tgravity_function_dump;
    Fgravity_function_free: Tgravity_function_free;
    Fgravity_function_new: Tgravity_function_new;
    Fgravity_function_new_bridged: Tgravity_function_new_bridged;
    Fgravity_function_new_internal: Tgravity_function_new_internal;
    Fgravity_function_new_special: Tgravity_function_new_special;
    Fgravity_function_serialize: Tgravity_function_serialize;
    Fgravity_function_setxdata: Tgravity_function_setxdata;
    Fgravity_function_size: Tgravity_function_size;

    Fgravity_closure_blacken: Tgravity_closure_blacken;
    Fgravity_closure_dec_refcount: Tgravity_closure_dec_refcount;
    Fgravity_closure_inc_refcount: Tgravity_closure_inc_refcount;
    Fgravity_closure_free: Tgravity_closure_free;
    Fgravity_closure_size: Tgravity_closure_size;
    Fgravity_closure_new: Tgravity_closure_new;

    Fgravity_upvalue_blacken: Tgravity_upvalue_blacken;
    Fgravity_upvalue_free: Tgravity_upvalue_free;
    Fgravity_upvalue_new: Tgravity_upvalue_new;
    Fgravity_upvalue_size: Tgravity_upvalue_size;
    Fgravity_class_blacken: Tgravity_class_blacken;
    Fgravity_class_add_ivar: Tgravity_class_add_ivar;
    Fgravity_class_bind: Tgravity_class_bind;
    Fgravity_class_count_ivars: Tgravity_class_count_ivars;
    Fgravity_class_deserialize: Tgravity_class_deserialize;
    Fgravity_class_dump: Tgravity_class_dump;
    Fgravity_class_free: Tgravity_class_free;
    Fgravity_class_free_core: Tgravity_class_free_core;
    Fgravity_class_get_meta: Tgravity_class_get_meta;
    Fgravity_class_getsuper: Tgravity_class_getsuper;
    Fgravity_class_grow: Tgravity_class_grow;
    Fgravity_class_is_meta: Tgravity_class_is_meta;
    Fgravity_class_lookup: Tgravity_class_lookup;
    Fgravity_class_lookup_closure: Tgravity_class_lookup_closure;
    Fgravity_class_lookup_constructor: Tgravity_class_lookup_constructor;
    Fgravity_class_new_pair: Tgravity_class_new_pair;
    Fgravity_class_new_single: Tgravity_class_new_single;
    Fgravity_class_serialize: Tgravity_class_serialize;
    Fgravity_class_setsuper: Tgravity_class_setsuper;
    Fgravity_class_setxdata: Tgravity_class_setxdata;
    Fgravity_class_size: Tgravity_class_size;
    Fgravity_fiber_blacken: Tgravity_fiber_blacken;
    Fgravity_fiber_free: Tgravity_fiber_free;
    Fgravity_fiber_new: Tgravity_fiber_new;
    Fgravity_fiber_reassign: Tgravity_fiber_reassign;
    Fgravity_fiber_seterror: Tgravity_fiber_seterror;
    Fgravity_fiber_size: Tgravity_fiber_size;
    Fgravity_instance_blacken: Tgravity_instance_blacken;
    Fgravity_instance_clone: Tgravity_instance_clone;
    Fgravity_instance_free: Tgravity_instance_free;
    Fgravity_instance_lookup_event: Tgravity_instance_lookup_event;
    Fgravity_instance_new: Tgravity_instance_new;
    Fgravity_instance_setivar: Tgravity_instance_setivar;
    Fgravity_instance_setxdata: Tgravity_instance_setxdata;
    Fgravity_instance_size: Tgravity_instance_size;
    Fgravity_value_dump: Tgravity_value_dump;
    Fgravity_value_equals: Tgravity_value_equals;
    Fgravity_value_free: Tgravity_value_free;
    Fgravity_value_getclass: Tgravity_value_getclass;
    Fgravity_value_getsuper: Tgravity_value_getsuper;
    Fgravity_value_hash: Tgravity_value_hash;
    Fgravity_value_isobject: Tgravity_value_isobject;
    Fgravity_value_serialize: Tgravity_value_serialize;
    Fgravity_value_size: Tgravity_value_size;
    Fgravity_value_xdata: Tgravity_value_xdata;
    Fgravity_value_from_bool: Tgravity_value_from_bool;
    Fgravity_value_from_error: Tgravity_value_from_error;
    Fgravity_value_from_float: Tgravity_value_from_float;
    Fgravity_value_from_int: Tgravity_value_from_int;
    Fgravity_value_from_null: Tgravity_value_from_null;
    Fgravity_value_from_object: Tgravity_value_from_object;
    Fgravity_value_from_undefined: Tgravity_value_from_undefined;
    Fgravity_object_blacken: Tgravity_object_blacken;
    Fgravity_object_debug: Tgravity_object_debug;
    Fgravity_object_deserialize: Tgravity_object_deserialize;
    Fgravity_object_free: Tgravity_object_free;
    Fgravity_object_serialize: Tgravity_object_serialize;
    Fgravity_object_size: Tgravity_object_size;
    Fgravity_list_append_list: Tgravity_list_append_list;
    Fgravity_list_blacken: Tgravity_list_blacken;
    Fgravity_list_free: Tgravity_list_free;
    Fgravity_list_from_array: Tgravity_list_from_array;
    Fgravity_list_new: Tgravity_list_new;
    Fgravity_list_size: Tgravity_list_size;
    Fgravity_map_blacken: Tgravity_map_blacken;
    Fgravity_map_append_map: Tgravity_map_append_map;
    Fgravity_map_free: Tgravity_map_free;
    Fgravity_map_insert: Tgravity_map_insert;
    Fgravity_map_new: Tgravity_map_new;
    Fgravity_map_size: Tgravity_map_size;
    Fgravity_range_blacken: Tgravity_range_blacken;
    Fgravity_range_free: Tgravity_range_free;
    Fgravity_range_new: Tgravity_range_new;
    Fgravity_range_size: Tgravity_range_size;
    Fgravity_string_blacken: Tgravity_string_blacken;
    Fgravity_string_free: Tgravity_string_free;
    Fgravity_string_new: Tgravity_string_new;
    Fgravity_string_set: Tgravity_string_set;
    Fgravity_string_size: Tgravity_string_size;
    Fgravity_string_to_value: Tgravity_string_to_value;
    Fgravity_compiler_create: Tgravity_compiler_create;
    Fgravity_compiler_run: Tgravity_compiler_run;
    Fgravity_compiler_ast: Tgravity_compiler_ast;
    Fgravity_compiler_free: Tgravity_compiler_free;
    Fgravity_compiler_serialize: Tgravity_compiler_serialize;
    Fgravity_compiler_serialize_infile: Tgravity_compiler_serialize_infile;
    Fgravity_compiler_transfer: Tgravity_compiler_transfer;
    Fgravity_vm_delegate: Tgravity_vm_delegate;
    Fgravity_vm_fiber: Tgravity_vm_fiber;
    Fgravity_vm_free: Tgravity_vm_free;
    Fgravity_vm_getvalue: Tgravity_vm_getvalue;
    Fgravity_vm_keyindex: Tgravity_vm_keyindex;
    Fgravity_vm_ismini: Tgravity_vm_ismini;
    Fgravity_vm_isaborted: Tgravity_vm_isaborted;
    Fgravity_vm_loadclosure: Tgravity_vm_loadclosure;
    Fgravity_vm_lookup: Tgravity_vm_lookup;
    Fgravity_vm_new: Tgravity_vm_new;
    Fgravity_vm_newmini: Tgravity_vm_newmini;
    Fgravity_vm_result: Tgravity_vm_result;
    Fgravity_vm_runclosure: Tgravity_vm_runclosure;
    Fgravity_vm_runmain: Tgravity_vm_runmain;
    Fgravity_vm_set_callbacks: Tgravity_vm_set_callbacks;
    Fgravity_vm_setaborted: Tgravity_vm_setaborted;
    Fgravity_vm_seterror: Tgravity_vm_seterror;
    Fgravity_vm_seterror_string: Tgravity_vm_seterror_string;
    Fgravity_vm_setfiber: Tgravity_vm_setfiber;
    Fgravity_vm_setvalue: Tgravity_vm_setvalue;
    Fgravity_vm_time: Tgravity_vm_time;
    Fgravity_vm_cleanup: Tgravity_vm_cleanup;
    Fgravity_vm_filter: Tgravity_vm_filter;
    Fgravity_vm_transfer: Tgravity_vm_transfer;
    Fgravity_vm_initmodule: Tgravity_vm_initmodule;
    Fgravity_vm_loadbuffer: Tgravity_vm_loadbuffer;
    Fgravity_vm_loadfile: Tgravity_vm_loadfile;
    Fgravity_vm_fastlookup: Tgravity_vm_fastlookup;
    Fgravity_vm_getdata: Tgravity_vm_getdata;
    Fgravity_vm_getslot: Tgravity_vm_getslot;
    Fgravity_vm_setdata: Tgravity_vm_setdata;
    Fgravity_vm_setslot: Tgravity_vm_setslot;
    Fgravity_vm_memupdate: Tgravity_vm_memupdate;
    Fgravity_vm_anonymous: Tgravity_vm_anonymous;
    Fgravity_vm_get: Tgravity_vm_get;
    Fgravity_vm_set: Tgravity_vm_set;
    Fgravity_gray_object: Tgravity_gray_object;
    Fgravity_gray_value: Tgravity_gray_value;
    Fgravity_gc_setenabled: Tgravity_gc_setenabled;
    Fgravity_gc_start: Tgravity_gc_start;
    Fgravity_gc_temppop: Tgravity_gc_temppop;
    Fgravity_gc_temppush: Tgravity_gc_temppush;
    Fgravity_core_class_from_name: Tgravity_core_class_from_name;
    Fgravity_core_free: Tgravity_core_free;
    Fgravity_core_identifiers: Tgravity_core_identifiers;
    Fgravity_core_register: Tgravity_core_register;
    Fgravity_iscore_class: Tgravity_iscore_class;
    Fconvert_value2bool: Tconvert_value2bool;
    Fconvert_value2float: Tconvert_value2float;
    Fconvert_value2int: Tconvert_value2int;
    Fconvert_value2string: Tconvert_value2string;
    Fcomputed_property_create: Tcomputed_property_create;
    Fcomputed_property_free: Tcomputed_property_free;
    Fgravity_hash_create: Tgravity_hash_create;
    Fgravity_hash_free: Tgravity_hash_free;
    Fgravity_hash_insert: Tgravity_hash_insert;
    Fgravity_hash_isempty: Tgravity_hash_isempty;
    Fgravity_hash_lookup: Tgravity_hash_lookup;
    Fgravity_hash_remove: Tgravity_hash_remove;
    Fgravity_hash_append: Tgravity_hash_append;
    Fgravity_hash_compute_buffer: Tgravity_hash_compute_buffer;
    Fgravity_hash_compute_float: Tgravity_hash_compute_float;
    Fgravity_hash_compute_int: Tgravity_hash_compute_int;
    Fgravity_hash_count: Tgravity_hash_count;
    Fgravity_hash_dump: Tgravity_hash_dump;
    Fgravity_hash_iterate: Tgravity_hash_iterate;
    Fgravity_hash_iterate2: Tgravity_hash_iterate2;
    Fgravity_hash_memsize: Tgravity_hash_memsize;
    Fgravity_hash_resetfree: Tgravity_hash_resetfree;
    Fgravity_hash_stat: Tgravity_hash_stat;
    Fgravity_hash_transform: Tgravity_hash_transform;
    Fgravity_hash_keyfree: Tgravity_hash_keyfree;
    Fgravity_hash_keyvaluefree: Tgravity_hash_keyvaluefree;
    Fgravity_hash_valuefree: Tgravity_hash_valuefree;

    function GetClassClosure: Pgravity_class_t;
    function GetClassBool: Pgravity_class_t;
    function GetClassClass: Pgravity_class_t;
    function GetClassFiber: Pgravity_class_t;
    function GetClassFloat: Pgravity_class_t;
    function GetClassFunction: Pgravity_class_t;
    function GetClassInstance: Pgravity_class_t;
    function GetClassInt: Pgravity_class_t;
    function GetClassList: Pgravity_class_t;
    function GetClassMap: Pgravity_class_t;
    function GetClassNull: Pgravity_class_t;
    function GetClassObject: Pgravity_class_t;
    function GetClassRange: Pgravity_class_t;
    function GetClassString: Pgravity_class_t;
    function GetClassSystem: Pgravity_class_t;
    function GetClassUpvalue: Pgravity_class_t;

    procedure ResetClassReferences;
  public
    class function DefaultInstance: IGVInterface;

    constructor Create;
    function LibraryName: String; virtual;

    procedure LoadGVLibrary;
    procedure FreeGVLibrary;
    function TryLoadLibrary: Boolean;
    procedure CheckLibraryLoaded;

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

    function VALUE_AS_OBJECT(val: gravity_value_t): Pgravity_object_t;
    function VALUE_AS_STRING(val: gravity_value_t): Pgravity_string_t;
    function VALUE_AS_FIBER(val: gravity_value_t): Pgravity_fiber_t;
    function VALUE_AS_FUNCTION(val: gravity_value_t): Pgravity_function_t;
    function VALUE_AS_CLOSURE(val: gravity_value_t): Pgravity_closure_t;
    function VALUE_AS_CLASS(val: gravity_value_t): Pgravity_class_t;
    function VALUE_AS_INSTANCE(val: gravity_value_t): Pgravity_instance_t;
    function VALUE_AS_LIST(val: gravity_value_t): Pgravity_list_t;
    function VALUE_AS_MAP(val: gravity_value_t): Pgravity_map_t;
    function VALUE_AS_RANGE(val: gravity_value_t): Pgravity_range_t;
    function VALUE_AS_ERROR(val: gravity_value_t): PAnsiChar;
    function VALUE_AS_FLOAT(val: gravity_value_t): gravity_float_t;
    function VALUE_AS_INT(val: gravity_value_t): gravity_int_t;
    function VALUE_AS_BOOL(val: gravity_value_t): gravity_int_t;
    function VALUE_AS_CSTRING(val: gravity_value_t): PAnsiChar;

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

    function OBJECT_ISA_FUNCTION(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_INSTANCE(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_CLOSURE(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_FIBER(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_CLASS(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_STRING(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_INT(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_FLOAT(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_BOOL(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_LIST(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_MAP(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_RANGE(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_NULL(v: Pgravity_object_t): Boolean;
    function OBJECT_IS_VALID(v: Pgravity_object_t): Boolean;
    function OBJECT_ISA_UPVALUE(v: Pgravity_object_t): Boolean;

    function NEW_FUNCTION(fptr: gravity_c_internal): Pgravity_function_t;
    function NEW_FUNCTION_BRIDGED(identifier: AnsiString; xdata: pointer): Pgravity_function_t;
    function NEW_FUNCTION_SPECIAL(identifier: AnsiString; index: UInt16; getter, setter: pointer): Pgravity_function_t;

    function NEW_CLOSURE_VALUE(fptr: gravity_c_internal): gravity_value_t;
    function NEW_CLOSURE_VALUE_BRIDGED(identifier: AnsiString; xdata: pointer): gravity_value_t;
    function NEW_CLOSURE_VALUE_SPECIAL(identifier: AnsiString;
      index: Uint16; getter, setter: pointer): gravity_value_t;
    function GET_VALUE(args: Pgravity_value_t; ndx: UInt16): gravity_value_t;
    procedure SETMETA_INITED(c: Pgravity_class_t);
    function RETURN_VALUE(vm: Pgravity_vm; v: gravity_value_t; index: UInt32): Boolean;
    function RETURN_CLOSURE(vm: Pgravity_vm; v: gravity_value_t; index: UInt32): Boolean;
    function RETURN_FIBER: Boolean;
    function RETURN_NOVALUE: Boolean;

    property gravity_class_int: Pgravity_class_t read GetClassInt;
    property gravity_class_float: Pgravity_class_t read GetClassFloat;
    property gravity_class_bool: Pgravity_class_t read GetClassBool;
    property gravity_class_null: Pgravity_class_t read GetClassNull;

    property gravity_class_string: Pgravity_class_t read GetClassString;
    property gravity_class_object: Pgravity_class_t read GetClassObject;
    property gravity_class_function: Pgravity_class_t read GetClassFunction;
    property gravity_class_closure: Pgravity_class_t read GetClassClosure;
    property gravity_class_fiber: Pgravity_class_t read GetClassFiber;
    property gravity_class_class: Pgravity_class_t read GetClassClass;
    property gravity_class_instance: Pgravity_class_t read GetClassInstance;
    property gravity_class_list: Pgravity_class_t read GetClassList;
    property gravity_class_map: Pgravity_class_t read GetClassMap;
    property gravity_class_range: Pgravity_class_t read GetClassRange;
    property gravity_class_upvalue: Pgravity_class_t read GetClassUpvalue;
    property gravity_class_system: Pgravity_class_t read GetClassSystem;
  end;

{$IFDEF GRAVITY_STATIC_LINK}

function _gravity_module_new(vm: Pgravity_vm; const identifier: PAnsiChar)
  : Pgravity_module_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_module_new';
procedure _gravity_module_free(vm: Pgravity_vm; m: Pgravity_module_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_module_free';
procedure _gravity_module_blacken(vm: Pgravity_vm; m: Pgravity_module_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_module_blacken';
function _gravity_module_size(vm: Pgravity_vm; m: Pgravity_module_t): UInt32;
  cdecl; external LIBNAME_DLL name _PU + 'gravity_module_size';

function _gravity_bytecode_deserialize(const buffer: PAnsiChar; len: NativeUInt;
  ninst: PUInt32): PUInt32; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_bytecode_deserialize';

procedure _gravity_function_blacken(vm: Pgravity_vm; f: Pgravity_function_t);
  cdecl; external LIBNAME_DLL name _PU + 'gravity_function_blacken';
function _gravity_function_cpool_add(vm: Pgravity_vm; f: Pgravity_function_t;
  v: gravity_value_t): UInt16; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_function_cpool_add';
function _gravity_function_cpool_get(f: Pgravity_function_t; i: UInt16)
  : gravity_value_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_function_cpool_get';
function _gravity_function_deserialize(vm: Pgravity_vm; json: Pjson_value)
  : Pgravity_function_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_function_deserialize';
procedure _gravity_function_dump(f: Pgravity_function_t;
  codef: code_dump_function); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_function_dump';
procedure _gravity_function_free(vm: Pgravity_vm; f: Pgravity_function_t);
  cdecl; external LIBNAME_DLL name _PU + 'gravity_function_free';
function _gravity_function_new(vm: Pgravity_vm; const identifier: PAnsiChar;
  nparams: UInt16; nlocals: UInt16; ntemps: UInt16; code: Pointer)
  : Pgravity_function_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_function_new';
function _gravity_function_new_bridged(vm: Pgravity_vm;
  const identifier: PAnsiChar; xdata: Pointer): Pgravity_function_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_function_new_bridged';
function _gravity_function_new_internal(vm: Pgravity_vm;
  const identifier: PAnsiChar; exec: gravity_c_internal; nparams: UInt16)
  : Pgravity_function_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_function_new_internal';
function _gravity_function_new_special(vm: Pgravity_vm;
  const identifier: PAnsiChar; index: UInt16; getter: Pointer; setter: Pointer)
  : Pgravity_function_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_function_new_special';
procedure _gravity_function_serialize(f: Pgravity_function_t; json: Pjson_t);
  cdecl; external LIBNAME_DLL name _PU + 'gravity_function_serialize';
procedure _gravity_function_setxdata(f: Pgravity_function_t; xdata: Pointer);
  cdecl; external LIBNAME_DLL name _PU + 'gravity_function_setxdata';
function _gravity_function_size(vm: Pgravity_vm; f: Pgravity_function_t)
  : UInt32; cdecl; external LIBNAME_DLL name _PU + 'gravity_function_size';

procedure _gravity_closure_blacken(vm: Pgravity_vm;
  closure: Pgravity_closure_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_closure_blacken';
procedure _gravity_closure_dec_refcount(vm: Pgravity_vm;
  closure: Pgravity_closure_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_closure_dec_refcount';
procedure _gravity_closure_inc_refcount(vm: Pgravity_vm;
  closure: Pgravity_closure_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_closure_inc_refcount';
procedure _gravity_closure_free(vm: Pgravity_vm; closure: Pgravity_closure_t);
  cdecl; external LIBNAME_DLL name _PU + 'gravity_closure_free';
function _gravity_closure_size(vm: Pgravity_vm; closure: Pgravity_closure_t)
  : UInt32; cdecl; external LIBNAME_DLL name _PU + 'gravity_closure_size';
function _gravity_closure_new(vm: Pgravity_vm; f: Pgravity_function_t)
  : Pgravity_closure_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_closure_new';

procedure _gravity_upvalue_blacken(vm: Pgravity_vm;
  upvalue: Pgravity_upvalue_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_upvalue_blacken';
procedure _gravity_upvalue_free(vm: Pgravity_vm; upvalue: Pgravity_upvalue_t);
  cdecl; external LIBNAME_DLL name _PU + 'gravity_upvalue_free';
function _gravity_upvalue_new(vm: Pgravity_vm; value: Pgravity_value_t)
  : Pgravity_upvalue_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_upvalue_new';
function _gravity_upvalue_size(vm: Pgravity_vm; upvalue: Pgravity_upvalue_t)
  : UInt32; cdecl; external LIBNAME_DLL name _PU + 'gravity_upvalue_size';

procedure _gravity_class_blacken(vm: Pgravity_vm; c: Pgravity_class_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_class_blacken';
function _gravity_class_add_ivar(c: Pgravity_class_t;
  const identifier: PAnsiChar): Int16; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_class_add_ivar';
procedure _gravity_class_bind(c: Pgravity_class_t; const key: PAnsiChar;
  value: gravity_value_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_class_bind';
function _gravity_class_count_ivars(c: Pgravity_class_t): UInt32; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_class_count_ivars';
function _gravity_class_deserialize(vm: Pgravity_vm; json: Pjson_value)
  : Pgravity_class_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_class_deserialize';
procedure _gravity_class_dump(c: Pgravity_class_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_class_dump';
procedure _gravity_class_free(vm: Pgravity_vm; c: Pgravity_class_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_class_free';
procedure _gravity_class_free_core(vm: Pgravity_vm; c: Pgravity_class_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_class_free_core';
function _gravity_class_get_meta(c: Pgravity_class_t): Pgravity_class_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_class_get_meta';
function _gravity_class_getsuper(c: Pgravity_class_t): Pgravity_class_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_class_getsuper';
function _gravity_class_grow(c: Pgravity_class_t; n: UInt32): Boolean; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_class_grow';
function _gravity_class_is_meta(c: Pgravity_class_t): Boolean; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_class_is_meta';
function _gravity_class_lookup(c: Pgravity_class_t; key: gravity_value_t)
  : Pgravity_object_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_class_lookup';
function _gravity_class_lookup_closure(c: Pgravity_class_t;
  key: gravity_value_t): Pgravity_closure_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_class_lookup_closure';
function _gravity_class_lookup_constructor(c: Pgravity_class_t; nparams: UInt32)
  : Pgravity_closure_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_class_lookup_constructor';
function _gravity_class_new_pair(vm: Pgravity_vm; const identifier: PAnsiChar;
  superclass: Pgravity_class_t; nivar: UInt32; nsvar: UInt32): Pgravity_class_t;
  cdecl; external LIBNAME_DLL name _PU + 'gravity_class_new_pair';
function _gravity_class_new_single(vm: Pgravity_vm; const identifier: PAnsiChar;
  nfields: UInt32): Pgravity_class_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_class_new_single';
procedure _gravity_class_serialize(c: Pgravity_class_t; json: Pjson_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_class_serialize';
function _gravity_class_setsuper(subclass: Pgravity_class_t;
  superclass: Pgravity_class_t): Boolean; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_class_setsuper';
procedure _gravity_class_setxdata(c: Pgravity_class_t; xdata: Pointer); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_class_setxdata';
function _gravity_class_size(vm: Pgravity_vm; c: Pgravity_class_t): UInt32;
  cdecl; external LIBNAME_DLL name _PU + 'gravity_class_size';

procedure _gravity_fiber_blacken(vm: Pgravity_vm; fiber: Pgravity_fiber_t);
  cdecl; external LIBNAME_DLL name _PU + 'gravity_fiber_blacken';
procedure _gravity_fiber_free(vm: Pgravity_vm; fiber: Pgravity_fiber_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_fiber_free';
function _gravity_fiber_new(vm: Pgravity_vm; closure: Pgravity_closure_t;
  nstack: UInt32; nframes: UInt32): Pgravity_fiber_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_fiber_new';
procedure _gravity_fiber_reassign(fiber: Pgravity_fiber_t;
  closure: Pgravity_closure_t; nargs: UInt16); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_fiber_reassign';
procedure _gravity_fiber_seterror(fiber: Pgravity_fiber_t;
  const error: PAnsiChar); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_fiber_seterror';
function _gravity_fiber_size(vm: Pgravity_vm; fiber: Pgravity_fiber_t): UInt32;
  cdecl; external LIBNAME_DLL name _PU + 'gravity_fiber_size';

procedure _gravity_instance_blacken(vm: Pgravity_vm; i: Pgravity_instance_t);
  cdecl; external LIBNAME_DLL name _PU + 'gravity_instance_blacken';
function _gravity_instance_clone(vm: Pgravity_vm;
  src_instance: Pgravity_instance_t): Pgravity_instance_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_instance_clone';
procedure _gravity_instance_free(vm: Pgravity_vm; i: Pgravity_instance_t);
  cdecl; external LIBNAME_DLL name _PU + 'gravity_instance_free';
function _gravity_instance_lookup_event(i: Pgravity_instance_t;
  const name: PAnsiChar): Pgravity_closure_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_instance_lookup_event';
function _gravity_instance_new(vm: Pgravity_vm; c: Pgravity_class_t)
  : Pgravity_instance_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_instance_new';
procedure _gravity_instance_setivar(instance: Pgravity_instance_t; idx: UInt32;
  value: gravity_value_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_instance_setivar';
procedure _gravity_instance_setxdata(i: Pgravity_instance_t; xdata: Pointer);
  cdecl; external LIBNAME_DLL name _PU + 'gravity_instance_setxdata';
function _gravity_instance_size(vm: Pgravity_vm; i: Pgravity_instance_t)
  : UInt32; cdecl; external LIBNAME_DLL name _PU + 'gravity_instance_size';

procedure _gravity_value_dump(vm: Pgravity_vm; v: gravity_value_t;
  buffer: PAnsiChar; len: UInt16); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_value_dump';
function _gravity_value_equals(v1: gravity_value_t; v2: gravity_value_t)
  : Boolean; cdecl; external LIBNAME_DLL name _PU + 'gravity_value_equals';
procedure _gravity_value_free(vm: Pgravity_vm; v: gravity_value_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_value_free';
function _gravity_value_getclass(v: gravity_value_t): Pgravity_class_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_value_getclass';
function _gravity_value_getsuper(v: gravity_value_t): Pgravity_class_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_value_getsuper';
function _gravity_value_hash(value: gravity_value_t): UInt32; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_value_hash';
function _gravity_value_isobject(v: gravity_value_t): Boolean; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_value_isobject';
procedure _gravity_value_serialize(const key: PAnsiChar; v: gravity_value_t;
  json: Pjson_t); cdecl; external LIBNAME_DLL name _PU +
  'gravity_value_serialize';
function _gravity_value_size(vm: Pgravity_vm; v: gravity_value_t): UInt32;
  cdecl; external LIBNAME_DLL name _PU + 'gravity_value_size';
function _gravity_value_xdata(value: gravity_value_t): Pointer; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_value_xdata';

function _gravity_value_from_bool(b: Boolean): gravity_value_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_value_from_bool';
function _gravity_value_from_error(const msg: PAnsiChar): gravity_value_t;
  cdecl; external LIBNAME_DLL name _PU + 'gravity_value_from_error';
function _gravity_value_from_float(f: gravity_float_t): gravity_value_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_value_from_float';
function _gravity_value_from_int(n: gravity_int_t): gravity_value_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_value_from_int';
function _gravity_value_from_null(): gravity_value_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_value_from_null';
function _gravity_value_from_object(obj: Pointer): gravity_value_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_value_from_object';
function _gravity_value_from_undefined(): gravity_value_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_value_from_undefined';

procedure _gravity_object_blacken(vm: Pgravity_vm; obj: Pgravity_object_t);
  cdecl; external LIBNAME_DLL name _PU + 'gravity_object_blacken';
function _gravity_object_debug(obj: Pgravity_object_t; is_free: Boolean)
  : PAnsiChar; cdecl; external LIBNAME_DLL name _PU + 'gravity_object_debug';
function _gravity_object_deserialize(vm: Pgravity_vm; entry: Pjson_value)
  : Pgravity_object_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_object_deserialize';
procedure _gravity_object_free(vm: Pgravity_vm; obj: Pgravity_object_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_object_free';
procedure _gravity_object_serialize(obj: Pgravity_object_t; json: Pjson_t);
  cdecl; external LIBNAME_DLL name _PU + 'gravity_object_serialize';
function _gravity_object_size(vm: Pgravity_vm; obj: Pgravity_object_t): UInt32;
  cdecl; external LIBNAME_DLL name _PU + 'gravity_object_size';

procedure _gravity_list_append_list(vm: Pgravity_vm; list1: Pgravity_list_t;
  list2: Pgravity_list_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_list_append_list';
procedure _gravity_list_blacken(vm: Pgravity_vm; list: Pgravity_list_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_list_blacken';
procedure _gravity_list_free(vm: Pgravity_vm; list: Pgravity_list_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_list_free';
function _gravity_list_from_array(vm: Pgravity_vm; n: UInt32;
  p: Pgravity_value_t): Pgravity_list_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_list_from_array';
function _gravity_list_new(vm: Pgravity_vm; n: UInt32): Pgravity_list_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_list_new';
function _gravity_list_size(vm: Pgravity_vm; list: Pgravity_list_t): UInt32;
  cdecl; external LIBNAME_DLL name _PU + 'gravity_list_size';

procedure _gravity_map_blacken(vm: Pgravity_vm; map: Pgravity_map_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_map_blacken';
procedure _gravity_map_append_map(vm: Pgravity_vm; map1: Pgravity_map_t;
  map2: Pgravity_map_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_map_append_map';
procedure _gravity_map_free(vm: Pgravity_vm; map: Pgravity_map_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_map_free';
procedure _gravity_map_insert(vm: Pgravity_vm; map: Pgravity_map_t;
  key: gravity_value_t; value: gravity_value_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_map_insert';
function _gravity_map_new(vm: Pgravity_vm; n: UInt32): Pgravity_map_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_map_new';
function _gravity_map_size(vm: Pgravity_vm; map: Pgravity_map_t): UInt32; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_map_size';

procedure _gravity_range_blacken(vm: Pgravity_vm; range: Pgravity_range_t);
  cdecl; external LIBNAME_DLL name _PU + 'gravity_range_blacken';
procedure _gravity_range_free(vm: Pgravity_vm; range: Pgravity_range_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_range_free';
function _gravity_range_new(vm: Pgravity_vm; from: gravity_int_t;
  _to: gravity_int_t; inclusive: Boolean): Pgravity_range_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_range_new';
function _gravity_range_size(vm: Pgravity_vm; range: Pgravity_range_t): UInt32;
  cdecl; external LIBNAME_DLL name _PU + 'gravity_range_size';

/// MARK: - STRING -
procedure _gravity_string_blacken(vm: Pgravity_vm; _string: Pgravity_string_t);
  cdecl; external LIBNAME_DLL name _PU + 'gravity_string_blacken';
procedure _gravity_string_free(vm: Pgravity_vm; value: Pgravity_string_t);
  cdecl; external LIBNAME_DLL name _PU + 'gravity_string_free';
function _gravity_string_new(vm: Pgravity_vm; s: PAnsiChar; len: UInt32;
  alloc: UInt32): Pgravity_string_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_string_new';
procedure _gravity_string_set(obj: Pgravity_string_t; s: PAnsiChar;
  len: UInt32); cdecl; external LIBNAME_DLL name _PU + 'gravity_string_set';
function _gravity_string_size(vm: Pgravity_vm; _string: Pgravity_string_t)
  : UInt32; cdecl; external LIBNAME_DLL name _PU + 'gravity_string_size';
function _gravity_string_to_value(vm: Pgravity_vm; const s: PAnsiChar;
  len: UInt32): gravity_value_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_string_to_value';

function _gravity_compiler_create(delegate: Pgravity_delegate_t)
  : Pgravity_compiler_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_compiler_create';
function _gravity_compiler_run(compiler: Pgravity_compiler_t;
  const source: PAnsiChar; len: NativeUInt; fileid: UInt32; is_static: Boolean;
  add_debug: Boolean): Pgravity_closure_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_compiler_run';
function _gravity_compiler_ast(compiler: Pgravity_compiler_t): Pgnode_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_compiler_ast';
procedure _gravity_compiler_free(compiler: Pgravity_compiler_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_compiler_free';
function _gravity_compiler_serialize(compiler: Pgravity_compiler_t;
  closure: Pgravity_closure_t): Pjson_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_compiler_serialize';
function _gravity_compiler_serialize_infile(compiler: Pgravity_compiler_t;
  closure: Pgravity_closure_t; const path: PAnsiChar): Boolean; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_compiler_serialize_infile';
procedure _gravity_compiler_transfer(compiler: Pgravity_compiler_t;
  vm: Pgravity_vm); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_compiler_transfer';

function _gravity_vm_delegate(vm: Pgravity_vm): Pgravity_delegate_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_delegate';
function _gravity_vm_fiber(vm: Pgravity_vm): Pgravity_fiber_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_fiber';
procedure _gravity_vm_free(vm: Pgravity_vm); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_free';
function _gravity_vm_getvalue(vm: Pgravity_vm; const key: PAnsiChar;
  keylen: UInt32): gravity_value_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_getvalue';
function _gravity_vm_keyindex(vm: Pgravity_vm; index: UInt32): gravity_value_t;
  cdecl; external LIBNAME_DLL name _PU + 'gravity_vm_keyindex';
function _gravity_vm_ismini(vm: Pgravity_vm): Boolean; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_ismini';
function _gravity_vm_isaborted(vm: Pgravity_vm): Boolean; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_isaborted';
procedure _gravity_vm_loadclosure(vm: Pgravity_vm; closure: Pgravity_closure_t);
  cdecl; external LIBNAME_DLL name _PU + 'gravity_vm_loadclosure';
function _gravity_vm_lookup(vm: Pgravity_vm; key: gravity_value_t)
  : gravity_value_t; cdecl; external LIBNAME_DLL name _PU + 'gravity_vm_lookup';
function _gravity_vm_new(delegate: Pgravity_delegate_t): Pgravity_vm; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_new';
function _gravity_vm_newmini(): Pgravity_vm; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_newmini';
function _gravity_vm_result(vm: Pgravity_vm): gravity_value_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_result';
function _gravity_vm_runclosure(vm: Pgravity_vm; closure: Pgravity_closure_t;
  sender: gravity_value_t; params: Pgravity_value_t; nparams: UInt16): Boolean;
  cdecl; external LIBNAME_DLL name _PU + 'gravity_vm_runclosure';
function _gravity_vm_runmain(vm: Pgravity_vm; closure: Pgravity_closure_t)
  : Boolean; cdecl; external LIBNAME_DLL name _PU + 'gravity_vm_runmain';
procedure _gravity_vm_set_callbacks(vm: Pgravity_vm;
  vm_transfer: vm_transfer_cb; vm_cleanup: vm_cleanup_cb); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_set_callbacks';
procedure _gravity_vm_setaborted(vm: Pgravity_vm); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_setaborted';
procedure _gravity_vm_seterror(vm: Pgravity_vm; const format: PAnsiChar)varargs;
  cdecl; external LIBNAME_DLL name _PU + 'gravity_vm_seterror';
procedure _gravity_vm_seterror_string(vm: Pgravity_vm; const s: PAnsiChar);
  cdecl; external LIBNAME_DLL name _PU + 'gravity_vm_seterror_string';
procedure _gravity_vm_setfiber(vm: Pgravity_vm; fiber: Pgravity_fiber_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_setfiber';
procedure _gravity_vm_setvalue(vm: Pgravity_vm; const key: PAnsiChar;
  value: gravity_value_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_setvalue';
function _gravity_vm_time(vm: Pgravity_vm): Double; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_time';

procedure _gravity_vm_cleanup(vm: Pgravity_vm); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_cleanup';
procedure _gravity_vm_filter(vm: Pgravity_vm; cleanup_filter: vm_filter_cb);
  cdecl; external LIBNAME_DLL name _PU + 'gravity_vm_filter';
procedure _gravity_vm_transfer(vm: Pgravity_vm; obj: Pgravity_object_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_transfer';
procedure _gravity_vm_initmodule(vm: Pgravity_vm; f: Pgravity_function_t);
  cdecl; external LIBNAME_DLL name _PU + 'gravity_vm_initmodule';
function _gravity_vm_loadbuffer(vm: Pgravity_vm; const buffer: PAnsiChar;
  len: NativeUInt): Pgravity_closure_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_loadbuffer';
function _gravity_vm_loadfile(vm: Pgravity_vm; const path: PAnsiChar)
  : Pgravity_closure_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_loadfile';
function _gravity_vm_fastlookup(vm: Pgravity_vm; c: Pgravity_class_t;
  index: Integer): Pgravity_closure_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_fastlookup';
function _gravity_vm_getdata(vm: Pgravity_vm): Pointer; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_getdata';
function _gravity_vm_getslot(vm: Pgravity_vm; index: UInt32): gravity_value_t;
  cdecl; external LIBNAME_DLL name _PU + 'gravity_vm_getslot';
procedure _gravity_vm_setdata(vm: Pgravity_vm; data: Pointer); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_setdata';
procedure _gravity_vm_setslot(vm: Pgravity_vm; value: gravity_value_t;
  index: UInt32); cdecl; external LIBNAME_DLL name _PU + 'gravity_vm_setslot';
procedure _gravity_vm_memupdate(vm: Pgravity_vm; value: gravity_int_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_memupdate';
function _gravity_vm_anonymous(vm: Pgravity_vm): PAnsiChar; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_anonymous';
function _gravity_vm_get(vm: Pgravity_vm; const key: PAnsiChar)
  : gravity_value_t; cdecl; external LIBNAME_DLL name _PU + 'gravity_vm_get';
function _gravity_vm_set(vm: Pgravity_vm; const key: PAnsiChar;
  value: gravity_value_t): Boolean; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_vm_set';

procedure _gravity_gray_object(vm: Pgravity_vm; obj: Pgravity_object_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_gray_object';
procedure _gravity_gray_value(vm: Pgravity_vm; v: gravity_value_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_gray_value';

procedure _gravity_gc_setenabled(vm: Pgravity_vm; enabled: Boolean); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_gc_setenabled';
procedure _gravity_gc_start(vm: Pgravity_vm); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_gc_start';
procedure _gravity_gc_temppop(vm: Pgravity_vm); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_gc_temppop';
procedure _gravity_gc_temppush(vm: Pgravity_vm; obj: Pgravity_object_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_gc_temppush';
function _gravity_core_class_from_name(const name: PAnsiChar): Pgravity_class_t;
  cdecl; external LIBNAME_DLL name _PU + 'gravity_core_class_from_name';
procedure _gravity_core_free(); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_core_free';
function _gravity_core_identifiers(): PPAnsiChar; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_core_identifiers';
procedure _gravity_core_register(vm: Pgravity_vm); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_core_register';
function _gravity_iscore_class(c: Pgravity_class_t): Boolean; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_iscore_class';

function _convert_value2bool(vm: Pgravity_vm; v: gravity_value_t)
  : gravity_value_t; cdecl;
  external LIBNAME_DLL name _PU + 'convert_value2bool';
function _convert_value2float(vm: Pgravity_vm; v: gravity_value_t)
  : gravity_value_t; cdecl;
  external LIBNAME_DLL name _PU + 'convert_value2float';
function _convert_value2int(vm: Pgravity_vm; v: gravity_value_t)
  : gravity_value_t; cdecl; external LIBNAME_DLL name _PU + 'convert_value2int';
function _convert_value2string(vm: Pgravity_vm; v: gravity_value_t)
  : gravity_value_t; cdecl;
  external LIBNAME_DLL name _PU + 'convert_value2string';

function _computed_property_create(vm: Pgravity_vm;
  getter_func: Pgravity_function_t; setter_func: Pgravity_function_t)
  : Pgravity_closure_t; cdecl;
  external LIBNAME_DLL name _PU + 'computed_property_create';
procedure _computed_property_free(c: Pgravity_class_t; const name: PAnsiChar;
  remove_flag: Boolean); cdecl;
  external LIBNAME_DLL name _PU + 'computed_property_free';

function _gravity_hash_create(size: UInt32; compute: gravity_hash_compute_fn;
  isequal: gravity_hash_isequal_fn; free: gravity_hash_iterate_fn;
  data: Pointer): Pgravity_hash_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_hash_create';
procedure _gravity_hash_free(hashtable: Pgravity_hash_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_hash_free';
function _gravity_hash_insert(hashtable: Pgravity_hash_t; key: gravity_value_t;
  value: gravity_value_t): Boolean; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_hash_insert';
function _gravity_hash_isempty(hashtable: Pgravity_hash_t): Boolean; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_hash_isempty';
function _gravity_hash_lookup(hashtable: Pgravity_hash_t; key: gravity_value_t)
  : Pgravity_value_t; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_hash_lookup';
function _gravity_hash_remove(hashtable: Pgravity_hash_t; key: gravity_value_t)
  : Boolean; cdecl; external LIBNAME_DLL name _PU + 'gravity_hash_remove';
procedure _gravity_hash_append(hashtable1: Pgravity_hash_t;
  hashtable2: Pgravity_hash_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_hash_append';
function _gravity_hash_compute_buffer(const key: PAnsiChar; len: UInt32)
  : UInt32; cdecl; external LIBNAME_DLL name _PU +
  'gravity_hash_compute_buffer';
function _gravity_hash_compute_float(f: gravity_float_t): UInt32; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_hash_compute_float';
function _gravity_hash_compute_int(n: gravity_int_t): UInt32; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_hash_compute_int';
function _gravity_hash_count(hashtable: Pgravity_hash_t): UInt32; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_hash_count';
procedure _gravity_hash_dump(hashtable: Pgravity_hash_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_hash_dump';
procedure _gravity_hash_iterate(hashtable: Pgravity_hash_t;
  iterate: gravity_hash_iterate_fn; data: Pointer); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_hash_iterate';
procedure _gravity_hash_iterate2(hashtable: Pgravity_hash_t;
  iterate: gravity_hash_iterate2_fn; data1: Pointer; data2: Pointer); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_hash_iterate2';
function _gravity_hash_memsize(hashtable: Pgravity_hash_t): UInt32; cdecl;
  external LIBNAME_DLL name _PU + 'gravity_hash_memsize';
procedure _gravity_hash_resetfree(hashtable: Pgravity_hash_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_hash_resetfree';
procedure _gravity_hash_stat(hashtable: Pgravity_hash_t); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_hash_stat';
procedure _gravity_hash_transform(hashtable: Pgravity_hash_t;
  iterate: gravity_hash_transform_fn; data: Pointer); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_hash_transform';
procedure _gravity_hash_keyfree(table: Pgravity_hash_t; key: gravity_value_t;
  value: gravity_value_t; data: Pointer); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_hash_keyfree';
procedure _gravity_hash_keyvaluefree(table: Pgravity_hash_t;
  key: gravity_value_t; value: gravity_value_t; data: Pointer); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_hash_keyvaluefree';
procedure _gravity_hash_valuefree(table: Pgravity_hash_t; key: gravity_value_t;
  value: gravity_value_t; data: Pointer); cdecl;
  external LIBNAME_DLL name _PU + 'gravity_hash_valuefree';
{$ENDIF GRAVITY_STATIC_LINK}
{ TGVInterface }

procedure TGVInterface.CheckLibraryLoaded;
begin
  Assert(TryLoadLibrary, 'Library Error');
end;

constructor TGVInterface.Create;
begin
  inherited;
  FGVLibrary := 0;
  ResetClassReferences;
end;

procedure TGVInterface.FreeGVLibrary;
begin
  if (FGVLibrary <= HINSTANCE_ERROR) then
    exit;
  FreeLibrary(FGVLibrary);
  FGVLibrary := 0;
end;

function TGVInterface.GetClassBool: Pgravity_class_t;
begin
  if not Assigned(Fgravity_class_bool) then
    Fgravity_class_bool := gravity_core_class_from_name
      (GRAVITY_CLASS_BOOL_NAME);
  result := Fgravity_class_bool;
end;

function TGVInterface.GetClassClass: Pgravity_class_t;
begin
  if not Assigned(Fgravity_class_class) then
    Fgravity_class_class := gravity_core_class_from_name
      (GRAVITY_CLASS_CLASS_NAME);
  result := Fgravity_class_class;
end;

function TGVInterface.GetClassClosure: Pgravity_class_t;
begin
  if not Assigned(Fgravity_class_closure) then
    Fgravity_class_closure := gravity_core_class_from_name
      (GRAVITY_CLASS_CLOSURE_NAME);
  result := Fgravity_class_closure;
end;

function TGVInterface.GetClassFiber: Pgravity_class_t;
begin
  if not Assigned(Fgravity_class_fiber) then
    Fgravity_class_fiber := gravity_core_class_from_name
      (GRAVITY_CLASS_FIBER_NAME);
  result := Fgravity_class_fiber;
end;

function TGVInterface.GetClassFloat: Pgravity_class_t;
begin
  if not Assigned(Fgravity_class_float) then
    Fgravity_class_float := gravity_core_class_from_name
      (GRAVITY_CLASS_FLOAT_NAME);
  result := Fgravity_class_float;
end;

function TGVInterface.GetClassFunction: Pgravity_class_t;
begin
  if not Assigned(Fgravity_class_function) then
    Fgravity_class_function := gravity_core_class_from_name
      (GRAVITY_CLASS_FUNCTION_NAME);
  result := Fgravity_class_function;
end;

function TGVInterface.GetClassInstance: Pgravity_class_t;
begin
  if not Assigned(Fgravity_class_instance) then
    Fgravity_class_instance := gravity_core_class_from_name
      (GRAVITY_CLASS_INSTANCE_NAME);
  result := Fgravity_class_instance;
end;

function TGVInterface.GetClassInt: Pgravity_class_t;
begin
  if not Assigned(Fgravity_class_int) then
    Fgravity_class_int := gravity_core_class_from_name(GRAVITY_CLASS_INT_NAME);
  result := Fgravity_class_int;
end;

function TGVInterface.GetClassList: Pgravity_class_t;
begin
  if not Assigned(Fgravity_class_list) then
    Fgravity_class_list := gravity_core_class_from_name
      (GRAVITY_CLASS_LIST_NAME);
  result := Fgravity_class_list;
end;

function TGVInterface.GetClassMap: Pgravity_class_t;
begin
  if not Assigned(Fgravity_class_map) then
    Fgravity_class_map := gravity_core_class_from_name(GRAVITY_CLASS_MAP_NAME);
  result := Fgravity_class_map;
end;

function TGVInterface.GetClassNull: Pgravity_class_t;
begin
  if not Assigned(Fgravity_class_null) then
    Fgravity_class_null := gravity_core_class_from_name
      (GRAVITY_CLASS_NULL_NAME);
  result := Fgravity_class_null;
end;

function TGVInterface.GetClassObject: Pgravity_class_t;
begin
  if not Assigned(Fgravity_class_object) then
    Fgravity_class_object := gravity_core_class_from_name
      (GRAVITY_CLASS_OBJECT_NAME);
  result := Fgravity_class_object;
end;

function TGVInterface.GetClassRange: Pgravity_class_t;
begin
  if not Assigned(Fgravity_class_range) then
    Fgravity_class_range := gravity_core_class_from_name
      (GRAVITY_CLASS_RANGE_NAME);
  result := Fgravity_class_range;
end;

function TGVInterface.GetClassString: Pgravity_class_t;
begin
  if not Assigned(Fgravity_class_string) then
    Fgravity_class_string := gravity_core_class_from_name
      (GRAVITY_CLASS_STRING_NAME);
  result := Fgravity_class_string;
end;

function TGVInterface.GetClassSystem: Pgravity_class_t;
begin
  if not Assigned(Fgravity_class_system) then
    Fgravity_class_system := gravity_core_class_from_name
      (GRAVITY_CLASS_SYSTEM_NAME);
  result := Fgravity_class_system;
end;

function TGVInterface.GetClassUpvalue: Pgravity_class_t;
begin
  if not Assigned(Fgravity_class_upvalue) then
    Fgravity_class_upvalue := gravity_core_class_from_name
      (GRAVITY_CLASS_UPVALUE_NAME);
  result := Fgravity_class_upvalue;
end;

function TGVInterface.GET_VALUE(args: Pgravity_value_t;
  ndx: UInt16): gravity_value_t;
begin
  Result := Pgravity_value_t(NativeUInt(args) + ndx * SizeOf(gravity_value_t))^;
end;

function TGVInterface.LibraryName: String;
begin
  result := LIBNAME_DLL;
end;

procedure TGVInterface.LoadGVLibrary;
{$IFNDEF GRAVITY_STATIC_LINK}
var
  ALibName: String;
  AHandle: THandle;

  function TryGetProcAddr(ProcName: String): Pointer;
  begin
    result := GetProcAddress(AHandle, PChar(ProcName));
  end;

  function GetProcAddr(ProcName: String): Pointer;
  begin
    result := GetProcAddress(AHandle, PChar(ProcName));
    if not Assigned(result) then
      RaiseLastOSError;
  end;
{$ENDIF GRAVITY_STATIC_LINK}

begin
{$IFDEF GRAVITY_STATIC_LINK}
  Fgravity_module_new := _gravity_module_new;
  Fgravity_module_free := _gravity_module_free;
  Fgravity_module_blacken := _gravity_module_blacken;
  Fgravity_module_size := _gravity_module_size;
  Fgravity_bytecode_deserialize := _gravity_bytecode_deserialize;
  Fgravity_function_blacken := _gravity_function_blacken;
  Fgravity_function_cpool_add := _gravity_function_cpool_add;
  Fgravity_function_cpool_get := _gravity_function_cpool_get;
  Fgravity_function_deserialize := _gravity_function_deserialize;
  Fgravity_function_dump := _gravity_function_dump;
  Fgravity_function_free := _gravity_function_free;
  Fgravity_function_new := _gravity_function_new;
  Fgravity_function_new_bridged := _gravity_function_new_bridged;
  Fgravity_function_new_internal := _gravity_function_new_internal;
  Fgravity_function_new_special := _gravity_function_new_special;
  Fgravity_function_serialize := _gravity_function_serialize;
  Fgravity_function_setxdata := _gravity_function_setxdata;
  Fgravity_function_size := _gravity_function_size;

  Fgravity_closure_blacken := _gravity_closure_blacken;
  Fgravity_closure_dec_refcount := _gravity_closure_dec_refcount;
  Fgravity_closure_inc_refcount := _gravity_closure_inc_refcount;
  Fgravity_closure_free := _gravity_closure_free;
  Fgravity_closure_size := _gravity_closure_size;
  Fgravity_closure_new := _gravity_closure_new;

  Fgravity_upvalue_blacken := _gravity_upvalue_blacken;
  Fgravity_upvalue_free := _gravity_upvalue_free;
  Fgravity_upvalue_new := _gravity_upvalue_new;
  Fgravity_upvalue_size := _gravity_upvalue_size;
  Fgravity_class_blacken := _gravity_class_blacken;
  Fgravity_class_add_ivar := _gravity_class_add_ivar;
  Fgravity_class_bind := _gravity_class_bind;
  Fgravity_class_count_ivars := _gravity_class_count_ivars;
  Fgravity_class_deserialize := _gravity_class_deserialize;
  Fgravity_class_dump := _gravity_class_dump;
  Fgravity_class_free := _gravity_class_free;
  Fgravity_class_free_core := _gravity_class_free_core;
  Fgravity_class_get_meta := _gravity_class_get_meta;
  Fgravity_class_getsuper := _gravity_class_getsuper;
  Fgravity_class_grow := _gravity_class_grow;
  Fgravity_class_is_meta := _gravity_class_is_meta;
  Fgravity_class_lookup := _gravity_class_lookup;
  Fgravity_class_lookup_closure := _gravity_class_lookup_closure;
  Fgravity_class_lookup_constructor := _gravity_class_lookup_constructor;
  Fgravity_class_new_pair := _gravity_class_new_pair;
  Fgravity_class_new_single := _gravity_class_new_single;
  Fgravity_class_serialize := _gravity_class_serialize;
  Fgravity_class_setsuper := _gravity_class_setsuper;
  Fgravity_class_setxdata := _gravity_class_setxdata;
  Fgravity_class_size := _gravity_class_size;
  Fgravity_fiber_blacken := _gravity_fiber_blacken;
  Fgravity_fiber_free := _gravity_fiber_free;
  Fgravity_fiber_new := _gravity_fiber_new;
  Fgravity_fiber_reassign := _gravity_fiber_reassign;
  Fgravity_fiber_seterror := _gravity_fiber_seterror;
  Fgravity_fiber_size := _gravity_fiber_size;
  Fgravity_instance_blacken := _gravity_instance_blacken;
  Fgravity_instance_clone := _gravity_instance_clone;
  Fgravity_instance_free := _gravity_instance_free;
  Fgravity_instance_lookup_event := _gravity_instance_lookup_event;
  Fgravity_instance_new := _gravity_instance_new;
  Fgravity_instance_setivar := _gravity_instance_setivar;
  Fgravity_instance_setxdata := _gravity_instance_setxdata;
  Fgravity_instance_size := _gravity_instance_size;
  Fgravity_value_dump := _gravity_value_dump;
  Fgravity_value_equals := _gravity_value_equals;
  Fgravity_value_free := _gravity_value_free;
  Fgravity_value_getclass := _gravity_value_getclass;
  Fgravity_value_getsuper := _gravity_value_getsuper;
  Fgravity_value_hash := _gravity_value_hash;
  Fgravity_value_isobject := _gravity_value_isobject;
  Fgravity_value_serialize := _gravity_value_serialize;
  Fgravity_value_size := _gravity_value_size;
  Fgravity_value_xdata := _gravity_value_xdata;

  Fgravity_value_from_bool := _gravity_value_from_bool;
  Fgravity_value_from_error := _gravity_value_from_error;
  Fgravity_value_from_float := _gravity_value_from_float;
  Fgravity_value_from_int := _gravity_value_from_int;
  Fgravity_value_from_null := _gravity_value_from_null;
  Fgravity_value_from_object := _gravity_value_from_object;
  Fgravity_value_from_undefined := _gravity_value_from_undefined;

  Fgravity_object_blacken := _gravity_object_blacken;
  Fgravity_object_debug := _gravity_object_debug;
  Fgravity_object_deserialize := _gravity_object_deserialize;
  Fgravity_object_free := _gravity_object_free;
  Fgravity_object_serialize := _gravity_object_serialize;
  Fgravity_object_size := _gravity_object_size;
  Fgravity_list_append_list := _gravity_list_append_list;
  Fgravity_list_blacken := _gravity_list_blacken;
  Fgravity_list_free := _gravity_list_free;
  Fgravity_list_from_array := _gravity_list_from_array;
  Fgravity_list_new := _gravity_list_new;
  Fgravity_list_size := _gravity_list_size;
  Fgravity_map_blacken := _gravity_map_blacken;
  Fgravity_map_append_map := _gravity_map_append_map;
  Fgravity_map_free := _gravity_map_free;
  Fgravity_map_insert := _gravity_map_insert;
  Fgravity_map_new := _gravity_map_new;
  Fgravity_map_size := _gravity_map_size;
  Fgravity_range_blacken := _gravity_range_blacken;
  Fgravity_range_free := _gravity_range_free;
  Fgravity_range_new := _gravity_range_new;
  Fgravity_range_size := _gravity_range_size;
  Fgravity_string_blacken := _gravity_string_blacken;
  Fgravity_string_free := _gravity_string_free;
  Fgravity_string_new := _gravity_string_new;
  Fgravity_string_set := _gravity_string_set;
  Fgravity_string_size := _gravity_string_size;
  Fgravity_string_to_value := _gravity_string_to_value;
  Fgravity_compiler_create := _gravity_compiler_create;
  Fgravity_compiler_run := _gravity_compiler_run;
  Fgravity_compiler_ast := _gravity_compiler_ast;
  Fgravity_compiler_free := _gravity_compiler_free;
  Fgravity_compiler_serialize := _gravity_compiler_serialize;
  Fgravity_compiler_serialize_infile := _gravity_compiler_serialize_infile;
  Fgravity_compiler_transfer := _gravity_compiler_transfer;
  Fgravity_vm_delegate := _gravity_vm_delegate;
  Fgravity_vm_fiber := _gravity_vm_fiber;
  Fgravity_vm_free := _gravity_vm_free;
  Fgravity_vm_getvalue := _gravity_vm_getvalue;
  Fgravity_vm_keyindex := _gravity_vm_keyindex;
  Fgravity_vm_ismini := _gravity_vm_ismini;
  Fgravity_vm_isaborted := _gravity_vm_isaborted;
  Fgravity_vm_loadclosure := _gravity_vm_loadclosure;
  Fgravity_vm_lookup := _gravity_vm_lookup;
  Fgravity_vm_new := _gravity_vm_new;
  Fgravity_vm_newmini := _gravity_vm_newmini;
  Fgravity_vm_result := _gravity_vm_result;
  Fgravity_vm_runclosure := _gravity_vm_runclosure;
  Fgravity_vm_runmain := _gravity_vm_runmain;
  Fgravity_vm_set_callbacks := _gravity_vm_set_callbacks;
  Fgravity_vm_setaborted := _gravity_vm_setaborted;
  Fgravity_vm_seterror := _gravity_vm_seterror;
  Fgravity_vm_seterror_string := _gravity_vm_seterror_string;
  Fgravity_vm_setfiber := _gravity_vm_setfiber;
  Fgravity_vm_setvalue := _gravity_vm_setvalue;
  Fgravity_vm_time := _gravity_vm_time;
  Fgravity_vm_cleanup := _gravity_vm_cleanup;
  Fgravity_vm_filter := _gravity_vm_filter;
  Fgravity_vm_transfer := _gravity_vm_transfer;
  Fgravity_vm_initmodule := _gravity_vm_initmodule;
  Fgravity_vm_loadbuffer := _gravity_vm_loadbuffer;
  Fgravity_vm_loadfile := _gravity_vm_loadfile;
  Fgravity_vm_fastlookup := _gravity_vm_fastlookup;
  Fgravity_vm_getdata := _gravity_vm_getdata;
  Fgravity_vm_getslot := _gravity_vm_getslot;
  Fgravity_vm_setdata := _gravity_vm_setdata;
  Fgravity_vm_setslot := _gravity_vm_setslot;
  Fgravity_vm_memupdate := _gravity_vm_memupdate;
  Fgravity_vm_anonymous := _gravity_vm_anonymous;
  Fgravity_vm_get := _gravity_vm_get;
  Fgravity_vm_set := _gravity_vm_set;
  Fgravity_gray_object := _gravity_gray_object;
  Fgravity_gray_value := _gravity_gray_value;
  Fgravity_gc_setenabled := _gravity_gc_setenabled;
  Fgravity_gc_start := _gravity_gc_start;
  Fgravity_gc_temppop := _gravity_gc_temppop;
  Fgravity_gc_temppush := _gravity_gc_temppush;
  Fgravity_core_class_from_name := _gravity_core_class_from_name;
  Fgravity_core_free := _gravity_core_free;
  Fgravity_core_identifiers := _gravity_core_identifiers;
  Fgravity_core_register := _gravity_core_register;
  Fgravity_iscore_class := _gravity_iscore_class;
  Fconvert_value2bool := _convert_value2bool;
  Fconvert_value2float := _convert_value2float;
  Fconvert_value2int := _convert_value2int;
  Fconvert_value2string := _convert_value2string;
  Fcomputed_property_create := _computed_property_create;
  Fcomputed_property_free := _computed_property_free;
  Fgravity_hash_create := _gravity_hash_create;
  Fgravity_hash_free := _gravity_hash_free;
  Fgravity_hash_insert := _gravity_hash_insert;
  Fgravity_hash_isempty := _gravity_hash_isempty;
  Fgravity_hash_lookup := _gravity_hash_lookup;
  Fgravity_hash_remove := _gravity_hash_remove;
  Fgravity_hash_append := _gravity_hash_append;
  Fgravity_hash_compute_buffer := _gravity_hash_compute_buffer;
  Fgravity_hash_compute_float := _gravity_hash_compute_float;
  Fgravity_hash_compute_int := _gravity_hash_compute_int;
  Fgravity_hash_count := _gravity_hash_count;
  Fgravity_hash_dump := _gravity_hash_dump;
  Fgravity_hash_iterate := _gravity_hash_iterate;
  Fgravity_hash_iterate2 := _gravity_hash_iterate2;

  Fgravity_hash_memsize := _gravity_hash_memsize;
  Fgravity_hash_resetfree := _gravity_hash_resetfree;
  Fgravity_hash_stat := _gravity_hash_stat;
  Fgravity_hash_transform := _gravity_hash_transform;
  Fgravity_hash_keyfree := _gravity_hash_keyfree;
  Fgravity_hash_keyvaluefree := _gravity_hash_keyvaluefree;
  Fgravity_hash_valuefree := _gravity_hash_valuefree;
{$ELSE GRAVITY_STATIC_LINK}
  ALibName := LibraryName;
  FGVLibrary := LoadLibrary(PChar(ALibName));
  if FGVLibrary <> 0 then
  begin
    AHandle := FGVLibrary;
    Fgravity_module_new := GetProcAddr('gravity_module_new');
    Fgravity_module_free := GetProcAddr('gravity_module_free');
    Fgravity_module_blacken := GetProcAddr('gravity_module_blacken');
    Fgravity_module_size := GetProcAddr('gravity_module_size');
    Fgravity_bytecode_deserialize :=
      GetProcAddr('gravity_bytecode_deserialize');
    Fgravity_function_blacken := GetProcAddr('gravity_function_blacken');
    Fgravity_function_cpool_add := GetProcAddr('gravity_function_cpool_add');
    Fgravity_function_cpool_get := GetProcAddr('gravity_function_cpool_get');
    Fgravity_function_deserialize :=
      GetProcAddr('gravity_function_deserialize');
    Fgravity_function_dump := GetProcAddr('gravity_function_dump');
    Fgravity_function_free := GetProcAddr('gravity_function_free');
    Fgravity_function_new := GetProcAddr('gravity_function_new');
    Fgravity_function_new_bridged :=
      GetProcAddr('gravity_function_new_bridged');
    Fgravity_function_new_internal :=
      GetProcAddr('gravity_function_new_internal');
    Fgravity_function_new_special :=
      GetProcAddr('gravity_function_new_special');
    Fgravity_function_serialize := GetProcAddr('gravity_function_serialize');
    Fgravity_function_setxdata := GetProcAddr('gravity_function_setxdata');
    Fgravity_function_size := GetProcAddr('gravity_function_size');
    Fgravity_closure_blacken := GetProcAddr('gravity_closure_blacken');
    Fgravity_closure_dec_refcount :=
      GetProcAddr('gravity_closure_dec_refcount');
    Fgravity_closure_inc_refcount :=
      GetProcAddr('gravity_closure_inc_refcount');
    Fgravity_closure_free := GetProcAddr('gravity_closure_free');
    Fgravity_closure_size := GetProcAddr('gravity_closure_size');
    Fgravity_closure_new := GetProcAddr('gravity_closure_new');
    Fgravity_upvalue_blacken := GetProcAddr('gravity_upvalue_blacken');
    Fgravity_upvalue_free := GetProcAddr('gravity_upvalue_free');
    Fgravity_upvalue_new := GetProcAddr('gravity_upvalue_new');
    Fgravity_upvalue_size := GetProcAddr('gravity_upvalue_size');
    Fgravity_class_blacken := GetProcAddr('gravity_class_blacken');
    Fgravity_class_add_ivar := GetProcAddr('gravity_class_add_ivar');
    Fgravity_class_bind := GetProcAddr('gravity_class_bind');
    Fgravity_class_count_ivars := GetProcAddr('gravity_class_count_ivars');
    Fgravity_class_deserialize := GetProcAddr('gravity_class_deserialize');
    Fgravity_class_dump := GetProcAddr('gravity_class_dump');
    Fgravity_class_free := GetProcAddr('gravity_class_free');
    Fgravity_class_free_core := GetProcAddr('gravity_class_free_core');
    Fgravity_class_get_meta := GetProcAddr('gravity_class_get_meta');
    Fgravity_class_getsuper := GetProcAddr('gravity_class_getsuper');
    Fgravity_class_grow := GetProcAddr('gravity_class_grow');
    Fgravity_class_is_meta := GetProcAddr('gravity_class_is_meta');
    Fgravity_class_lookup := GetProcAddr('gravity_class_lookup');
    Fgravity_class_lookup_closure :=
      GetProcAddr('gravity_class_lookup_closure');
    Fgravity_class_lookup_constructor :=
      GetProcAddr('gravity_class_lookup_constructor');
    Fgravity_class_new_pair := GetProcAddr('gravity_class_new_pair');
    Fgravity_class_new_single := GetProcAddr('gravity_class_new_single');
    Fgravity_class_serialize := GetProcAddr('gravity_class_serialize');
    Fgravity_class_setsuper := GetProcAddr('gravity_class_setsuper');
    Fgravity_class_setxdata := GetProcAddr('gravity_class_setxdata');
    Fgravity_class_size := GetProcAddr('gravity_class_size');
    Fgravity_fiber_blacken := GetProcAddr('gravity_fiber_blacken');
    Fgravity_fiber_free := GetProcAddr('gravity_fiber_free');
    Fgravity_fiber_new := GetProcAddr('gravity_fiber_new');
    Fgravity_fiber_reassign := GetProcAddr('gravity_fiber_reassign');
    Fgravity_fiber_seterror := GetProcAddr('gravity_fiber_seterror');
    Fgravity_fiber_size := GetProcAddr('gravity_fiber_size');
    Fgravity_instance_blacken := GetProcAddr('gravity_instance_blacken');
    Fgravity_instance_clone := GetProcAddr('gravity_instance_clone');
    Fgravity_instance_free := GetProcAddr('gravity_instance_free');
    Fgravity_instance_lookup_event :=
      GetProcAddr('gravity_instance_lookup_event');
    Fgravity_instance_new := GetProcAddr('gravity_instance_new');
    Fgravity_instance_setivar := GetProcAddr('gravity_instance_setivar');
    Fgravity_instance_setxdata := GetProcAddr('gravity_instance_setxdata');
    Fgravity_instance_size := GetProcAddr('gravity_instance_size');
    Fgravity_value_dump := GetProcAddr('gravity_value_dump');
    Fgravity_value_equals := GetProcAddr('gravity_value_equals');
    Fgravity_value_free := GetProcAddr('gravity_value_free');
    Fgravity_value_getclass := GetProcAddr('gravity_value_getclass');
    Fgravity_value_getsuper := GetProcAddr('gravity_value_getsuper');
    Fgravity_value_hash := GetProcAddr('gravity_value_hash');
    Fgravity_value_isobject := GetProcAddr('gravity_value_isobject');
    Fgravity_value_serialize := GetProcAddr('gravity_value_serialize');
    Fgravity_value_size := GetProcAddr('gravity_value_size');
    Fgravity_value_xdata := GetProcAddr('gravity_value_xdata');

    Fgravity_value_from_bool := GetProcAddr('gravity_value_from_bool');
    Fgravity_value_from_error := GetProcAddr('gravity_value_from_error');
    Fgravity_value_from_float := GetProcAddr('gravity_value_from_float');
    Fgravity_value_from_int := GetProcAddr('gravity_value_from_int');
    Fgravity_value_from_null := GetProcAddr('gravity_value_from_null');
    Fgravity_value_from_object := GetProcAddr('gravity_value_from_object');
    Fgravity_value_from_undefined :=
      GetProcAddr('gravity_value_from_undefined');

    Fgravity_object_blacken := GetProcAddr('gravity_object_blacken');
    Fgravity_object_debug := GetProcAddr('gravity_object_debug');
    Fgravity_object_deserialize := GetProcAddr('gravity_object_deserialize');
    Fgravity_object_free := GetProcAddr('gravity_object_free');
    Fgravity_object_serialize := GetProcAddr('gravity_object_serialize');
    Fgravity_object_size := GetProcAddr('gravity_object_size');
    Fgravity_list_append_list := GetProcAddr('gravity_list_append_list');
    Fgravity_list_blacken := GetProcAddr('gravity_list_blacken');
    Fgravity_list_free := GetProcAddr('gravity_list_free');
    Fgravity_list_from_array := GetProcAddr('gravity_list_from_array');
    Fgravity_list_new := GetProcAddr('gravity_list_new');
    Fgravity_list_size := GetProcAddr('gravity_list_size');
    Fgravity_map_blacken := GetProcAddr('gravity_map_blacken');
    Fgravity_map_append_map := GetProcAddr('gravity_map_append_map');
    Fgravity_map_free := GetProcAddr('gravity_map_free');
    Fgravity_map_insert := GetProcAddr('gravity_map_insert');
    Fgravity_map_new := GetProcAddr('gravity_map_new');
    Fgravity_map_size := GetProcAddr('gravity_map_size');
    Fgravity_range_blacken := GetProcAddr('gravity_range_blacken');
    Fgravity_range_free := GetProcAddr('gravity_range_free');
    Fgravity_range_new := GetProcAddr('gravity_range_new');
    Fgravity_range_size := GetProcAddr('gravity_range_size');
    Fgravity_string_blacken := GetProcAddr('gravity_string_blacken');
    Fgravity_string_free := GetProcAddr('gravity_string_free');
    Fgravity_string_new := GetProcAddr('gravity_string_new');
    Fgravity_string_set := GetProcAddr('gravity_string_set');
    Fgravity_string_size := GetProcAddr('gravity_string_size');
    Fgravity_string_to_value := GetProcAddr('gravity_string_to_value');
    Fgravity_compiler_create := GetProcAddr('gravity_compiler_create');
    Fgravity_compiler_run := GetProcAddr('gravity_compiler_run');
    Fgravity_compiler_ast := GetProcAddr('gravity_compiler_ast');
    Fgravity_compiler_free := GetProcAddr('gravity_compiler_free');
    Fgravity_compiler_serialize := GetProcAddr('gravity_compiler_serialize');
    Fgravity_compiler_serialize_infile :=
      GetProcAddr('gravity_compiler_serialize_infile');
    Fgravity_compiler_transfer := GetProcAddr('gravity_compiler_transfer');
    Fgravity_vm_delegate := GetProcAddr('gravity_vm_delegate');
    Fgravity_vm_fiber := GetProcAddr('gravity_vm_fiber');
    Fgravity_vm_free := GetProcAddr('gravity_vm_free');
    Fgravity_vm_getvalue := GetProcAddr('gravity_vm_getvalue');
    Fgravity_vm_keyindex := GetProcAddr('gravity_vm_keyindex');
    Fgravity_vm_ismini := GetProcAddr('gravity_vm_ismini');
    Fgravity_vm_isaborted := GetProcAddr('gravity_vm_isaborted');
    Fgravity_vm_loadclosure := GetProcAddr('gravity_vm_loadclosure');
    Fgravity_vm_lookup := GetProcAddr('gravity_vm_lookup');
    Fgravity_vm_new := GetProcAddr('gravity_vm_new');
    Fgravity_vm_newmini := GetProcAddr('gravity_vm_newmini');
    Fgravity_vm_result := GetProcAddr('gravity_vm_result');
    Fgravity_vm_runclosure := GetProcAddr('gravity_vm_runclosure');
    Fgravity_vm_runmain := GetProcAddr('gravity_vm_runmain');
    Fgravity_vm_set_callbacks := GetProcAddr('gravity_vm_set_callbacks');
    Fgravity_vm_setaborted := GetProcAddr('gravity_vm_setaborted');
    Fgravity_vm_seterror := GetProcAddr('gravity_vm_seterror');
    Fgravity_vm_seterror_string := GetProcAddr('gravity_vm_seterror_string');
    Fgravity_vm_setfiber := GetProcAddr('gravity_vm_setfiber');
    Fgravity_vm_setvalue := GetProcAddr('gravity_vm_setvalue');
    Fgravity_vm_time := GetProcAddr('gravity_vm_time');
    Fgravity_vm_cleanup := GetProcAddr('gravity_vm_cleanup');
    Fgravity_vm_filter := GetProcAddr('gravity_vm_filter');
    Fgravity_vm_transfer := GetProcAddr('gravity_vm_transfer');
    Fgravity_vm_initmodule := GetProcAddr('gravity_vm_initmodule');
    Fgravity_vm_loadbuffer := GetProcAddr('gravity_vm_loadbuffer');
    Fgravity_vm_loadfile := GetProcAddr('gravity_vm_loadfile');
    Fgravity_vm_fastlookup := GetProcAddr('gravity_vm_fastlookup');
    Fgravity_vm_getdata := GetProcAddr('gravity_vm_getdata');
    Fgravity_vm_getslot := GetProcAddr('gravity_vm_getslot');
    Fgravity_vm_setdata := GetProcAddr('gravity_vm_setdata');
    Fgravity_vm_setslot := GetProcAddr('gravity_vm_setslot');
    Fgravity_vm_memupdate := GetProcAddr('gravity_vm_memupdate');
    Fgravity_vm_anonymous := GetProcAddr('gravity_vm_anonymous');
    Fgravity_vm_get := GetProcAddr('gravity_vm_get');
    Fgravity_vm_set := GetProcAddr('gravity_vm_set');
    Fgravity_gray_object := GetProcAddr('gravity_gray_object');
    Fgravity_gray_value := GetProcAddr('gravity_gray_value');
    Fgravity_gc_setenabled := GetProcAddr('gravity_gc_setenabled');
    Fgravity_gc_start := GetProcAddr('gravity_gc_start');
    Fgravity_gc_temppop := GetProcAddr('gravity_gc_temppop');
    Fgravity_gc_temppush := GetProcAddr('gravity_gc_temppush');
    Fgravity_core_class_from_name :=
      GetProcAddr('gravity_core_class_from_name');
    Fgravity_core_free := GetProcAddr('gravity_core_free');
    Fgravity_core_identifiers := GetProcAddr('gravity_core_identifiers');
    Fgravity_core_register := GetProcAddr('gravity_core_register');
    Fgravity_iscore_class := GetProcAddr('gravity_iscore_class');
    Fconvert_value2bool := GetProcAddr('convert_value2bool');
    Fconvert_value2float := GetProcAddr('convert_value2float');
    Fconvert_value2int := GetProcAddr('convert_value2int');
    Fconvert_value2string := GetProcAddr('convert_value2string');
    Fcomputed_property_create := GetProcAddr('computed_property_create');
    Fcomputed_property_free := GetProcAddr('computed_property_free');
    Fgravity_hash_create := GetProcAddr('gravity_hash_create');
    Fgravity_hash_free := GetProcAddr('gravity_hash_free');
    Fgravity_hash_insert := GetProcAddr('gravity_hash_insert');
    Fgravity_hash_isempty := GetProcAddr('gravity_hash_isempty');
    Fgravity_hash_lookup := GetProcAddr('gravity_hash_lookup');
    Fgravity_hash_remove := GetProcAddr('gravity_hash_remove');
    Fgravity_hash_append := GetProcAddr('gravity_hash_append');
    Fgravity_hash_compute_buffer := GetProcAddr('gravity_hash_compute_buffer');
    Fgravity_hash_compute_float := GetProcAddr('gravity_hash_compute_float');
    Fgravity_hash_compute_int := GetProcAddr('gravity_hash_compute_int');
    Fgravity_hash_count := GetProcAddr('gravity_hash_count');
    Fgravity_hash_dump := GetProcAddr('gravity_hash_dump');
    Fgravity_hash_iterate := GetProcAddr('gravity_hash_iterate');
    Fgravity_hash_iterate2 := GetProcAddr('gravity_hash_iterate2');
    Fgravity_hash_memsize := GetProcAddr('gravity_hash_memsize');
    Fgravity_hash_resetfree := GetProcAddr('gravity_hash_resetfree');
    Fgravity_hash_stat := GetProcAddr('gravity_hash_stat');
    Fgravity_hash_transform := GetProcAddr('gravity_hash_transform');
    Fgravity_hash_keyfree := GetProcAddr('gravity_hash_keyfree');
    Fgravity_hash_keyvaluefree := GetProcAddr('gravity_hash_keyvaluefree');
    Fgravity_hash_valuefree := GetProcAddr('gravity_hash_valuefree');
  end;
{$ENDIF GRAVITY_STATIC_LINK}
end;

function TGVInterface.NEW_CLOSURE_VALUE_BRIDGED(identifier: AnsiString;
  xdata: pointer): gravity_value_t;
begin
  result.isa := gravity_class_closure;
  result.f2.p := Pgravity_object_t(gravity_closure_new(nil,
    NEW_FUNCTION_BRIDGED(identifier, xdata)));
end;

function TGVInterface.NEW_CLOSURE_VALUE_SPECIAL(identifier: AnsiString;
  index: Uint16; getter, setter: pointer): gravity_value_t;
begin
  result.isa := gravity_class_closure;
  result.f2.p := Pgravity_object_t(gravity_closure_new(nil,
    NEW_FUNCTION_SPECIAL(identifier, index, getter, setter)));
end;

function TGVInterface.NEW_CLOSURE_VALUE(fptr: gravity_c_internal)
  : gravity_value_t;
begin
  result.isa := gravity_class_closure;
  result.f2.p := Pgravity_object_t(gravity_closure_new(nil,
    NEW_FUNCTION(fptr)));
end;

function TGVInterface.NEW_FUNCTION(fptr: gravity_c_internal)
  : Pgravity_function_t;
begin
  result := Fgravity_function_new_internal(nil, nil, fptr, 0);
end;

function TGVInterface.NEW_FUNCTION_BRIDGED(identifier: AnsiString;
  xdata: pointer): Pgravity_function_t;
begin
  Result := Fgravity_function_new_bridged(nil, PAnsiChar(identifier), xdata);
end;

function TGVInterface.NEW_FUNCTION_SPECIAL(identifier: AnsiString;
  index: UInt16; getter, setter: pointer): Pgravity_function_t;
begin
  Result := Fgravity_function_new_special(nil, PAnsiChar(identifier), index, getter, setter);
end;

function TGVInterface.OBJECT_ISA_BOOL(v: Pgravity_object_t): Boolean;
begin
  Result := v^.isa = gravity_class_bool;
end;

function TGVInterface.OBJECT_ISA_CLASS(v: Pgravity_object_t): Boolean;
begin
  Result := v^.isa = gravity_class_class;
end;

function TGVInterface.OBJECT_ISA_CLOSURE(v: Pgravity_object_t): Boolean;
begin

end;

function TGVInterface.OBJECT_ISA_FIBER(v: Pgravity_object_t): Boolean;
begin
  Result := v^.isa = gravity_class_closure;
end;

function TGVInterface.OBJECT_ISA_FLOAT(v: Pgravity_object_t): Boolean;
begin
  Result := v^.isa = gravity_class_float;
end;

function TGVInterface.OBJECT_ISA_FUNCTION(v: Pgravity_object_t): Boolean;
begin
  Result := v^.isa = gravity_class_function;
end;

function TGVInterface.OBJECT_ISA_INSTANCE(v: Pgravity_object_t): Boolean;
begin
  Result := v^.isa = gravity_class_instance;
end;

function TGVInterface.OBJECT_ISA_INT(v: Pgravity_object_t): Boolean;
begin
  Result := v^.isa = gravity_class_int;
end;

function TGVInterface.OBJECT_ISA_LIST(v: Pgravity_object_t): Boolean;
begin
  Result := v^.isa = gravity_class_list;
end;

function TGVInterface.OBJECT_ISA_MAP(v: Pgravity_object_t): Boolean;
begin
  Result := v^.isa = gravity_class_map;
end;

function TGVInterface.OBJECT_ISA_NULL(v: Pgravity_object_t): Boolean;
begin
  Result := v^.isa = gravity_class_null;
end;

function TGVInterface.OBJECT_ISA_RANGE(v: Pgravity_object_t): Boolean;
begin
  Result := v^.isa = gravity_class_range;
end;

function TGVInterface.OBJECT_ISA_STRING(v: Pgravity_object_t): Boolean;
begin
  Result := v^.isa = gravity_class_string;
end;

function TGVInterface.OBJECT_ISA_UPVALUE(v: Pgravity_object_t): Boolean;
begin
  Result := v^.isa = gravity_class_upvalue;
end;

function TGVInterface.OBJECT_IS_VALID(v: Pgravity_object_t): Boolean;
begin
  Result := Assigned(v^.isa);
end;

procedure TGVInterface.ResetClassReferences;
begin
  Fgravity_class_int := nil;
  Fgravity_class_float := nil;
  Fgravity_class_bool := nil;
  Fgravity_class_null := nil;

  Fgravity_class_string := nil;
  Fgravity_class_object := nil;
  Fgravity_class_function := nil;
  Fgravity_class_closure := nil;
  Fgravity_class_fiber := nil;
  Fgravity_class_class := nil;
  Fgravity_class_instance := nil;
  Fgravity_class_list := nil;
  Fgravity_class_map := nil;
  Fgravity_class_range := nil;
  Fgravity_class_upvalue := nil;
  Fgravity_class_system := nil;
end;

function TGVInterface.RETURN_CLOSURE(vm: Pgravity_vm; v: gravity_value_t;
  index: UInt32): Boolean;
begin
  gravity_vm_setslot(vm, v, index);
  Result := False;
end;

function TGVInterface.RETURN_FIBER: Boolean;
begin
  Result := False;
end;

function TGVInterface.RETURN_NOVALUE: Boolean;
begin
  Result := True;
end;

function TGVInterface.RETURN_VALUE(vm: Pgravity_vm; v: gravity_value_t;
  index: UInt32): Boolean;
begin
  gravity_vm_setslot(vm, v, index);
  Result := True;
end;

procedure TGVInterface.SETMETA_INITED(c: Pgravity_class_t);
begin
  if Assigned(c) then
    c^.is_inited := True;
end;

function TGVInterface.TryLoadLibrary: Boolean;
begin
{$IFDEF GRAVITY_STATIC_LINK}
  if not Assigned(Fgravity_vm_new) then
    LoadGVLibrary;
  result := Assigned(Fgravity_vm_new);
{$ELSE GRAVITY_STATIC_LINK}
  if FGVLibrary <= HINSTANCE_ERROR then
    LoadGVLibrary;
  result := FGVLibrary > HINSTANCE_ERROR;
{$ENDIF GRAVITY_STATIC_LINK}
end;

function TGVInterface.VALUE_AS_BOOL(val: gravity_value_t): gravity_int_t;
begin
  result := val.f2.n;
end;

function TGVInterface.VALUE_AS_CLASS(val: gravity_value_t): Pgravity_class_t;
begin
  result := Pgravity_class_t(VALUE_AS_OBJECT(val));
end;

function TGVInterface.VALUE_AS_CLOSURE(val: gravity_value_t)
  : Pgravity_closure_t;
begin
  result := Pgravity_closure_t(VALUE_AS_OBJECT(val));
end;

function TGVInterface.VALUE_AS_CSTRING(val: gravity_value_t): PAnsiChar;
begin
  result := VALUE_AS_STRING(val)^.s;
end;

function TGVInterface.VALUE_AS_ERROR(val: gravity_value_t): PAnsiChar;
begin
  result := PAnsiChar(val.f2.p);
end;

function TGVInterface.VALUE_AS_FIBER(val: gravity_value_t): Pgravity_fiber_t;
begin
  result := Pgravity_fiber_t(VALUE_AS_OBJECT(val));
end;

function TGVInterface.VALUE_AS_FLOAT(val: gravity_value_t): gravity_float_t;
begin
  result := val.f2.f;
end;

function TGVInterface.VALUE_AS_FUNCTION(val: gravity_value_t)
  : Pgravity_function_t;
begin
  result := Pgravity_function_t(VALUE_AS_OBJECT(val));
end;

function TGVInterface.VALUE_AS_INSTANCE(val: gravity_value_t)
  : Pgravity_instance_t;
begin
  result := Pgravity_instance_t(VALUE_AS_OBJECT(val));
end;

function TGVInterface.VALUE_AS_INT(val: gravity_value_t): gravity_int_t;
begin
  result := val.f2.n;
end;

function TGVInterface.VALUE_AS_LIST(val: gravity_value_t): Pgravity_list_t;
begin
  result := Pgravity_list_t(VALUE_AS_OBJECT(val));
end;

function TGVInterface.VALUE_AS_MAP(val: gravity_value_t): Pgravity_map_t;
begin
  result := Pgravity_map_t(VALUE_AS_OBJECT(val));
end;

function TGVInterface.VALUE_AS_OBJECT(val: gravity_value_t): Pgravity_object_t;
begin
  result := val.f2.p;
end;

function TGVInterface.VALUE_AS_RANGE(val: gravity_value_t): Pgravity_range_t;
begin
  result := Pgravity_range_t(VALUE_AS_OBJECT(val));
end;

function TGVInterface.VALUE_AS_STRING(val: gravity_value_t): Pgravity_string_t;
begin
  result := Pgravity_string_t(VALUE_AS_OBJECT(val));
end;

function TGVInterface.VALUE_FROM_BOOL(b: Boolean): gravity_value_t;
begin
  result := gravity_value_from_bool(b);
end;

function TGVInterface.VALUE_FROM_ERROR(msg: PAnsiChar): gravity_value_t;
begin
  result := gravity_value_from_error(msg);
end;

function TGVInterface.VALUE_FROM_FALSE: gravity_value_t;
begin
  result := VALUE_FROM_BOOL(False);
end;

function TGVInterface.VALUE_FROM_FLOAT(n: gravity_float_t): gravity_value_t;
begin
  result := gravity_value_from_float(n);
end;

function TGVInterface.VALUE_FROM_INT(n: gravity_int_t): gravity_value_t;
begin
  result := gravity_value_from_int(n);
end;

function TGVInterface.VALUE_FROM_NULL: gravity_value_t;
begin
  result := gravity_value_from_null;
end;

function TGVInterface.VALUE_FROM_OBJECT(obj: Pointer): gravity_value_t;
begin
  result := gravity_value_from_object(obj)
end;

function TGVInterface.VALUE_FROM_STRING(vm: Pgravity_vm; const s: AnsiString;
  len: UInt32): gravity_value_t;
begin
  result := gravity_string_to_value(vm, s, len);
end;

function TGVInterface.VALUE_FROM_TRUE: gravity_value_t;
begin
  result := VALUE_FROM_BOOL(True);
end;

function TGVInterface.VALUE_FROM_UNDEFINED: gravity_value_t;
begin
  result := gravity_value_from_undefined;
end;

function TGVInterface.VALUE_ISA_BASIC_TYPE(v: gravity_value_t): Boolean;
begin
  result := VALUE_ISA_STRING(v) or VALUE_ISA_INT(v) or VALUE_ISA_FLOAT(v) or
    VALUE_ISA_BOOL(v);
end;

function TGVInterface.VALUE_ISA_BOOL(v: gravity_value_t): Boolean;
begin
  result := v.isa = gravity_class_bool;
end;

function TGVInterface.VALUE_ISA_CALLABLE(v: gravity_value_t): Boolean;
begin
  result := VALUE_ISA_FUNCTION(v) or VALUE_ISA_CLASS(v) or VALUE_ISA_FIBER(v);
end;

function TGVInterface.VALUE_ISA_CLASS(v: gravity_value_t): Boolean;
begin
  result := v.isa = gravity_class_class;
end;

function TGVInterface.VALUE_ISA_CLOSURE(v: gravity_value_t): Boolean;
begin
  result := v.isa = gravity_class_closure;
end;

function TGVInterface.VALUE_ISA_ERROR(v: gravity_value_t): Boolean;
begin
  result := VALUE_ISA_NOTVALID(v);
end;

function TGVInterface.VALUE_ISA_FIBER(v: gravity_value_t): Boolean;
begin
  result := v.isa = gravity_class_fiber;
end;

function TGVInterface.VALUE_ISA_FLOAT(v: gravity_value_t): Boolean;
begin
  result := v.isa = gravity_class_fiber;
end;

function TGVInterface.VALUE_ISA_FUNCTION(v: gravity_value_t): Boolean;
begin
  result := v.isa = gravity_class_function;
end;

function TGVInterface.VALUE_ISA_INSTANCE(v: gravity_value_t): Boolean;
begin
  result := v.isa = gravity_class_instance;
end;

function TGVInterface.VALUE_ISA_INT(v: gravity_value_t): Boolean;
begin
  result := v.isa = gravity_class_int;
end;

function TGVInterface.VALUE_ISA_LIST(v: gravity_value_t): Boolean;
begin
  result := v.isa = gravity_class_list;
end;

function TGVInterface.VALUE_ISA_MAP(v: gravity_value_t): Boolean;
begin
  result := v.isa = gravity_class_map;
end;

function TGVInterface.VALUE_ISA_NOTVALID(v: gravity_value_t): Boolean;
begin
  result := (not Assigned(v.isa)) or (v.isa = nil);
end;

function TGVInterface.VALUE_ISA_NULL(v: gravity_value_t): Boolean;
begin
  result := (v.isa = gravity_class_null) and (v.f2.n = 0);
end;

function TGVInterface.VALUE_ISA_NULLCLASS(v: gravity_value_t): Boolean;
begin
  result := v.isa = gravity_class_null;
end;

function TGVInterface.VALUE_ISA_RANGE(v: gravity_value_t): Boolean;
begin
  result := v.isa = gravity_class_range;
end;

function TGVInterface.VALUE_ISA_STRING(v: gravity_value_t): Boolean;
begin
  result := v.isa = gravity_class_string;
end;

function TGVInterface.VALUE_ISA_UNDEFINED(v: gravity_value_t): Boolean;
begin
  result := (v.isa = gravity_class_null) and (v.f2.n = 1);
end;

function TGVInterface.VALUE_ISA_VALID(v: gravity_value_t): Boolean;
begin
  result := Assigned(v.isa) and (v.isa <> nil);
end;

function TGVInterface.VALUE_NOT_VALID: gravity_value_t;
begin
  result := VALUE_FROM_ERROR(nil)
end;

function TGVInterface.gravity_module_new(vm: Pgravity_vm;
  const identifier: AnsiString): Pgravity_module_t;
begin
  result := Fgravity_module_new(vm, PAnsiChar(identifier));
end;

procedure TGVInterface.gravity_module_free(vm: Pgravity_vm;
  m: Pgravity_module_t);
begin
  Fgravity_module_free(vm, m);
end;

procedure TGVInterface.gravity_module_blacken(vm: Pgravity_vm;
  m: Pgravity_module_t);
begin
  Fgravity_module_blacken(vm, m);
end;

function TGVInterface.gravity_module_size(vm: Pgravity_vm;
  m: Pgravity_module_t): UInt32;
begin
  result := Fgravity_module_size(vm, m);
end;

function TGVInterface.gravity_bytecode_deserialize(const buffer: AnsiString;
  len: NativeUInt; ninst: PUInt32): PUInt32;
begin
  result := Fgravity_bytecode_deserialize(PAnsiChar(buffer), len, ninst);
end;

procedure TGVInterface.gravity_function_blacken(vm: Pgravity_vm;
  f: Pgravity_function_t);
begin
  Fgravity_function_blacken(vm, f);
end;

function TGVInterface.gravity_function_cpool_add(vm: Pgravity_vm;
  f: Pgravity_function_t; v: gravity_value_t): UInt16;
begin
  result := Fgravity_function_cpool_add(vm, f, v);
end;

function TGVInterface.gravity_function_cpool_get(f: Pgravity_function_t;
  i: UInt16): gravity_value_t;
begin
  result := Fgravity_function_cpool_get(f, i);
end;

function TGVInterface.gravity_function_deserialize(vm: Pgravity_vm;
  json: Pjson_value): Pgravity_function_t;
begin
  result := Fgravity_function_deserialize(vm, json);
end;

procedure TGVInterface.gravity_function_dump(f: Pgravity_function_t;
  codef: code_dump_function);
begin
  Fgravity_function_dump(f, codef);
end;

procedure TGVInterface.gravity_function_free(vm: Pgravity_vm;
  f: Pgravity_function_t);
begin
  Fgravity_function_free(vm, f);
end;

function TGVInterface.gravity_function_new(vm: Pgravity_vm;
  const identifier: AnsiString; nparams: UInt16; nlocals: UInt16;
  ntemps: UInt16; code: Pointer): Pgravity_function_t;
begin
  result := Fgravity_function_new(vm, PAnsiChar(identifier), nparams, nlocals,
    ntemps, code);
end;

function TGVInterface.gravity_function_new_bridged(vm: Pgravity_vm;
  const identifier: AnsiString; xdata: Pointer): Pgravity_function_t;
begin
  result := Fgravity_function_new_bridged(vm, PAnsiChar(identifier), xdata);
end;

function TGVInterface.gravity_function_new_internal(vm: Pgravity_vm;
  const identifier: AnsiString; exec: gravity_c_internal; nparams: UInt16)
  : Pgravity_function_t;
begin
  if identifier = '' then
    result := Fgravity_function_new_internal(vm, nil,
      exec, nparams)
  else
    result := Fgravity_function_new_internal(vm, PAnsiChar(identifier),
      exec, nparams);
end;

function TGVInterface.gravity_function_new_special(vm: Pgravity_vm;
  const identifier: AnsiString; index: UInt16; getter: Pointer; setter: Pointer)
  : Pgravity_function_t;
begin
  result := Fgravity_function_new_special(vm, PAnsiChar(identifier), index,
    getter, setter);
end;

procedure TGVInterface.gravity_function_serialize(f: Pgravity_function_t;
  json: Pjson_t);
begin
  Fgravity_function_serialize(f, json);
end;

procedure TGVInterface.gravity_function_setxdata(f: Pgravity_function_t;
  xdata: Pointer);
begin
  Fgravity_function_setxdata(f, xdata);
end;

function TGVInterface.gravity_function_size(vm: Pgravity_vm;
  f: Pgravity_function_t): UInt32;
begin
  result := Fgravity_function_size(vm, f);
end;

procedure TGVInterface.gravity_upvalue_blacken(vm: Pgravity_vm;
  upvalue: Pgravity_upvalue_t);
begin
  Fgravity_upvalue_blacken(vm, upvalue);
end;

procedure TGVInterface.gravity_upvalue_free(vm: Pgravity_vm;
  upvalue: Pgravity_upvalue_t);
begin
  Fgravity_upvalue_free(vm, upvalue);
end;

function TGVInterface.gravity_upvalue_new(vm: Pgravity_vm;
  value: Pgravity_value_t): Pgravity_upvalue_t;
begin
  result := Fgravity_upvalue_new(vm, value);
end;

function TGVInterface.gravity_upvalue_size(vm: Pgravity_vm;
  upvalue: Pgravity_upvalue_t): UInt32;
begin
  result := Fgravity_upvalue_size(vm, upvalue);
end;

procedure TGVInterface.gravity_class_blacken(vm: Pgravity_vm;
  c: Pgravity_class_t);
begin
  Fgravity_class_blacken(vm, c);
end;

function TGVInterface.gravity_class_add_ivar(c: Pgravity_class_t;
  const identifier: AnsiString): Int16;
begin
  result := Fgravity_class_add_ivar(c, PAnsiChar(identifier));
end;

procedure TGVInterface.gravity_class_bind(c: Pgravity_class_t;
  const key: AnsiString; value: gravity_value_t);
begin
  Fgravity_class_bind(c, PAnsiChar(key), value);
end;

function TGVInterface.gravity_class_count_ivars(c: Pgravity_class_t): UInt32;
begin
  result := Fgravity_class_count_ivars(c);
end;

function TGVInterface.gravity_class_deserialize(vm: Pgravity_vm;
  json: Pjson_value): Pgravity_class_t;
begin
  result := Fgravity_class_deserialize(vm, json);
end;

procedure TGVInterface.gravity_class_dump(c: Pgravity_class_t);
begin
  Fgravity_class_dump(c);
end;

procedure TGVInterface.gravity_class_free(vm: Pgravity_vm; c: Pgravity_class_t);
begin
  Fgravity_class_free(vm, c);
end;

procedure TGVInterface.gravity_class_free_core(vm: Pgravity_vm;
  c: Pgravity_class_t);
begin
  Fgravity_class_free_core(vm, c);
end;

function TGVInterface.gravity_class_get_meta(c: Pgravity_class_t)
  : Pgravity_class_t;
begin
  result := Fgravity_class_get_meta(c);
end;

function TGVInterface.gravity_class_getsuper(c: Pgravity_class_t)
  : Pgravity_class_t;
begin
  result := Fgravity_class_getsuper(c);
end;

function TGVInterface.gravity_class_grow(c: Pgravity_class_t;
  n: UInt32): Boolean;
begin
  result := Fgravity_class_grow(c, n);
end;

function TGVInterface.gravity_class_is_meta(c: Pgravity_class_t): Boolean;
begin
  result := Fgravity_class_is_meta(c);
end;

function TGVInterface.gravity_class_lookup(c: Pgravity_class_t;
  key: gravity_value_t): Pgravity_object_t;
begin
  result := Fgravity_class_lookup(c, key);
end;

function TGVInterface.gravity_class_lookup_closure(c: Pgravity_class_t;
  key: gravity_value_t): Pgravity_closure_t;
begin
  result := Fgravity_class_lookup_closure(c, key);
end;

function TGVInterface.gravity_class_lookup_constructor(c: Pgravity_class_t;
  nparams: UInt32): Pgravity_closure_t;
begin
  result := Fgravity_class_lookup_constructor(c, nparams);
end;

function TGVInterface.gravity_class_new_pair(vm: Pgravity_vm;
  const identifier: AnsiString; superclass: Pgravity_class_t; nivar: UInt32;
  nsvar: UInt32): Pgravity_class_t;
begin
  result := Fgravity_class_new_pair(vm, PAnsiChar(identifier), superclass,
    nivar, nsvar);
end;

function TGVInterface.gravity_class_new_single(vm: Pgravity_vm;
  const identifier: AnsiString; nfields: UInt32): Pgravity_class_t;
begin
  result := Fgravity_class_new_single(vm, PAnsiChar(identifier), nfields);
end;

procedure TGVInterface.gravity_class_serialize(c: Pgravity_class_t;
  json: Pjson_t);
begin
  Fgravity_class_serialize(c, json);
end;

function TGVInterface.gravity_class_setsuper(subclass: Pgravity_class_t;
  superclass: Pgravity_class_t): Boolean;
begin
  result := Fgravity_class_setsuper(subclass, superclass);
end;

procedure TGVInterface.gravity_class_setxdata(c: Pgravity_class_t;
  xdata: Pointer);
begin
  Fgravity_class_setxdata(c, xdata);
end;

function TGVInterface.gravity_class_size(vm: Pgravity_vm;
  c: Pgravity_class_t): UInt32;
begin
  result := Fgravity_class_size(vm, c);
end;

procedure TGVInterface.gravity_closure_blacken(vm: Pgravity_vm;
  closure: Pgravity_closure_t);
begin
  Fgravity_closure_blacken(vm, closure);
end;

procedure TGVInterface.gravity_closure_dec_refcount(vm: Pgravity_vm;
  closure: Pgravity_closure_t);
begin
  Fgravity_closure_dec_refcount(vm, closure);
end;

procedure TGVInterface.gravity_closure_inc_refcount(vm: Pgravity_vm;
  closure: Pgravity_closure_t);
begin
  Fgravity_closure_inc_refcount(vm, closure);
end;

procedure TGVInterface.gravity_closure_free(vm: Pgravity_vm;
  closure: Pgravity_closure_t);
begin
  Fgravity_closure_free(vm, closure);
end;

function TGVInterface.gravity_closure_size(vm: Pgravity_vm;
  closure: Pgravity_closure_t): UInt32;
begin
  result := Fgravity_closure_size(vm, closure);
end;

function TGVInterface.gravity_closure_new(vm: Pgravity_vm;
  f: Pgravity_function_t): Pgravity_closure_t;
begin
  result := Fgravity_closure_new(vm, f);
end;

procedure TGVInterface.gravity_fiber_blacken(vm: Pgravity_vm;
  fiber: Pgravity_fiber_t);
begin
  Fgravity_fiber_blacken(vm, fiber);
end;

procedure TGVInterface.gravity_fiber_free(vm: Pgravity_vm;
  fiber: Pgravity_fiber_t);
begin
  Fgravity_fiber_free(vm, fiber);
end;

function TGVInterface.gravity_fiber_new(vm: Pgravity_vm;
  closure: Pgravity_closure_t; nstack: UInt32; nframes: UInt32)
  : Pgravity_fiber_t;
begin
  result := Fgravity_fiber_new(vm, closure, nstack, nframes);
end;

procedure TGVInterface.gravity_fiber_reassign(fiber: Pgravity_fiber_t;
  closure: Pgravity_closure_t; nargs: UInt16);
begin
  Fgravity_fiber_reassign(fiber, closure, nargs);
end;

procedure TGVInterface.gravity_fiber_seterror(fiber: Pgravity_fiber_t;
  const error: AnsiString);
begin
  Fgravity_fiber_seterror(fiber, PAnsiChar(error));
end;

function TGVInterface.gravity_fiber_size(vm: Pgravity_vm;
  fiber: Pgravity_fiber_t): UInt32;
begin
  result := Fgravity_fiber_size(vm, fiber);
end;

procedure TGVInterface.gravity_instance_blacken(vm: Pgravity_vm;
  i: Pgravity_instance_t);
begin
  Fgravity_instance_blacken(vm, i);
end;

function TGVInterface.gravity_instance_clone(vm: Pgravity_vm;
  src_instance: Pgravity_instance_t): Pgravity_instance_t;
begin
  result := Fgravity_instance_clone(vm, src_instance);
end;

procedure TGVInterface.gravity_instance_free(vm: Pgravity_vm;
  i: Pgravity_instance_t);
begin
  Fgravity_instance_free(vm, i);
end;

function TGVInterface.gravity_instance_lookup_event(i: Pgravity_instance_t;
  const name: AnsiString): Pgravity_closure_t;
begin
  result := Fgravity_instance_lookup_event(i, PAnsiChar(name));
end;

function TGVInterface.gravity_instance_new(vm: Pgravity_vm; c: Pgravity_class_t)
  : Pgravity_instance_t;
begin
  result := Fgravity_instance_new(vm, c);
end;

procedure TGVInterface.gravity_instance_setivar(instance: Pgravity_instance_t;
  idx: UInt32; value: gravity_value_t);
begin
  Fgravity_instance_setivar(instance, idx, value);
end;

procedure TGVInterface.gravity_instance_setxdata(i: Pgravity_instance_t;
  xdata: Pointer);
begin
  Fgravity_instance_setxdata(i, xdata);
end;

function TGVInterface.gravity_instance_size(vm: Pgravity_vm;
  i: Pgravity_instance_t): UInt32;
begin
  result := Fgravity_instance_size(vm, i);
end;

procedure TGVInterface.gravity_value_dump(vm: Pgravity_vm; v: gravity_value_t;
  buffer: PAnsiChar; len: UInt16);
begin
  Fgravity_value_dump(vm, v, buffer, len);
end;

function TGVInterface.gravity_value_equals(v1: gravity_value_t;
  v2: gravity_value_t): Boolean;
begin
  result := Fgravity_value_equals(v1, v2);
end;

procedure TGVInterface.gravity_value_free(vm: Pgravity_vm; v: gravity_value_t);
begin
  Fgravity_value_free(vm, v);
end;

function TGVInterface.gravity_value_from_bool(b: Boolean): gravity_value_t;
begin
  result := Fgravity_value_from_bool(b);
end;

function TGVInterface.gravity_value_from_error(msg: PAnsiChar): gravity_value_t;
begin
  result := Fgravity_value_from_error(msg);
end;

function TGVInterface.gravity_value_from_float(f: gravity_float_t)
  : gravity_value_t;
begin
  result := Fgravity_value_from_float(f);
end;

function TGVInterface.gravity_value_from_int(n: gravity_int_t): gravity_value_t;
begin
  result := Fgravity_value_from_int(n);
end;

function TGVInterface.gravity_value_from_null(): gravity_value_t;
begin
  result := Fgravity_value_from_null();
end;

function TGVInterface.gravity_value_from_object(obj: Pointer): gravity_value_t;
begin
  result := Fgravity_value_from_object(obj);
end;

function TGVInterface.gravity_value_from_undefined(): gravity_value_t;
begin
  result := Fgravity_value_from_undefined();
end;

function TGVInterface.gravity_value_getclass(v: gravity_value_t)
  : Pgravity_class_t;
begin
  result := Fgravity_value_getclass(v);
end;

function TGVInterface.gravity_value_getsuper(v: gravity_value_t)
  : Pgravity_class_t;
begin
  result := Fgravity_value_getsuper(v);
end;

function TGVInterface.gravity_value_hash(value: gravity_value_t): UInt32;
begin
  result := Fgravity_value_hash(value);
end;

function TGVInterface.gravity_value_isobject(v: gravity_value_t): Boolean;
begin
  result := Fgravity_value_isobject(v);
end;

procedure TGVInterface.gravity_value_serialize(const key: AnsiString;
  v: gravity_value_t; json: Pjson_t);
begin
  Fgravity_value_serialize(PAnsiChar(key), v, json);
end;

function TGVInterface.gravity_value_size(vm: Pgravity_vm;
  v: gravity_value_t): UInt32;
begin
  result := Fgravity_value_size(vm, v);
end;

function TGVInterface.gravity_value_xdata(value: gravity_value_t): Pointer;
begin
  result := Fgravity_value_xdata(value);
end;

procedure TGVInterface.gravity_object_blacken(vm: Pgravity_vm;
  obj: Pgravity_object_t);
begin
  Fgravity_object_blacken(vm, obj);
end;

function TGVInterface.gravity_object_debug(obj: Pgravity_object_t;
  is_free: Boolean): PAnsiChar;
begin
  result := Fgravity_object_debug(obj, is_free);
end;

function TGVInterface.gravity_object_deserialize(vm: Pgravity_vm;
  entry: Pjson_value): Pgravity_object_t;
begin
  result := Fgravity_object_deserialize(vm, entry);
end;

procedure TGVInterface.gravity_object_free(vm: Pgravity_vm;
  obj: Pgravity_object_t);
begin
  Fgravity_object_free(vm, obj);
end;

procedure TGVInterface.gravity_object_serialize(obj: Pgravity_object_t;
  json: Pjson_t);
begin
  Fgravity_object_serialize(obj, json);
end;

function TGVInterface.gravity_object_size(vm: Pgravity_vm;
  obj: Pgravity_object_t): UInt32;
begin
  result := Fgravity_object_size(vm, obj);
end;

procedure TGVInterface.gravity_list_append_list(vm: Pgravity_vm;
  list1: Pgravity_list_t; list2: Pgravity_list_t);
begin
  Fgravity_list_append_list(vm, list1, list2);
end;

procedure TGVInterface.gravity_list_blacken(vm: Pgravity_vm;
  list: Pgravity_list_t);
begin
  Fgravity_list_blacken(vm, list);
end;

procedure TGVInterface.gravity_list_free(vm: Pgravity_vm;
  list: Pgravity_list_t);
begin
  Fgravity_list_free(vm, list);
end;

function TGVInterface.gravity_list_from_array(vm: Pgravity_vm; n: UInt32;
  p: Pgravity_value_t): Pgravity_list_t;
begin
  result := Fgravity_list_from_array(vm, n, p);
end;

function TGVInterface.gravity_list_new(vm: Pgravity_vm; n: UInt32)
  : Pgravity_list_t;
begin
  result := Fgravity_list_new(vm, n);
end;

function TGVInterface.gravity_list_size(vm: Pgravity_vm;
  list: Pgravity_list_t): UInt32;
begin
  result := Fgravity_list_size(vm, list);
end;

procedure TGVInterface.gravity_map_blacken(vm: Pgravity_vm;
  map: Pgravity_map_t);
begin
  Fgravity_map_blacken(vm, map);
end;

procedure TGVInterface.gravity_map_append_map(vm: Pgravity_vm;
  map1: Pgravity_map_t; map2: Pgravity_map_t);
begin
  Fgravity_map_append_map(vm, map1, map2);
end;

procedure TGVInterface.gravity_map_free(vm: Pgravity_vm; map: Pgravity_map_t);
begin
  Fgravity_map_free(vm, map);
end;

procedure TGVInterface.gravity_map_insert(vm: Pgravity_vm; map: Pgravity_map_t;
  key: gravity_value_t; value: gravity_value_t);
begin
  Fgravity_map_insert(vm, map, key, value);
end;

function TGVInterface.gravity_map_new(vm: Pgravity_vm; n: UInt32)
  : Pgravity_map_t;
begin
  result := Fgravity_map_new(vm, n);
end;

function TGVInterface.gravity_map_size(vm: Pgravity_vm;
  map: Pgravity_map_t): UInt32;
begin
  result := Fgravity_map_size(vm, map);
end;

procedure TGVInterface.gravity_range_blacken(vm: Pgravity_vm;
  range: Pgravity_range_t);
begin
  Fgravity_range_blacken(vm, range);
end;

procedure TGVInterface.gravity_range_free(vm: Pgravity_vm;
  range: Pgravity_range_t);
begin
  Fgravity_range_free(vm, range);
end;

function TGVInterface.gravity_range_new(vm: Pgravity_vm; from: gravity_int_t;
  _to: gravity_int_t; inclusive: Boolean): Pgravity_range_t;
begin
  result := Fgravity_range_new(vm, from, _to, inclusive);
end;

function TGVInterface.gravity_range_size(vm: Pgravity_vm;
  range: Pgravity_range_t): UInt32;
begin
  result := Fgravity_range_size(vm, range);
end;

procedure TGVInterface.gravity_string_blacken(vm: Pgravity_vm;
  _string: Pgravity_string_t);
begin
  Fgravity_string_blacken(vm, _string);
end;

procedure TGVInterface.gravity_string_free(vm: Pgravity_vm;
  value: Pgravity_string_t);
begin
  Fgravity_string_free(vm, value);
end;

function TGVInterface.gravity_string_new(vm: Pgravity_vm; s: PAnsiChar;
  len: UInt32; alloc: UInt32): Pgravity_string_t;
begin
  result := Fgravity_string_new(vm, s, len, alloc);
end;

procedure TGVInterface.gravity_string_set(obj: Pgravity_string_t; s: PAnsiChar;
  len: UInt32);
begin
  Fgravity_string_set(obj, s, len);
end;

function TGVInterface.gravity_string_size(vm: Pgravity_vm;
  _string: Pgravity_string_t): UInt32;
begin
  result := Fgravity_string_size(vm, _string);
end;

function TGVInterface.gravity_string_to_value(vm: Pgravity_vm;
  const s: AnsiString; len: UInt32): gravity_value_t;
begin
  result := Fgravity_string_to_value(vm, PAnsiChar(s), len);
end;

function TGVInterface.gravity_compiler_create(delegate: Pgravity_delegate_t)
  : Pgravity_compiler_t;
begin
  result := Fgravity_compiler_create(delegate);
end;

function TGVInterface.gravity_compiler_run(compiler: Pgravity_compiler_t;
  const source: AnsiString; len: NativeUInt; fileid: UInt32; is_static: Boolean;
  add_debug: Boolean): Pgravity_closure_t;
begin
  result := Fgravity_compiler_run(compiler, PAnsiChar(source), len, fileid,
    is_static, add_debug);
end;

function TGVInterface.gravity_compiler_ast(compiler: Pgravity_compiler_t)
  : Pgnode_t;
begin
  result := Fgravity_compiler_ast(compiler);
end;

procedure TGVInterface.gravity_compiler_free(compiler: Pgravity_compiler_t);
begin
  Fgravity_compiler_free(compiler);
end;

function TGVInterface.gravity_compiler_serialize(compiler: Pgravity_compiler_t;
  closure: Pgravity_closure_t): Pjson_t;
begin
  result := Fgravity_compiler_serialize(compiler, closure);
end;

function TGVInterface.gravity_compiler_serialize_infile
  (compiler: Pgravity_compiler_t; closure: Pgravity_closure_t;
  const path: AnsiString): Boolean;
begin
  result := Fgravity_compiler_serialize_infile(compiler, closure,
    PAnsiChar(path));
end;

procedure TGVInterface.gravity_compiler_transfer(compiler: Pgravity_compiler_t;
  vm: Pgravity_vm);
begin
  Fgravity_compiler_transfer(compiler, vm);
end;

function TGVInterface.gravity_vm_delegate(vm: Pgravity_vm): Pgravity_delegate_t;
begin
  result := Fgravity_vm_delegate(vm);
end;

function TGVInterface.gravity_vm_fiber(vm: Pgravity_vm): Pgravity_fiber_t;
begin
  result := Fgravity_vm_fiber(vm);
end;

procedure TGVInterface.gravity_vm_free(vm: Pgravity_vm);
begin
  Fgravity_vm_free(vm);
end;

function TGVInterface.gravity_vm_getvalue(vm: Pgravity_vm;
  const key: AnsiString; keylen: UInt32): gravity_value_t;
begin
  result := Fgravity_vm_getvalue(vm, PAnsiChar(key), keylen);
end;

function TGVInterface.gravity_vm_keyindex(vm: Pgravity_vm; index: UInt32)
  : gravity_value_t;
begin
  result := Fgravity_vm_keyindex(vm, index);
end;

function TGVInterface.gravity_vm_ismini(vm: Pgravity_vm): Boolean;
begin
  result := Fgravity_vm_ismini(vm);
end;

function TGVInterface.gravity_vm_isaborted(vm: Pgravity_vm): Boolean;
begin
  result := Fgravity_vm_isaborted(vm);
end;

procedure TGVInterface.gravity_vm_loadclosure(vm: Pgravity_vm;
  closure: Pgravity_closure_t);
begin
  Fgravity_vm_loadclosure(vm, closure);
end;

function TGVInterface.gravity_vm_lookup(vm: Pgravity_vm; key: gravity_value_t)
  : gravity_value_t;
begin
  result := Fgravity_vm_lookup(vm, key);
end;

function TGVInterface.gravity_vm_new(delegate: Pgravity_delegate_t)
  : Pgravity_vm;
begin
  result := Fgravity_vm_new(delegate);
end;

function TGVInterface.gravity_vm_newmini(): Pgravity_vm;
begin
  result := Fgravity_vm_newmini();
end;

function TGVInterface.gravity_vm_result(vm: Pgravity_vm): gravity_value_t;
begin
  result := Fgravity_vm_result(vm);
end;

function TGVInterface.gravity_vm_runclosure(vm: Pgravity_vm;
  closure: Pgravity_closure_t; sender: gravity_value_t;
  params: Pgravity_value_t; nparams: UInt16): Boolean;
begin
  result := Fgravity_vm_runclosure(vm, closure, sender, params, nparams);
end;

function TGVInterface.gravity_vm_runmain(vm: Pgravity_vm;
  closure: Pgravity_closure_t): Boolean;
begin
  result := Fgravity_vm_runmain(vm, closure);
end;

procedure TGVInterface.gravity_vm_set_callbacks(vm: Pgravity_vm;
  vm_transfer: vm_transfer_cb; vm_cleanup: vm_cleanup_cb);
begin
  Fgravity_vm_set_callbacks(vm, vm_transfer, vm_cleanup);
end;

procedure TGVInterface.gravity_vm_setaborted(vm: Pgravity_vm);
begin
  Fgravity_vm_setaborted(vm);
end;

procedure TGVInterface.gravity_vm_seterror(vm: Pgravity_vm;
  const format: AnsiString);
begin
  Fgravity_vm_seterror(vm, PAnsiChar(format));
end;

procedure TGVInterface.gravity_vm_seterror_string(vm: Pgravity_vm;
  const s: AnsiString);
begin
  Fgravity_vm_seterror_string(vm, PAnsiChar(s));
end;

procedure TGVInterface.gravity_vm_setfiber(vm: Pgravity_vm;
  fiber: Pgravity_fiber_t);
begin
  Fgravity_vm_setfiber(vm, fiber);
end;

procedure TGVInterface.gravity_vm_setvalue(vm: Pgravity_vm;
  const key: AnsiString; value: gravity_value_t);
begin
  Fgravity_vm_setvalue(vm, PAnsiChar(key), value);
end;

function TGVInterface.gravity_vm_time(vm: Pgravity_vm): Double;
begin
  result := Fgravity_vm_time(vm);
end;

procedure TGVInterface.gravity_vm_cleanup(vm: Pgravity_vm);
begin
  Fgravity_vm_cleanup(vm);
end;

procedure TGVInterface.gravity_vm_filter(vm: Pgravity_vm;
  cleanup_filter: vm_filter_cb);
begin
  Fgravity_vm_filter(vm, cleanup_filter);
end;

procedure TGVInterface.gravity_vm_transfer(vm: Pgravity_vm;
  obj: Pgravity_object_t);
begin
  Fgravity_vm_transfer(vm, obj);
end;

procedure TGVInterface.gravity_vm_initmodule(vm: Pgravity_vm;
  f: Pgravity_function_t);
begin
  Fgravity_vm_initmodule(vm, f);
end;

function TGVInterface.gravity_vm_loadbuffer(vm: Pgravity_vm;
  const buffer: AnsiString; len: NativeUInt): Pgravity_closure_t;
begin
  result := Fgravity_vm_loadbuffer(vm, PAnsiChar(buffer), len);
end;

function TGVInterface.gravity_vm_loadfile(vm: Pgravity_vm;
  const path: AnsiString): Pgravity_closure_t;
begin
  result := Fgravity_vm_loadfile(vm, PAnsiChar(path));
end;

function TGVInterface.gravity_vm_fastlookup(vm: Pgravity_vm;
  c: Pgravity_class_t; index: Integer): Pgravity_closure_t;
begin
  result := Fgravity_vm_fastlookup(vm, c, index);
end;

function TGVInterface.gravity_vm_getdata(vm: Pgravity_vm): Pointer;
begin
  result := Fgravity_vm_getdata(vm);
end;

function TGVInterface.gravity_vm_getslot(vm: Pgravity_vm; index: UInt32)
  : gravity_value_t;
begin
  result := Fgravity_vm_getslot(vm, index);
end;

procedure TGVInterface.gravity_vm_setdata(vm: Pgravity_vm; data: Pointer);
begin
  Fgravity_vm_setdata(vm, data);
end;

procedure TGVInterface.gravity_vm_setslot(vm: Pgravity_vm;
  value: gravity_value_t; index: UInt32);
begin
  Fgravity_vm_setslot(vm, value, index);
end;

procedure TGVInterface.gravity_vm_memupdate(vm: Pgravity_vm;
  value: gravity_int_t);
begin
  Fgravity_vm_memupdate(vm, value);
end;

function TGVInterface.gravity_vm_anonymous(vm: Pgravity_vm): PAnsiChar;
begin
  result := Fgravity_vm_anonymous(vm);
end;

function TGVInterface.gravity_vm_get(vm: Pgravity_vm; const key: AnsiString)
  : gravity_value_t;
begin
  result := Fgravity_vm_get(vm, PAnsiChar(key));
end;

function TGVInterface.gravity_vm_set(vm: Pgravity_vm; const key: AnsiString;
  value: gravity_value_t): Boolean;
begin
  result := Fgravity_vm_set(vm, PAnsiChar(key), value);
end;

procedure TGVInterface.gravity_gray_object(vm: Pgravity_vm;
  obj: Pgravity_object_t);
begin
  Fgravity_gray_object(vm, obj);
end;

procedure TGVInterface.gravity_gray_value(vm: Pgravity_vm; v: gravity_value_t);
begin
  Fgravity_gray_value(vm, v);
end;

procedure TGVInterface.gravity_gc_setenabled(vm: Pgravity_vm; enabled: Boolean);
begin
  Fgravity_gc_setenabled(vm, enabled);
end;

procedure TGVInterface.gravity_gc_start(vm: Pgravity_vm);
begin
  Fgravity_gc_start(vm);
end;

procedure TGVInterface.gravity_gc_temppop(vm: Pgravity_vm);
begin
  Fgravity_gc_temppop(vm);
end;

procedure TGVInterface.gravity_gc_temppush(vm: Pgravity_vm;
  obj: Pgravity_object_t);
begin
  Fgravity_gc_temppush(vm, obj);
end;

function TGVInterface.gravity_core_class_from_name(const name: AnsiString)
  : Pgravity_class_t;
begin
  result := Fgravity_core_class_from_name(PAnsiChar(name));
end;

procedure TGVInterface.gravity_core_free();
begin
  Fgravity_core_free();
  ResetClassReferences;
end;

function TGVInterface.gravity_core_identifiers(): PPAnsiChar;
begin
  result := Fgravity_core_identifiers();
end;

procedure TGVInterface.gravity_core_register(vm: Pgravity_vm);
begin
  Fgravity_core_register(vm);
end;

function TGVInterface.gravity_iscore_class(c: Pgravity_class_t): Boolean;
begin
  result := Fgravity_iscore_class(c);
end;

function TGVInterface.computed_property_create(vm: Pgravity_vm;
  getter_func: Pgravity_function_t; setter_func: Pgravity_function_t)
  : Pgravity_closure_t;
begin
  result := Fcomputed_property_create(vm, getter_func, setter_func);
end;

procedure TGVInterface.computed_property_free(c: Pgravity_class_t;
  const name: AnsiString; remove_flag: Boolean);
begin
  Fcomputed_property_free(c, PAnsiChar(name), remove_flag);
end;

function TGVInterface.convert_value2bool(vm: Pgravity_vm; v: gravity_value_t)
  : gravity_value_t;
begin
  result := Fconvert_value2bool(vm, v);
end;

function TGVInterface.convert_value2float(vm: Pgravity_vm; v: gravity_value_t)
  : gravity_value_t;
begin
  result := Fconvert_value2float(vm, v);
end;

function TGVInterface.convert_value2int(vm: Pgravity_vm; v: gravity_value_t)
  : gravity_value_t;
begin
  result := Fconvert_value2int(vm, v);
end;

function TGVInterface.convert_value2string(vm: Pgravity_vm; v: gravity_value_t)
  : gravity_value_t;
begin
  result := Fconvert_value2string(vm, v);
end;

function TGVInterface.gravity_hash_create(size: UInt32;
  compute: gravity_hash_compute_fn; isequal: gravity_hash_isequal_fn;
  free: gravity_hash_iterate_fn; data: Pointer): Pgravity_hash_t;
begin
  result := Fgravity_hash_create(size, compute, isequal, free, data);
end;

procedure TGVInterface.gravity_hash_free(hashtable: Pgravity_hash_t);
begin
  Fgravity_hash_free(hashtable);
end;

function TGVInterface.gravity_hash_insert(hashtable: Pgravity_hash_t;
  key: gravity_value_t; value: gravity_value_t): Boolean;
begin
  result := Fgravity_hash_insert(hashtable, key, value);
end;

function TGVInterface.gravity_hash_isempty(hashtable: Pgravity_hash_t): Boolean;
begin
  result := Fgravity_hash_isempty(hashtable);
end;

function TGVInterface.gravity_hash_lookup(hashtable: Pgravity_hash_t;
  key: gravity_value_t): Pgravity_value_t;
begin
  result := Fgravity_hash_lookup(hashtable, key);
end;

function TGVInterface.gravity_hash_remove(hashtable: Pgravity_hash_t;
  key: gravity_value_t): Boolean;
begin
  result := Fgravity_hash_remove(hashtable, key);
end;

procedure TGVInterface.gravity_hash_append(hashtable1: Pgravity_hash_t;
  hashtable2: Pgravity_hash_t);
begin
  Fgravity_hash_append(hashtable1, hashtable2);
end;

function TGVInterface.gravity_hash_compute_buffer(const key: AnsiString;
  len: UInt32): UInt32;
begin
  result := Fgravity_hash_compute_buffer(PAnsiChar(key), len);
end;

function TGVInterface.gravity_hash_compute_float(f: gravity_float_t): UInt32;
begin
  result := Fgravity_hash_compute_float(f);
end;

function TGVInterface.gravity_hash_compute_int(n: gravity_int_t): UInt32;
begin
  result := Fgravity_hash_compute_int(n);
end;

function TGVInterface.gravity_hash_count(hashtable: Pgravity_hash_t): UInt32;
begin
  result := Fgravity_hash_count(hashtable);
end;

procedure TGVInterface.gravity_hash_dump(hashtable: Pgravity_hash_t);
begin
  Fgravity_hash_dump(hashtable);
end;

procedure TGVInterface.gravity_hash_iterate(hashtable: Pgravity_hash_t;
  iterate: gravity_hash_iterate_fn; data: Pointer);
begin
  Fgravity_hash_iterate(hashtable, iterate, data);
end;

procedure TGVInterface.gravity_hash_iterate2(hashtable: Pgravity_hash_t;
  iterate: gravity_hash_iterate2_fn; data1: Pointer; data2: Pointer);
begin
  Fgravity_hash_iterate2(hashtable, iterate, data1, data2);
end;

function TGVInterface.gravity_hash_memsize(hashtable: Pgravity_hash_t): UInt32;
begin
  result := Fgravity_hash_memsize(hashtable);
end;

procedure TGVInterface.gravity_hash_resetfree(hashtable: Pgravity_hash_t);
begin
  Fgravity_hash_resetfree(hashtable);
end;

procedure TGVInterface.gravity_hash_stat(hashtable: Pgravity_hash_t);
begin
  Fgravity_hash_stat(hashtable);
end;

procedure TGVInterface.gravity_hash_transform(hashtable: Pgravity_hash_t;
  iterate: gravity_hash_transform_fn; data: Pointer);
begin
  Fgravity_hash_transform(hashtable, iterate, data);
end;

procedure TGVInterface.gravity_hash_keyfree(table: Pgravity_hash_t;
  key: gravity_value_t; value: gravity_value_t; data: Pointer);
begin
  Fgravity_hash_keyfree(table, key, value, data);
end;

procedure TGVInterface.gravity_hash_keyvaluefree(table: Pgravity_hash_t;
  key: gravity_value_t; value: gravity_value_t; data: Pointer);
begin
  Fgravity_hash_keyvaluefree(table, key, value, data);
end;

procedure TGVInterface.gravity_hash_valuefree(table: Pgravity_hash_t;
  key: gravity_value_t; value: gravity_value_t; data: Pointer);
begin
  Fgravity_hash_valuefree(table, key, value, data);
end;

class function TGVInterface.DefaultInstance: IGVInterface;
begin
  if not Assigned(DefaultIntf) then
    if Assigned(FRegisterGVInterface) then
      DefaultIntf := FRegisterGVInterface
    else
      DefaultIntf := TGVInterface.Create;
  result := DefaultIntf;
end;

procedure RegisterGVInterfaceFactory(ARegisterGVIntf: TRegisterGVInterface);
begin
  FRegisterGVInterface := ARegisterGVIntf;
end;

function GravityEng: IGVInterface;
begin
  result := TGVInterface.DefaultInstance;
end;

initialization

TGVInterface.DefaultIntf := nil;

finalization

TGVInterface.DefaultIntf := nil;

end.
