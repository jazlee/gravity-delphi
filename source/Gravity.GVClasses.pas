unit Gravity.GVClasses;

interface

uses
  System.SysUtils, System.Classes, Winapi.Windows, Gravity.GVIntf;

type
  TGravityClassItem = class;

  TGravityWrapperBase = class
  private
    FGravityInstance: Pgravity_instance_t;
  protected
    class function RegisterClass(vm: Pgravity_vm; var ClassItem: TGravityClassItem): boolean; virtual;
    class function UnregisterClass(vm: Pgravity_vm; var ClassItem: TGravityClassItem): boolean; virtual;

  public
    constructor Create;
    destructor Destroy; override;
  end;

  TGravityWrapperBaseClass = class of TGravityWrapperBase;

  TGravityClassItem = class(TCollectionItem)
  private
    FGravityClass: Pgravity_class_t;
    FNativeClass: TGravityWrapperBaseClass;
  protected

    function GetDisplayName: string; override;

    procedure RegisterClass(vm: Pgravity_vm);
    procedure UnregisterClass(vm: Pgravity_vm);
  public

    procedure SetNativeClass(AClass: TGravityWrapperBaseClass);
    procedure SetGravityClass(AClass: Pgravity_class_t);

    property NativeClass: TGravityWrapperBaseClass read FNativeClass;
    property GravityClass: Pgravity_class_t read FGravityClass;
  end;

  TGravityModuleProc = procedure(VM: Pgravity_vm);
  TGravityDelegateProc = procedure(vm: Pgravity_vm);

function GravityCall(AVm: Pgravity_vm; AFunc, ASender: gravity_value_t;
  Args: array of const): Variant;



procedure BindNativeModules(AVm: Pgravity_vm);
procedure UnbindNativeModules(AVm: Pgravity_vm);
procedure RegisterGravityClass(AClass: TGravityWrapperBaseClass);
procedure RegisterGravityClasses(AClasses: array of TGravityWrapperBaseClass);
procedure RegisterGravityName(const AVarName: String);
procedure UnRegisterGravityClass(AClass: TGravityWrapperBaseClass);
procedure UnRegisterGravityClasses(AClasses: array of TGravityWrapperBaseClass);
procedure RegisterGravityModule(ARegProc, AUnregProc: TGravityModuleProc);
procedure RegisterGravityDelegate(ADelegateProc: TGravityDelegateProc);
function FindNativeClass(const ClassName: string): TGravityWrapperBaseClass;
function GetNativeClass(const AClassName: string): TGravityWrapperBaseClass;
function GetGravityClass(const AClassName: string): Pgravity_class_t;
procedure DefineNativeObject(AVm: Pgravity_vm; const AVarName: String;
  AObj: TGravityWrapperBase);

procedure host_bridge_hash_iterate(hashtable: Pgravity_hash_t;
  key, value: gravity_value_t; data: pointer); cdecl;

implementation

uses
  System.Generics.Defaults,
  System.Generics.Collections,
  System.Rtti,
  System.TypInfo,
  System.RTLConsts,
  System.AnsiStrings;

type
  TAnsiStringItemList = array of AnsiString;
  TAnsiCharItemList = array of PAnsiChar;

  TAnsiStrings = class(TPersistent)
  private
    FList: TAnsiStringItemList;
    FPtr: TAnsiCharItemList;
    FCount: Integer;
    FCapacity: Integer;

    function GetCount: Integer;
    procedure Grow;
  protected
    procedure Error(Msg: PResStringRec; data: Integer);
    procedure SetCapacity(NewCapacity: Integer);

    function CompareStrings(const S1, S2: AnsiString): Integer; virtual;
    function Get(Index: Integer): AnsiString; virtual;
    procedure InsertItem(Index: Integer; const S: AnsiString);
  public
    constructor Create;
    destructor Destroy; override;

    function Add(const S: AnsiString): Integer; virtual;
    function IndexOf(const S: AnsiString): Integer; virtual;
    procedure Insert(Index: Integer; const S: AnsiString);
    procedure Append(const S: AnsiString);
    procedure Clear;
    procedure Delete(Index: Integer);

    property Count: Integer read GetCount;
    property Strings[Index: Integer]: AnsiString read Get; default;
  end;

  TGravityClassList = class(TCollection)
  public
    constructor Create;

    function IndexOf(Item: pointer): Integer;
    function CollectionNames: TArray<String>;
  end;

  TGravityModuleItem = class(TCollectionItem)
  private
    FRegisterProc: TGravityModuleProc;
    FUnregisterProc: TGravityModuleProc;

  public
    property RegisterProc: TGravityModuleProc read FRegisterProc;
    property UnregisterProc: TGravityModuleProc read FUnregisterProc;
  end;

  TGravityModuleList = class(TCollection)
  public
    constructor Create;
  end;

  TGravityFuncType = (ftMethod = 0, ftPropGetter, ftPropSetter,
    ftFieldGetter, ftFieldSetter);
  PNativeFunctionRecord = ^TNativeFunctionRecord;
  TNativeFunctionRecord = record
    FuncType: TGravityFuncType;
    FuncName: PAnsiChar;
  end;

function host_bridge_initinstance(vm: Pgravity_vm; xdata: pointer;
  ctx: gravity_value_t; instance: Pgravity_instance_t; Args: Pgravity_value_t;
  NArgs: Int16): Boolean; cdecl;
begin
  with GravityEng do
  begin
    Result := True;
  end;
end;

procedure host_bridge_free_instance(vm: Pgravity_vm;
  instance: Pgravity_instance_t); cdecl;
var
  AObjInstance: TObject;
begin
  if Assigned(instance.xdata) then  
  with GravityEng do
  begin
    AObjInstance := TObject(instance.xdata);
    if Assigned(AObjInstance) then
    begin
      if AObjInstance.InheritsFrom(TGravityWrapperBase) then
        TGravityWrapperBase(AObjInstance).FGravityInstance := nil;
      AObjInstance.Free;
    end;
    instance.xdata := nil;
  end;
end;

procedure host_bridge_free_func(bridge_var: Pointer); cdecl;
var
  AFunc: PNativeFunctionRecord;
begin
  if Assigned(bridge_var) then
  with GravityEng do
  begin
    AFunc := PNativeFunctionRecord(bridge_var);
    FreeMem(AFunc^.FuncName);
    FreeMem(AFunc);
  end;
end;

procedure host_bridge_free_closure(vm: Pgravity_vm;
  closure: Pgravity_closure_t); cdecl;
var
  AGetter, ASetter: Pgravity_closure_t;
begin
  if Assigned(closure) then  
  with GravityEng do
  begin
    if closure^.f^.tag = EXEC_TYPE_SPECIAL then
    begin
      if closure^.f^.f10.f3.index = GRAVITY_BRIDGE_INDEX then
      begin
        if Assigned(closure^.f^.xdata) then
          host_bridge_free_func(closure^.f^.xdata);
        closure^.f^.xdata := nil;
        AGetter := closure^.f^.f10.f3.special[0];
        closure^.f^.f10.f3.special[0] := nil;
        ASetter := nil;
        if (closure^.f^.f10.f3.special[0] <> closure^.f^.f10.f3.special[1]) then
        begin
          ASetter := closure^.f^.f10.f3.special[1];
          closure^.f^.f10.f3.special[1] := nil;
        end;
        host_bridge_free_closure(vm, AGetter);
        host_bridge_free_closure(vm, ASetter);
        gravity_closure_free(nil, AGetter);
        gravity_closure_free(nil, ASetter);
      end;
    end
    else if closure^.f^.tag = EXEC_TYPE_BRIDGED then
    begin
      host_bridge_free_func(closure^.f^.xdata);
      closure^.f^.xdata := nil;
    end;
    if closure^.f^.xdata <> nil then
    begin
      host_bridge_free_func(closure^.f^.xdata);
      closure^.f^.xdata := nil;
    end;
    gravity_function_free(nil, closure^.f);
    closure^.f := nil;
  end;
end;

procedure host_bridge_free_class(vm: Pgravity_vm;
  klass: Pgravity_class_t); cdecl;
var
  AMeta: Pgravity_class_t;
begin
  if Assigned(klass^.xdata) then
    with GravityEng do
    begin
      klass^.xdata := nil;
      AMeta := gravity_class_get_meta(klass);
      gravity_hash_iterate(AMeta^.htable, host_bridge_hash_iterate, vm);
      gravity_hash_iterate(klass^.htable, host_bridge_hash_iterate, vm);
      gravity_class_free(nil, AMeta);
    end;
end;

procedure host_bridge_free(vm: Pgravity_vm; obj: Pgravity_object_t); cdecl;
begin
  with GravityEng do
  begin
    if OBJECT_ISA_INSTANCE(obj) then
      host_bridge_free_instance(vm, Pgravity_instance_t(obj))
    else if OBJECT_ISA_CLOSURE(obj) then
      host_bridge_free_closure(vm, Pgravity_closure_t(obj))
    else if OBJECT_ISA_CLASS(obj) then
      host_bridge_free_class(vm, Pgravity_class_t(obj));
  end;
end;

function host_bridge_execute(vm: Pgravity_vm; xdata: pointer;
  ctx: gravity_value_t; Args: Pgravity_value_t; NArgs: Int16; vindex: UInt32)
  : Boolean; cdecl;
var
  AFuncRec: PNativeFunctionRecord;
  AValue: gravity_value_t;
  AInstVal: Pgravity_instance_t;
  AObjInstance: TGravityWrapperBase;
  ARetValue: TValue;

  AParams: array of TValue;
  I: Integer;
  LContext: TRttiContext;
  LType: TRttiType;
  LMethod: TRttiMethod;
  LParams: TArray<TRttiParameter>;
  AStr: AnsiString;
begin
  with GravityEng do
  begin
    Result := RETURN_NOVALUE;
    AFuncRec := PNativeFunctionRecord(xdata);
    if AFuncRec <> nil then
    begin
      gravity_gc_setenabled(vm, False);
      AValue := GET_VALUE(Args, 0);
      if VALUE_ISA_INSTANCE(AValue) then
      begin
        AObjInstance := TGravityWrapperBase(gravity_value_xdata(AValue));
        LContext := TRttiContext.Create;
        try
          LType := LContext.GetType(AObjInstance.ClassType);
          LMethod := LType.GetMethod(String(AnsiString(AFuncRec^.FuncName)));
          if (AObjInstance <> nil) and (LMethod <> nil) and
            AObjInstance.InheritsFrom(TGravityWrapperBase) then
          begin
            if NArgs > 1 then
            begin
              SetLength(AParams, NArgs - 1);
              for I := 1 to NArgs - 1 do
              begin
                AValue := GET_VALUE(Args, I);
                if VALUE_ISA_BASIC_TYPE(AValue) then
                begin
                  if VALUE_ISA_INT(AValue) then
                    AParams[I - 1] := TValue.From(VALUE_AS_INT(AValue))
                  else if VALUE_ISA_FLOAT(AValue) then
                    AParams[I - 1] := TValue.From(VALUE_AS_FLOAT(AValue))
                  else if VALUE_ISA_STRING(AValue) then
                    AParams[I - 1] :=
                      TValue.From(String(AnsiString(VALUE_AS_CSTRING(AValue))))
                  else if VALUE_ISA_BOOL(AValue) then
                    AParams[I - 1] := TValue.From(VALUE_AS_BOOL(AValue))
                end
                else
                begin
                  LParams := LMethod.GetParameters;
                  if VALUE_ISA_INSTANCE(AValue) then
                  begin
                    if Assigned(LParams[I - 1].ParamType) then
                    begin
                      AInstVal := VALUE_AS_INSTANCE(AValue);
                      if (AInstVal.xdata <> nil) and
                        (TObject(AInstVal).InheritsFrom(TGravityWrapperBase))
                      then
                        AParams[I - 1] := TValue.From(TObject(AInstVal))
                    end;
                  end;
                end;
              end;
              try
                ARetValue := LMethod.Invoke(AObjInstance, AParams);
                if not ARetValue.IsEmpty then
                begin
                  gravity_gc_setenabled(vm, True);
                  if ARetValue.Kind = tkInteger then
                    Result := RETURN_VALUE(vm, VALUE_FROM_INT(ARetValue.AsInteger), vindex)
                  else if ARetValue.Kind = tkInt64 then
                    Result := RETURN_VALUE(vm, VALUE_FROM_INT(ARetValue.AsInt64), vindex)
                  else if ARetValue.Kind = tkFloat then
                    Result := RETURN_VALUE(vm,
                      VALUE_FROM_FLOAT(ARetValue.AsExtended), vindex)
                  else if (ARetValue.Kind = tkString) or (ARetValue.Kind = tkChar)
                    or (ARetValue.Kind = tkWString) or (ARetValue.Kind = tkWChar)
                    or (ARetValue.Kind = tkUString) then
                  begin
                    AStr := AnsiString(ARetValue.AsString);
                    Result := RETURN_VALUE(vm, VALUE_FROM_STRING(vm, AStr), vindex)
                  end;
                end;
              except
                on E:Exception do
                  begin
                    Result := RETURN_ERROR(vm, AnsiString(E.Message), vindex);
                  end;
              end;
            end;
          end;
        finally
          gravity_gc_setenabled(vm, True);
          LContext.Free;
        end;
      end;
    end else
      Result := RETURN_ERROR(vm, 'Unable to process bridge request', vindex)
  end;
end;

function host_bridge_getvalue(vm: Pgravity_vm; xdata: pointer;
  target: gravity_value_t; const key: PAnsiChar; vindex: UInt32)
  : Boolean; cdecl;
var
  AFuncRec: PNativeFunctionRecord;
  AObjInstance: TGravityWrapperBase;
  ARetValue: TValue;

  LContext: TRttiContext;
  LType: TRttiType;
  LProp: TRttiProperty;
  LField: TRttiField;
  AStr: AnsiString;
begin
  with GravityEng do
  begin
    Result := RETURN_NOVALUE;
    AFuncRec := PNativeFunctionRecord(xdata);
    if VALUE_ISA_INSTANCE(target) and (AFuncRec <> nil)then
    begin
      AObjInstance:= TGravityWrapperBase(gravity_value_xdata(target));
      LContext := TRttiContext.Create;
      try
        gravity_gc_setenabled(vm, False);
        LType := LContext.GetType(AObjInstance.ClassType);
        if AFuncRec.FuncType = ftPropGetter then
        begin
          LProp := LType.GetProperty(String(AnsiString(AFuncRec^.FuncName)));
          if (AObjInstance <> nil) and (LProp <> nil) and
            AObjInstance.InheritsFrom(TGravityWrapperBase) then
          try
            ARetValue := LProp.GetValue(AObjInstance);
            gravity_gc_setenabled(vm, True);
            if not ARetValue.IsEmpty then
            begin
              if ARetValue.Kind = tkInteger then
                Result := RETURN_VALUE(vm, VALUE_FROM_INT(ARetValue.AsInteger), vindex)
              else if ARetValue.Kind = tkInt64 then
                Result := RETURN_VALUE(vm, VALUE_FROM_INT(ARetValue.AsInt64), vindex)
              else if ARetValue.Kind = tkFloat then
                Result := RETURN_VALUE(vm,
                  VALUE_FROM_FLOAT(ARetValue.AsExtended), vindex)
              else if (ARetValue.Kind = tkString) or (ARetValue.Kind = tkChar)
                or (ARetValue.Kind = tkWString) or (ARetValue.Kind = tkWChar)
                or (ARetValue.Kind = tkUString) then
              begin
                AStr := AnsiString(ARetValue.AsString);
                Result := RETURN_VALUE(vm, VALUE_FROM_STRING(vm, AStr), vindex)
              end;
            end else
              Result := RETURN_VALUE(vm, VALUE_FROM_NULL, vindex);
          except
            on E:Exception do
              begin
                Result := RETURN_ERROR(vm, AnsiString(E.Message), vindex);
              end;
          end;
        end else
        begin
          LField := LType.GetField(String(AnsiString(AFuncRec^.FuncName)));
          if (AObjInstance <> nil) and (LField <> nil) and
            AObjInstance.InheritsFrom(TGravityWrapperBase) then
          try
            ARetValue := LField.GetValue(AObjInstance);
            gravity_gc_setenabled(vm, True);
            if not ARetValue.IsEmpty then
            begin
              if ARetValue.Kind = tkInteger then
                Result := RETURN_VALUE(vm, VALUE_FROM_INT(ARetValue.AsInteger), vindex)
              else if ARetValue.Kind = tkInt64 then
                Result := RETURN_VALUE(vm, VALUE_FROM_INT(ARetValue.AsInt64), vindex)
              else if ARetValue.Kind = tkFloat then
                Result := RETURN_VALUE(vm,
                  VALUE_FROM_FLOAT(ARetValue.AsExtended), vindex)
              else if (ARetValue.Kind = tkString) or (ARetValue.Kind = tkChar)
                or (ARetValue.Kind = tkWString) or (ARetValue.Kind = tkWChar)
                or (ARetValue.Kind = tkUString) then
              begin
                AStr := AnsiString(ARetValue.AsString);
                Result := RETURN_VALUE(vm, VALUE_FROM_STRING(vm, AStr), vindex)
              end;
            end else
              Result := RETURN_VALUE(vm, VALUE_FROM_NULL, vindex);
          except
            on E:Exception do
              begin
                Result := RETURN_ERROR(vm, AnsiString(E.Message), vindex);
              end;
          end;
        end;
      finally
        gravity_gc_setenabled(vm, True);
        LContext.Free;
      end;
    end else
      Result := RETURN_ERROR(vm, 'Unable to process bridge request', vindex)
  end;
end;

function host_bridge_setvalue(vm: Pgravity_vm; xdata: pointer;
  target: gravity_value_t; const key: PAnsiChar; value: gravity_value_t)
  : Boolean; cdecl;
var
  AObjInstance: TGravityWrapperBase;
  AFuncRec: PNativeFunctionRecord;
  AValue: TValue;
  LContext: TRttiContext;
  LType: TRttiType;
  LProp: TRttiProperty;
  LField: TRttiField;
  AInstVal: Pgravity_instance_t;
begin
  LContext := TRttiContext.Create;
  with GravityEng do
  try
    gravity_gc_setenabled(vm, False);
    Result := RETURN_NOVALUE;
    AObjInstance:= TGravityWrapperBase(gravity_value_xdata(target));
    AFuncRec := PNativeFunctionRecord(xdata);
    if VALUE_ISA_INSTANCE(target) and (AFuncRec <> nil)then
    begin
      LType := LContext.GetType(AObjInstance.ClassType);
      if AFuncRec^.FuncType = ftPropSetter then
      begin
        LProp := LType.GetProperty(String(AnsiString(AFuncRec^.FuncName)));
        if VALUE_ISA_BASIC_TYPE(value) then
        begin
          if VALUE_ISA_INT(value) then
            AValue := TValue.From(VALUE_AS_INT(value))
          else if VALUE_ISA_FLOAT(value) then
            AValue := TValue.From(VALUE_AS_FLOAT(value))
          else if VALUE_ISA_STRING(value) then
            AValue := TValue.From(String(AnsiString(VALUE_AS_CSTRING(value))))
          else if VALUE_ISA_BOOL(value) then
            AValue := TValue.From(VALUE_AS_BOOL(value))
        end else
        begin
          if VALUE_ISA_INSTANCE(value) then
          begin
            AInstVal := VALUE_AS_INSTANCE(value);
            if (AInstVal.xdata <> nil) and
              (TObject(AInstVal).InheritsFrom(TGravityWrapperBase))
            then
              AValue := TValue.From(TObject(AInstVal))
          end;
        end;
        try
          LProp.SetValue(AObjInstance, AValue);
        except
          on E:Exception do
            begin
              Result := RETURN_NOVALUE;
            end;
        end;
      end else
      begin
        LField := LType.GetField(String(AnsiString(AFuncRec^.FuncName)));
        if VALUE_ISA_BASIC_TYPE(value) then
        begin
          if VALUE_ISA_INT(value) then
            AValue := TValue.From(VALUE_AS_INT(value))
          else if VALUE_ISA_FLOAT(value) then
            AValue := TValue.From(VALUE_AS_FLOAT(value))
          else if VALUE_ISA_STRING(value) then
            AValue := TValue.From(String(AnsiString(VALUE_AS_CSTRING(value))))
          else if VALUE_ISA_BOOL(value) then
            AValue := TValue.From(VALUE_AS_BOOL(value))
        end else
        begin
          if VALUE_ISA_INSTANCE(value) then
          begin
            AInstVal := VALUE_AS_INSTANCE(value);
            if (AInstVal.xdata <> nil) and
              (TObject(AInstVal).InheritsFrom(TGravityWrapperBase))
            then
              AValue := TValue.From(TObject(AInstVal))
          end;
        end;
        try
          LField.SetValue(AObjInstance, AValue);
        except
          on E:Exception do
            begin
              Result := RETURN_NOVALUE;
            end;
        end;
      end;
    end;
  finally
    gravity_gc_setenabled(vm, True);
    LContext.Free;
  end;
end;

function host_bridge_getundef(vm: Pgravity_vm; xdata: pointer;
  target: gravity_value_t; const key: PAnsiChar; vindex: UInt32)
  : Boolean; cdecl;
begin
  with GravityEng do
  begin
    Result := RETURN_NOVALUE;
  end;
end;

function host_bridge_setundef(vm: Pgravity_vm; xdata: pointer;
  target: gravity_value_t; const key: PAnsiChar; value: gravity_value_t)
  : Boolean; cdecl;
begin
  with GravityEng do
  begin
    Result := RETURN_NOVALUE;
  end;
end;

function host_bridge_clone(vm: Pgravity_vm; xdata: pointer): pointer; cdecl;
begin
  with GravityEng do
  begin
    Result := nil;
  end;
end;

function host_bridge_equals(vm: Pgravity_vm; obj1: pointer; obj2: pointer)
  : Boolean; cdecl;
begin
  with GravityEng do
  begin
    Result := false;
  end;
end;

function host_bridge_size(vm: Pgravity_vm; obj: Pgravity_object_t)
  : UInt32; cdecl;
begin
  with GravityEng do
  begin
    Result := 0;
  end;
end;

function host_bridge_string(vm: Pgravity_vm; xdata: pointer; len: PUInt32)
  : PAnsiChar; cdecl;
begin
  with GravityEng do
  begin
    Result := nil;
  end;
end;

procedure host_bridge_blacken(vm: Pgravity_vm; xdata: pointer); cdecl;
begin
  with GravityEng do
  begin
  end;
end;

procedure host_bridge_hash_iterate(hashtable: Pgravity_hash_t;
  key, value: gravity_value_t; data: pointer);
begin
  with GravityEng do
    if gravity_value_isobject(value) then
    begin
      host_bridge_free(data, VALUE_AS_OBJECT(value));
      gravity_object_free(nil, VALUE_AS_OBJECT(value));
    end;
end;

{ TAnsiStrings }

function TAnsiStrings.Add(const S: AnsiString): Integer;
begin
  Result := GetCount;
  Insert(Result, S);
end;

procedure TAnsiStrings.Append(const S: AnsiString);
begin
  Add(S);
end;

procedure TAnsiStrings.Clear;
begin
  if FCount <> 0 then
  begin
    FCount := 0;
    SetCapacity(0);
  end;
end;

function TAnsiStrings.CompareStrings(const S1, S2: AnsiString): Integer;
begin
  Result := AnsiCompareText(S1, S2);
end;

constructor TAnsiStrings.Create;
begin
  FPtr := nil;
end;

procedure TAnsiStrings.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Finalize(FList[Index]);
  Dec(FCount);
  if Index < FCount then
  begin
    System.Move(FList[Index + 1], FList[Index],
      (FCount - Index) * SizeOf(AnsiString));
    PPointer(@FList[FCount])^ := nil;
  end;
end;

destructor TAnsiStrings.Destroy;
begin
  inherited Destroy;
  FCount := 0;
  SetCapacity(0);
end;

procedure TAnsiStrings.Error(Msg: PResStringRec; data: Integer);
begin
  raise EStringListError.CreateFmt(LoadResString(Msg), [data])
    at PPointer(PByte(@Msg) + SizeOf(Msg) + SizeOf(Self) + SizeOf(pointer))^;
end;

function TAnsiStrings.Get(Index: Integer): AnsiString;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Result := FList[Index];
end;

function TAnsiStrings.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TAnsiStrings.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else if FCapacity > 8 then
    Delta := 16
  else
    Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TAnsiStrings.IndexOf(const S: AnsiString): Integer;
begin
  for Result := 0 to GetCount - 1 do
    if CompareStrings(Get(Result), S) = 0 then
      Exit;
  Result := -1;
end;

procedure TAnsiStrings.Insert(Index: Integer; const S: AnsiString);
begin
  if (Index < 0) or (Index > FCount) then
    Error(@SListIndexError, Index);
  InsertItem(Index, S);
end;

procedure TAnsiStrings.InsertItem(Index: Integer; const S: AnsiString);
begin
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
  begin
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(AnsiString));
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(PAnsiChar));
  end;
  pointer(FList[Index]) := nil;
  pointer(FPtr[Index]) := nil;
  FList[Index] := S;
  FPtr[Index] := PAnsiChar(FList[Index]);
  Inc(FCount);
end;

procedure TAnsiStrings.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    SetLength(FPtr, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

type
  TGravityGetClass = procedure(AClass: TGravityWrapperBaseClass) of object;

  TGravityClassReg = class
  private
    FWrapperList: TGravityClassList;
    FWrapperGroupClasses: TList;
    FActive: Boolean;
    function BestClass(AClass: TGravityWrapperBaseClass)
      : TGravityWrapperBaseClass;
    function GetClassNames: TArray<String>;
  public
    constructor Create(AClass: TGravityWrapperBaseClass);
    destructor Destroy; override;

    class function BestGroup(Group1, Group2: TGravityClassReg;
      AClass: TGravityWrapperBaseClass): TGravityClassReg;

    function GetClass(const AClassName: string): TGravityWrapperBaseClass;
    function GetGravityClass(const AClassName: string): Pgravity_class_t;
    procedure GetClasses(Proc: TGravityGetClass);
    procedure RegisterClass(AClass: TGravityWrapperBaseClass);
    function Registered(AClass: TGravityWrapperBaseClass): Boolean;
    procedure UnregisterClass(AClass: TGravityWrapperBaseClass);
    property Active: Boolean read FActive write FActive;
    property ClassNames: TArray<String> read GetClassNames;
    property WrapperList: TGravityClassList read FWrapperList;
  end;

  TGravityClassRegs = class
  private
    FModules: TGravityModuleList;
    FNameList: TAnsiStrings;
    FWrapperGroups: TList;
    FLock: TRTLCriticalSection;
    FDelegates: TList;
    function FindGroup(AClass: TGravityWrapperBaseClass): TGravityClassReg;
    function GetName(I: Integer): string;
  public
    constructor Create;
    destructor Destroy; override;
    function GetClass(const AClassName: string): TGravityWrapperBaseClass;
    function GetGravityClass(const AClassName: string): Pgravity_class_t;
    procedure Lock;
    procedure RegisterClass(AClass: TGravityWrapperBaseClass);
    procedure RegisterName(AName: String);
    function Registered(AClass: TGravityWrapperBaseClass): Boolean;
    procedure Unlock;
    procedure UnregisterClass(AClass: TGravityWrapperBaseClass);

    property Names[I: Integer]: string read GetName;
    property Modules: TGravityModuleList read FModules;
    property Delegates: TList read FDelegates;
    property WrapperGroups: TList read FWrapperGroups;
  end;

var
  GravityRegGroups: TGravityClassRegs;

function native_classes_callback(xdata: pointer): PPAnsiChar; cdecl;
var
  APtr: TAnsiCharItemList;
begin
  APtr := GravityRegGroups.FNameList.FPtr;
  Result := @APtr[0];
end;

procedure gravity_register_bridges(vm: Pgravity_vm);
var
  ADlgt: Pgravity_delegate_t;

begin
  with GravityEng do
  begin
    ADlgt := gravity_vm_delegate(vm);
    ADlgt^.optional_classes := native_classes_callback;
    ADlgt^.bridge_initinstance := host_bridge_initinstance;
    ADlgt^.bridge_getvalue := host_bridge_getvalue;
    ADlgt^.bridge_setvalue := host_bridge_setvalue;
    ADlgt^.bridge_execute := host_bridge_execute;
    // ...
    // ADlgt^.bridge_getundef := host_bridge_getundef;
    // ADlgt^.bridge_setundef := host_bridge_setundef;
    // ADlgt^.bridge_blacken := host_bridge_blacken;
    // ADlgt^.bridge_string := host_bridge_string;
    // ADlgt^.bridge_equals := host_bridge_equals;
    // ADlgt^.bridge_clone := host_bridge_clone;
    // ...
    ADlgt^.bridge_size := host_bridge_size;
    ADlgt^.bridge_free := host_bridge_free;
  end;
end;

{ TGravityClassList }
function TGravityClassList.CollectionNames: TArray<String>;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := Items[I].DisplayName;
end;

constructor TGravityClassList.Create;
begin
  inherited Create(TGravityClassItem);
end;

function TGravityClassList.IndexOf(Item: pointer): Integer;
begin
  for Result := 0 to Count - 1 do
    if TGravityClassItem(Items[Result]).NativeClass = Item then
      Exit;
  Result := -1;
end;

{ TGravityClassItem }

function TGravityClassItem.GetDisplayName: string;
begin
  Result := FNativeClass.ClassName;
end;

procedure TGravityClassItem.RegisterClass(vm: Pgravity_vm);
var
  AMeta: Pgravity_class_t;
  LContext: TRttiContext;
  LType: TRttiType;
  LMethod: TRttiMethod;
  LProperty: TRttiProperty;
  LField: TRttiField;

  procedure InternalRegisterMethod(AKlass: Pgravity_class_t);
  var
    AFuncRec: PNativeFunctionRecord;
  begin
    with GravityEng do
    begin
      GetMem(AFuncRec, SizeOf(TNativeFunctionRecord));
      AFuncRec^.FuncType := ftMethod;
      GetMem(AFuncRec^.FuncName, Length(AnsiString(LMethod.Name)) + 1);
      System.AnsiStrings.StrPCopy(AFuncRec^.FuncName, AnsiString(LMethod.Name));
      gravity_class_bind(AKlass, AnsiString(LMethod.Name),
        NEW_CLOSURE_VALUE_BRIDGED(AnsiString(LMethod.Name), AFuncRec));
    end;
  end;

  procedure InternalRegisterProperty(AKlass: Pgravity_class_t);
  var
    LPropInfo: PPropInfo;
    AGetterRec, ASetterRec: PNativeFunctionRecord;
    CGetter, CSetter: Pgravity_closure_t;

  begin
    with GravityEng do
    begin
      LPropInfo := TRttiInstanceProperty(LProperty).PropInfo;
      CGetter := nil;
      CSetter := nil;
      if LPropInfo.GetProc <> nil then
      begin
        GetMem(AGetterRec, SizeOf(TNativeFunctionRecord));
        AGetterRec^.FuncType := ftPropGetter;
        GetMem(AGetterRec^.FuncName, Length(AnsiString(LProperty.Name)) + 1);
        System.AnsiStrings.StrPCopy(AGetterRec^.FuncName, AnsiString(LProperty.Name));
        CGetter := gravity_closure_new(nil, NEW_FUNCTION_BRIDGED(AnsiString(LProperty.Name), AGetterRec));
      end;
      if LPropInfo.SetProc <> nil then
      begin
        GetMem(ASetterRec, SizeOf(TNativeFunctionRecord));
        ASetterRec^.FuncType := ftPropSetter;
        GetMem(ASetterRec^.FuncName, Length(AnsiString(LProperty.Name)) + 1);
        System.AnsiStrings.StrPCopy(ASetterRec^.FuncName, AnsiString(LProperty.Name));
        CSetter := gravity_closure_new(nil, NEW_FUNCTION_BRIDGED(AnsiString(LProperty.Name), ASetterRec));
      end;
      gravity_class_bind(AKlass, AnsiString(LProperty.Name),
          NEW_CLOSURE_VALUE_SPECIAL(AnsiString(LProperty.Name),
          GRAVITY_BRIDGE_INDEX, CGetter, CSetter));
    end;
  end;

  procedure InternalRegisterField(AKlass: Pgravity_class_t);
  var
    AGetterRec, ASetterRec: PNativeFunctionRecord;
    CGetter, CSetter: Pgravity_closure_t;
  begin
    with GravityEng do
    begin
      GetMem(AGetterRec, SizeOf(TNativeFunctionRecord));
      AGetterRec^.FuncType := ftFieldGetter;
      GetMem(AGetterRec^.FuncName, Length(AnsiString(LField.Name)) + 1);
      System.AnsiStrings.StrPCopy(AGetterRec^.FuncName, AnsiString(LField.Name));
      CGetter := gravity_closure_new(nil, NEW_FUNCTION_BRIDGED(AnsiString(LField.Name), AGetterRec));

      GetMem(ASetterRec, SizeOf(TNativeFunctionRecord));
      ASetterRec^.FuncType := ftFieldSetter;
      GetMem(ASetterRec^.FuncName, Length(AnsiString(LField.Name)) + 1);
      System.AnsiStrings.StrPCopy(ASetterRec^.FuncName, AnsiString(LField.Name));
      CSetter := gravity_closure_new(nil, NEW_FUNCTION_BRIDGED(AnsiString(LField.Name), ASetterRec));

      gravity_class_bind(AKlass, AnsiString(LField.Name),
          NEW_CLOSURE_VALUE_SPECIAL(AnsiString(LField.Name),
          GRAVITY_BRIDGE_INDEX, CGetter, CSetter));
    end;
  end;

begin
  if FNativeClass.RegisterClass(vm, Self) then
    exit;
  with GravityEng do
  begin
    if not Assigned(FGravityClass) then
    begin
      LContext := TRttiContext.Create;
      LType := LContext.GetType(FNativeClass);
      if LType = nil then
        Exit;
      try
        FGravityClass := gravity_class_new_pair(nil,
          AnsiString(FNativeClass.ClassName), nil, 0, 0);
        AMeta := gravity_class_get_meta(FGravityClass);
        gravity_class_setxdata(FGravityClass, Self);
        for LMethod in LType.GetMethods do
          if LMethod.Visibility >= mvPublic then
            case LMethod.MethodKind of
              mkProcedure, mkFunction:
                InternalRegisterMethod(FGravityClass);
              mkClassProcedure, mkClassFunction:
                InternalRegisterMethod(AMeta);
            end;
        for LProperty in LType.GetProperties do
          if (LProperty.Visibility >= mvPublic) and
            (LProperty is TRttiInstanceProperty) then
              InternalRegisterProperty(FGravityClass);
        for LField in LType.GetFields do
          if LField.Visibility >= mvPublic then
            InternalRegisterField(FGravityClass);
      finally
        LContext.Free;
      end;
    end;
    if vm <> nil then
      gravity_vm_setvalue(vm, AnsiString(FNativeClass.ClassName),
        VALUE_FROM_OBJECT(FGravityClass));
  end;
end;

procedure TGravityClassItem.SetGravityClass(AClass: Pgravity_class_t);
begin
  FGravityClass := AClass;
end;

procedure TGravityClassItem.SetNativeClass(AClass: TGravityWrapperBaseClass);
begin
  FNativeClass := AClass;
end;

procedure TGravityClassItem.UnregisterClass(vm: Pgravity_vm);
var
  AMeta: Pgravity_class_t;
begin
  if Assigned(FGravityClass) and (not FNativeClass.UnregisterClass(vm, Self)) then
  with GravityEng do
  begin
    AMeta := gravity_class_get_meta(FGravityClass);
    gravity_hash_iterate(AMeta^.htable, host_bridge_hash_iterate, vm);
    gravity_hash_iterate(FGravityClass^.htable,
       host_bridge_hash_iterate, vm);
    gravity_class_free(nil, FGravityClass);
    FGravityClass := nil;
  end;
end;

{ TGravityClassReg }

function TGravityClassReg.BestClass(AClass: TGravityWrapperBaseClass)
  : TGravityWrapperBaseClass;
var
  AItem: pointer;
begin
  Result := nil;
  for AItem in FWrapperGroupClasses do
  begin
    if AClass.InheritsFrom(TGravityWrapperBaseClass(AItem)) and
      ((Result = nil) or (TGravityWrapperBaseClass(AItem).InheritsFrom(Result)))
    then
      Result := TGravityWrapperBaseClass(AItem);
  end;
end;

class function TGravityClassReg.BestGroup(Group1, Group2: TGravityClassReg;
  AClass: TGravityWrapperBaseClass): TGravityClassReg;
var
  Group1Class: TGravityWrapperBaseClass;
  Group2Class: TGravityWrapperBaseClass;
begin
  Group1Class := nil;
  Group2Class := nil;
  if Group1 <> nil then
    Group1Class := Group1.BestClass(AClass);
  if Group2 <> nil then
    Group2Class := Group2.BestClass(AClass);
  if Group1Class = nil then
  begin
    Result := Group2;
    if Group2Class = nil then
      Result := nil;
  end
  else
  begin
    Result := Group1;
    if Group2Class <> nil then
    begin
      Result := Group2;
      if Group1Class.InheritsFrom(Group2Class) then
        Result := Group1;
    end;
  end;
end;

constructor TGravityClassReg.Create(AClass: TGravityWrapperBaseClass);
begin
  inherited Create;
  FWrapperList := TGravityClassList.Create;
  FWrapperGroupClasses := TList.Create;
  FWrapperGroupClasses.Add(AClass);
end;

destructor TGravityClassReg.Destroy;
begin
  FWrapperGroupClasses.Free;
  FWrapperList.Free;
  inherited Destroy;
end;

function TGravityClassReg.GetClass(const AClassName: string)
  : TGravityWrapperBaseClass;
var
  AItem: pointer;
begin
  for AItem in FWrapperList do
  begin
    Result := TGravityWrapperBaseClass(TGravityClassItem(AItem).NativeClass);
    if Result.ClassNameIs(AClassName) then
      Exit;
  end;
  Result := nil;
end;

procedure TGravityClassReg.GetClasses(Proc: TGravityGetClass);
var
  AItem: pointer;
begin
  for AItem in FWrapperList do
    Proc(TGravityWrapperBaseClass(TGravityClassItem(AItem).NativeClass));
end;

function TGravityClassReg.GetClassNames: TArray<String>;
begin
  Result := FWrapperList.CollectionNames;
end;

function TGravityClassReg.GetGravityClass(const AClassName: string)
  : Pgravity_class_t;
var
  AItem: pointer;
begin
  for AItem in FWrapperList do
  begin
    Result := TGravityClassItem(AItem).FGravityClass;
    if TGravityClassItem(AItem).FNativeClass.ClassNameIs(AClassName) then
      Exit;
  end;
  Result := nil;
end;

procedure TGravityClassReg.RegisterClass(AClass: TGravityWrapperBaseClass);
var
  LClassName: string;
begin
  LClassName := AClass.ClassName;
  if GetClass(LClassName) <> nil then
    raise EFilerError.CreateResFmt(@SDuplicateClass, [LClassName]);
  with TGravityClassItem(FWrapperList.Add) do
  begin
    SetGravityClass(nil);
    SetNativeClass(AClass);
  end;
end;

function TGravityClassReg.Registered(AClass: TGravityWrapperBaseClass): Boolean;
begin
  Result := FWrapperList.IndexOf(AClass) >= 0;
end;

procedure TGravityClassReg.UnregisterClass(AClass: TGravityWrapperBaseClass);
var
  I: Integer;
begin
  I := FWrapperList.IndexOf(AClass);
  if I >= 0 then
    FWrapperList.Delete(I);
end;

{ TGravityClassRegs }

constructor TGravityClassRegs.Create;
var
  AGroup: TGravityClassReg;
begin
  inherited Create;
  InitializeCriticalSection(FLock);
  FWrapperGroups := TList.Create;
  AGroup := TGravityClassReg.Create(TGravityWrapperBase);
  FWrapperGroups.Add(AGroup);
  AGroup.Active := True;
  FNameList := TAnsiStrings.Create;
  FModules := TGravityModuleList.Create;
  FDelegates := TList.Create;
end;

destructor TGravityClassRegs.Destroy;
var
  I: Integer;
begin
  for I := 0 to FWrapperGroups.Count - 1 do
    TGravityClassReg(FWrapperGroups[I]).Free;
  FWrapperGroups.Free;
  FDelegates.Free;
  FModules.Free;
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

function TGravityClassRegs.FindGroup(AClass: TGravityWrapperBaseClass)
  : TGravityClassReg;
var
  AItem: pointer;
begin
  Result := nil;
  for AItem in FWrapperGroups do
    Result := TGravityClassReg.BestGroup(TGravityClassReg(AItem),
      Result, AClass);
end;

function TGravityClassRegs.GetClass(const AClassName: string)
  : TGravityWrapperBaseClass;
var
  AItem: pointer;
begin
  Result := nil;
  for AItem in FWrapperGroups do
    with TGravityClassReg(AItem) do
    begin
      if Active then
        Result := GetClass(AClassName);
      if Result <> nil then
        Exit;
    end;
end;

function TGravityClassRegs.GetGravityClass(const AClassName: string)
  : Pgravity_class_t;
var
  AItem: pointer;
begin
  Result := nil;
  for AItem in FWrapperGroups do
    with TGravityClassReg(AItem) do
    begin
      if Active then
        Result := GetGravityClass(AClassName);
      if Result <> nil then
        Exit;
    end;
end;

function TGravityClassRegs.GetName(I: Integer): string;
begin
  Result := String(FNameList[I]);
end;

procedure TGravityClassRegs.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TGravityClassRegs.RegisterClass(AClass: TGravityWrapperBaseClass);
var
  AGroup: TGravityClassReg;
begin
  AGroup := FindGroup(AClass);
  if AGroup <> nil then
  begin
    AGroup.RegisterClass(AClass);
    RegisterName(AClass.ClassName);
  end;
end;

function TGravityClassRegs.Registered(AClass: TGravityWrapperBaseClass)
  : Boolean;
var
  AItem: pointer;
begin
  Result := True;
  for AItem in FWrapperGroups do
    if TGravityClassReg(AItem).Registered(AClass) then
      Exit;
  Result := False;
end;

procedure TGravityClassRegs.RegisterName(AName: String);
begin
  if FNameList.IndexOf(AnsiString(AName)) < 0 then
    FNameList.Append(AnsiString(AName));
end;

procedure TGravityClassRegs.Unlock;
begin
  LeaveCriticalSection(FLock);
end;

procedure TGravityClassRegs.UnregisterClass(AClass: TGravityWrapperBaseClass);
var
  I: Integer;
begin
  for I := 0 to FWrapperGroups.Count - 1 do
    TGravityClassReg(FWrapperGroups[I]).UnregisterClass(AClass);
  I := FNameList.IndexOf(AnsiString(AClass.ClassName));
  if I >= 0 then
    FNameList.Delete(I);
end;

procedure RegisterGravityClass(AClass: TGravityWrapperBaseClass);
begin
  GravityRegGroups.Lock;
  try
    while not GravityRegGroups.Registered(AClass) do
    begin
      GravityRegGroups.RegisterClass(AClass);
      if AClass = TGravityWrapperBase then
        Break;
      AClass := TGravityWrapperBaseClass(AClass.ClassParent);
    end;
  finally
    GravityRegGroups.Unlock;
  end;
end;

procedure RegisterGravityClasses(AClasses: array of TGravityWrapperBaseClass);
var
  I: Integer;
begin
  for I := Low(AClasses) to High(AClasses) do
    RegisterGravityClass(AClasses[I]);
end;

procedure UnRegisterGravityClass(AClass: TGravityWrapperBaseClass);
begin
  GravityRegGroups.Lock;
  try
    GravityRegGroups.UnregisterClass(AClass);
  finally
    GravityRegGroups.Unlock;
  end;
end;

procedure UnRegisterGravityClasses(AClasses: array of TGravityWrapperBaseClass);
var
  I: Integer;
begin
  for I := Low(AClasses) to High(AClasses) do
    UnRegisterGravityClass(AClasses[I]);
end;

procedure BindNativeModules(AVm: Pgravity_vm);
var
  AGroup, AItem: pointer;
begin
  with GravityEng do
  begin
    gravity_register_bridges(AVm);
    for AItem in GravityRegGroups.Delegates do
      if AItem <> nil then
        TGravityDelegateProc(AItem)(AVm);
    for AItem in GravityRegGroups.Modules do
      TGravityModuleItem(AItem).RegisterProc(AVm);
    for AGroup in GravityRegGroups.WrapperGroups do
      for AItem in TGravityClassReg(AGroup).FWrapperList do
        TGravityClassItem(AItem).RegisterClass(AVm);
  end;
end;

procedure UnbindNativeModules(AVm: Pgravity_vm);
var
  AGroup, AItem: pointer;
begin
  with GravityEng do
  begin
    for AItem in GravityRegGroups.Modules do
      TGravityModuleItem(AItem).UnregisterProc(AVm);
    for AGroup in GravityRegGroups.WrapperGroups do
      for AItem in TGravityClassReg(AGroup).FWrapperList do
        TGravityClassItem(AItem).UnregisterClass(AVm);
  end;
end;

procedure ClassNotFound(const ClassName: string);
begin
  raise EClassNotFound.CreateFmt(SClassNotFound, [ClassName]);
end;

function FindNativeClass(const ClassName: string): TGravityWrapperBaseClass;
begin
  Result := GetNativeClass(ClassName);
  if Result = nil then
    ClassNotFound(ClassName);
end;

function GetNativeClass(const AClassName: string): TGravityWrapperBaseClass;
begin
  GravityRegGroups.Lock;
  try
    Result := GravityRegGroups.GetClass(AClassName);
  finally
    GravityRegGroups.Unlock;
  end;
end;

function GetGravityClass(const AClassName: string): Pgravity_class_t;
begin
  GravityRegGroups.Lock;
  try
    Result := GravityRegGroups.GetGravityClass(AClassName);
  finally
    GravityRegGroups.Unlock;
  end;
end;

procedure DefineNativeObject(AVm: Pgravity_vm; const AVarName: String;
  AObj: TGravityWrapperBase);
var
  AClass: Pgravity_class_t;
  AInstance: Pgravity_instance_t;
begin
  with GravityEng do
  begin
    AClass := GetGravityClass(AObj.ClassName);
    if AClass <> nil then
    begin
      gravity_gc_setenabled(AVm, False);
      AInstance := gravity_instance_new(AVm, AClass);
      gravity_instance_setxdata(AInstance, AObj);
      AObj.FGravityInstance := AInstance;
      gravity_gc_setenabled(AVm, True);
      gravity_vm_setvalue(AVm, AnsiString(AVarName),
        VALUE_FROM_OBJECT(AInstance));
      GravityRegGroups.RegisterName(AVarName);
    end;
  end;
end;

procedure RegisterGravityName(const AVarName: String);
begin
  GravityRegGroups.Lock;
  try
    GravityRegGroups.RegisterName(AVarName);
  finally
    GravityRegGroups.Unlock;
  end;
end;

{ TGravityWrapperBase }

constructor TGravityWrapperBase.Create;
begin
  inherited Create;
  FGravityInstance := nil;
end;

destructor TGravityWrapperBase.Destroy;
begin
  FGravityInstance := nil;
  inherited Destroy;
end;

class function TGravityWrapperBase.RegisterClass(vm: Pgravity_vm;
  var ClassItem: TGravityClassItem): boolean;
begin
  Result := False;
end;

class function TGravityWrapperBase.UnregisterClass(vm: Pgravity_vm;
  var ClassItem: TGravityClassItem): boolean;
begin
  Result := False;
end;

function GravityCall(AVm: Pgravity_vm; AFunc, ASender: gravity_value_t;
  Args: array of const): Variant;
var
  ALen, I: Integer;
  AParams: gravity_value_t_array;
  AVarRec: TVarRec;
  AResult: gravity_value_t;
begin
  with GravityEng do
  begin
    if VALUE_ISA_CLOSURE(AFunc) then
    begin
      ALen := Length(Args);
      if ALen > 0 then
      begin
        SetLength(AParams, ALen);
        for I := Low(Args) to High(Args) do
        begin
          AVarRec := Args[I];
          case AVarRec.VType of
            vtBoolean:
              AParams[I] := VALUE_FROM_BOOL(AVarRec.VBoolean);
            vtInteger:
              AParams[I] := VALUE_FROM_INT(AVarRec.VInteger);
            vtInt64:
              AParams[I] := VALUE_FROM_INT(AVarRec.VInteger);
            vtExtended:
              AParams[I] := VALUE_FROM_FLOAT(AVarRec.VExtended^);
            vtCurrency:
              AParams[I] := VALUE_FROM_FLOAT(AVarRec.VCurrency^);
            vtString:
              AParams[I] := VALUE_FROM_STRING(AVm,
                PAnsiString(AVarRec.VAnsiString)^);
            vtChar:
              AParams[I] := VALUE_FROM_STRING(AVm, AVarRec.VChar);
            vtWideChar:
              AParams[I] := VALUE_FROM_STRING(AVm, AnsiString(String(AVarRec.VWideChar)));
            vtWideString:
              AParams[I] := VALUE_FROM_STRING(AVm,
                AnsiString(String(WideString(AVarRec.VWideString))));
          end;
        end;
      end;
      Result := None;
      try
        if gravity_vm_runclosure(AVm, VALUE_AS_CLOSURE(AFunc), ASender,
          @AParams[0], Length(AParams)) then
        begin
          AResult := gravity_vm_result(AVm);
          if VALUE_ISA_BASIC_TYPE(AResult) then
          begin
            if VALUE_ISA_INT(AResult) then
              Result := VALUE_AS_INT(AResult)
            else if VALUE_ISA_FLOAT(AResult) then
              Result := VALUE_AS_FLOAT(AResult)
            else if VALUE_ISA_BOOL(AResult) then
              Result := VALUE_AS_BOOL(AResult)
            else if VALUE_ISA_STRING(AResult) then
              Result := AnsiString(VALUE_AS_CSTRING(AResult));
          end;
        end;
      finally
        SetLength(AParams, 0);
      end;
    end;
  end;
end;

procedure RegisterGravityModule(ARegProc, AUnregProc: TGravityModuleProc);
begin
  with TGravityModuleItem(GravityRegGroups.FModules.Add) do
  begin
    FRegisterProc := ARegProc;
    FUnregisterProc := AUnregProc;
  end;
end;

procedure RegisterGravityDelegate(ADelegateProc: TGravityDelegateProc);
begin
  GravityRegGroups.Delegates.Add(@ADelegateProc);
end;

{ TGravityModuleList }

constructor TGravityModuleList.Create;
begin
  inherited Create(TGravityModuleItem);
end;

initialization

GravityRegGroups := TGravityClassRegs.Create;

end.
