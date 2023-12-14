unit Gravity.GVSysUtils;

interface
uses
  System.SysUtils, Gravity.GVIntf, Gravity.GVClasses;


procedure GravitySysUtilsInit;
procedure GravitySysUtilsRegister(vm: Pgravity_vm);
procedure GravitySysUtilsFree;


implementation
uses
  System.AnsiStrings;

var
  GravitySysUtilsClass: Pgravity_class_t;

  reff_count: integer = 0;
  sysutils_initialized: boolean = False;


const
  GravitySysUtilsClassName: AnsiString = 'SysUtils';



function gravity_date(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AVal: gravity_value_t;
begin
  with GravityEng do
  try
    AVal := VALUE_FROM_FLOAT(Date);
    if (VALUE_ISA_NOTVALID(AVal)) then
      Result := RETURN_NOVALUE
    else
      Result := RETURN_VALUE(vm, AVal, rindex);
  except
    on E:Exception do
      begin
        Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
      end;
  end;
end;

function gravity_curryear(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AVal: gravity_value_t;
begin
  with GravityEng do
  try
    AVal := VALUE_FROM_INT(CurrentYear);
    if (VALUE_ISA_NOTVALID(AVal)) then
      Result := RETURN_NOVALUE
    else
      Result := RETURN_VALUE(vm, AVal, rindex);
  except
    on E:Exception do
      begin
        Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
      end;
  end;
end;

function gravity_time(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AVal: gravity_value_t;
begin
  with GravityEng do
  try
    AVal := VALUE_FROM_FLOAT(Time);
    if (VALUE_ISA_NOTVALID(AVal)) then
      Result := RETURN_NOVALUE
    else
      Result := RETURN_VALUE(vm, AVal, rindex);
  except
    on E:Exception do
      begin
        Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
      end;
  end;
end;

function gravity_now(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AVal: gravity_value_t;
begin
  with GravityEng do
  try
    AVal := VALUE_FROM_FLOAT(Now);
    if (VALUE_ISA_NOTVALID(AVal)) then
      Result := RETURN_NOVALUE
    else
      Result := RETURN_VALUE(vm, AVal, rindex);
  except
    on E:Exception do
      begin
        Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
      end;
  end;
end;

function gravity_replacedate(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  ADate_1, ADate_2: TDateTime;
  AVal: gravity_value_t;
begin
  with GravityEng do
  begin
    if (nargs <> 4) then
      Result := RETURN_ERROR(vm, 'Incorrect number of arguments.', rindex)
    else
    try
      ADate_1 := VALUE_AS_FLOAT(GET_VALUE(args, 1));
      ADate_2 := VALUE_AS_FLOAT(GET_VALUE(args, 1));
      ReplaceDate(ADate_1, ADate_2);
      AVal := VALUE_FROM_FLOAT(ADate_1);
      if (VALUE_ISA_NOTVALID(AVal)) then
        Result := RETURN_NOVALUE
      else
        Result := RETURN_VALUE(vm, AVal, rindex);
    except
      on E:Exception do
        begin
          Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
        end;
    end;
  end;
end;

function gravity_replacetime(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  ADate_1, ADate_2: TDateTime;
  AVal: gravity_value_t;
begin
  with GravityEng do
  begin
    if (nargs <> 4) then
      Result := RETURN_ERROR(vm, 'Incorrect number of arguments.', rindex)
    else
    try
      ADate_1 := VALUE_AS_FLOAT(GET_VALUE(args, 1));
      ADate_2 := VALUE_AS_FLOAT(GET_VALUE(args, 1));
      ReplaceTime(ADate_1, ADate_2);
      AVal := VALUE_FROM_FLOAT(ADate_1);
      if (VALUE_ISA_NOTVALID(AVal)) then
        Result := RETURN_NOVALUE
      else
        Result := RETURN_VALUE(vm, AVal, rindex);
    except
      on E:Exception do
        begin
          Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
        end;
    end;
  end;
end;

function gravity_encodedate(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AVal: gravity_value_t;
begin
  with GravityEng do
  begin
    if (nargs <> 4) then
      Result := RETURN_ERROR(vm, 'Incorrect number of arguments.', rindex)
    else
    try
      AVal := VALUE_FROM_FLOAT(
          EncodeDate(
            VALUE_AS_INT(GET_VALUE(args, 1)),
            VALUE_AS_INT(GET_VALUE(args, 2)),
            VALUE_AS_INT(GET_VALUE(args, 3))
          )
        );
      if (VALUE_ISA_NOTVALID(AVal)) then
        Result := RETURN_NOVALUE
      else
        Result := RETURN_VALUE(vm, AVal, rindex);
    except
      on E:Exception do
        begin
          Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
        end;
    end;
  end;
end;

function gravity_encodetime(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AVal: gravity_value_t;
begin
  with GravityEng do
  begin
    if (nargs <> 5) then
      Result := RETURN_ERROR(vm, 'Incorrect number of arguments.', rindex)
    else
    try
      AVal := VALUE_FROM_FLOAT(
          EncodeTime(
            VALUE_AS_INT(GET_VALUE(args, 1)),
            VALUE_AS_INT(GET_VALUE(args, 2)),
            VALUE_AS_INT(GET_VALUE(args, 3)),
            VALUE_AS_INT(GET_VALUE(args, 4))
          )
        );
      if (VALUE_ISA_NOTVALID(AVal)) then
        Result := RETURN_NOVALUE
      else
        Result := RETURN_VALUE(vm, AVal, rindex);
    except
      on E:Exception do
        begin
          Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
        end;
    end;
  end;
end;

function gravity_encodedatetime(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AVal: gravity_value_t;
begin
  with GravityEng do
  begin
    if (nargs <> 8) then
      Result := RETURN_ERROR(vm, 'Incorrect number of arguments.', rindex)
    else
    try
      AVal := VALUE_FROM_FLOAT(
          EncodeDate(
            VALUE_AS_INT(GET_VALUE(args, 1)),
            VALUE_AS_INT(GET_VALUE(args, 2)),
            VALUE_AS_INT(GET_VALUE(args, 3))
          ) +
          EncodeTime(
            VALUE_AS_INT(GET_VALUE(args, 4)),
            VALUE_AS_INT(GET_VALUE(args, 5)),
            VALUE_AS_INT(GET_VALUE(args, 6)),
            VALUE_AS_INT(GET_VALUE(args, 7))
          )
        );
      if (VALUE_ISA_NOTVALID(AVal)) then
        Result := RETURN_NOVALUE
      else
        Result := RETURN_VALUE(vm, AVal, rindex);
    except
      on E:Exception do
        begin
          Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
        end;
    end;
  end;
end;

function gravity_yearof(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AYear, AMonth, ADay: Word;
begin
  with GravityEng do
  begin
    if (nargs <> 2) then
      Result := RETURN_ERROR(vm, 'Incorrect number of arguments.', rindex)
    else
    try
      DecodeDate(VALUE_AS_FLOAT(GET_VALUE(args, 1)), AYear, AMonth, ADay);
      Result := RETURN_VALUE(vm, VALUE_FROM_INT(AYear), rindex);
    except
      on E:Exception do
        begin
          Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
        end;
    end;
  end;
end;

function gravity_monthof(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AYear, AMonth, ADay: Word;
begin
  with GravityEng do
  begin
    if (nargs <> 2) then
      Result := RETURN_ERROR(vm, 'Incorrect number of arguments.', rindex)
    else
    try
      DecodeDate(VALUE_AS_FLOAT(GET_VALUE(args, 1)), AYear, AMonth, ADay);
      Result := RETURN_VALUE(vm, VALUE_FROM_INT(AMonth), rindex);
    except
      on E:Exception do
        begin
          Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
        end;
    end;
  end;
end;

function gravity_dayof(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AYear, AMonth, ADay: Word;
begin
  with GravityEng do
  begin
    if (nargs <> 2) then
      Result := RETURN_ERROR(vm, 'Incorrect number of arguments.', rindex)
    else
    try
      DecodeDate(VALUE_AS_FLOAT(GET_VALUE(args, 1)), AYear, AMonth, ADay);
      Result := RETURN_VALUE(vm, VALUE_FROM_INT(ADay), rindex);
    except
      on E:Exception do
        begin
          Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
        end;
    end;
  end;
end;

function gravity_hourof(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AHour, AMin, ASec, AMsec: Word;
begin
  with GravityEng do
  begin
    if (nargs <> 2) then
      Result := RETURN_ERROR(vm, 'Incorrect number of arguments.', rindex)
    else
    try
      DecodeTime(VALUE_AS_FLOAT(GET_VALUE(args, 1)), AHour, AMin, ASec, AMsec);
      Result := RETURN_VALUE(vm, VALUE_FROM_INT(AHour), rindex);
    except
      on E:Exception do
        begin
          Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
        end;
    end;
  end;
end;

function gravity_minof(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AHour, AMin, ASec, AMsec: Word;
begin
  with GravityEng do
  begin
    if (nargs <> 2) then
      Result := RETURN_ERROR(vm, 'Incorrect number of arguments.', rindex)
    else
    try
      DecodeTime(VALUE_AS_FLOAT(GET_VALUE(args, 1)), AHour, AMin, ASec, AMsec);
      Result := RETURN_VALUE(vm, VALUE_FROM_INT(AMin), rindex);
    except
      on E:Exception do
        begin
          Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
        end;
    end;
  end;
end;

function gravity_secof(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AHour, AMin, ASec, AMsec: Word;
begin
  with GravityEng do
  begin
    if (nargs <> 2) then
      Result := RETURN_ERROR(vm, 'Incorrect number of arguments.', rindex)
    else
    try
      DecodeTime(VALUE_AS_FLOAT(GET_VALUE(args, 1)), AHour, AMin, ASec, AMsec);
      Result := RETURN_VALUE(vm, VALUE_FROM_INT(ASec), rindex);
    except
      on E:Exception do
        begin
          Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
        end;
    end;
  end;
end;

function gravity_msecof(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AHour, AMin, ASec, AMsec: Word;
begin
  with GravityEng do
  begin
    if (nargs <> 2) then
      Result := RETURN_ERROR(vm, 'Incorrect number of arguments.', rindex)
    else
    try      
      DecodeTime(VALUE_AS_FLOAT(GET_VALUE(args, 1)), AHour, AMin, ASec, AMsec);
      Result := RETURN_VALUE(vm, VALUE_FROM_INT(AMsec), rindex);  
    except
      on E:Exception do
        begin
          Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
        end;
    end;
  end;
end;

function gravity_incmonth(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AMonth: integer;
begin
  with GravityEng do
  begin
    if (nargs < 2) then
      Result := RETURN_ERROR(vm, 'Incorrect number of arguments.', rindex)
    else
    begin
      AMonth := 1;
      if nargs > 2 then
        AMonth := VALUE_AS_INT(GET_VALUE(args, 2));
      try
        Result := RETURN_VALUE(vm, VALUE_FROM_FLOAT(
            IncMonth(VALUE_AS_FLOAT(GET_VALUE(args, 1)), AMonth)
          ), rindex);
      except
        on E:Exception do
          begin
            Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
          end;
      end;
    end;
  end;
end;

function gravity_strtodatetime(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
begin
  with GravityEng do
  begin
    if (nargs <> 2) then
      Result := RETURN_ERROR(vm, 'Incorrect number of arguments.', rindex)
    else
    begin
      try
        Result := RETURN_VALUE(vm, VALUE_FROM_FLOAT(
            StrToDateTime(String(AnsiString(VALUE_AS_CSTRING(GET_VALUE(args, 1)))))
          ), rindex);
      except
        on E:Exception do
          begin
            Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
          end;
      end;
    end;
  end;
end;

function gravity_strtodate(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
begin
  with GravityEng do
  begin
    if (nargs <> 2) then
      Result := RETURN_ERROR(vm, 'Incorrect number of arguments.', rindex)
    else
    begin
      try
        Result := RETURN_VALUE(vm, VALUE_FROM_FLOAT(
            StrToDate(String(AnsiString(VALUE_AS_CSTRING(GET_VALUE(args, 1)))))
          ), rindex);
      except
        on E:Exception do
          begin
            Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
          end;
      end;
    end;
  end;
end;

function gravity_strtotime(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
begin
  with GravityEng do
  begin
    if (nargs <> 2) then
      Result := RETURN_ERROR(vm, 'Incorrect number of arguments.', rindex)
    else
    begin
      try
        Result := RETURN_VALUE(vm, VALUE_FROM_FLOAT(
            StrToTime(String(AnsiString(VALUE_AS_CSTRING(GET_VALUE(args, 1)))))
          ), rindex);
      except
        on E:Exception do
          begin
            Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
          end;
      end;
    end;
  end;
end;

function gravity_formatdatetime(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
begin
  with GravityEng do
  begin
    if (nargs <> 3) then
      Result := RETURN_ERROR(vm, 'Incorrect number of arguments.', rindex)
    else
    begin
      try
        Result := RETURN_VALUE(vm, VALUE_FROM_STRING(vm,
          AnsiString(FormatDateTime(
            String(AnsiString(VALUE_AS_CSTRING(GET_VALUE(args, 1)))),
            VALUE_AS_FLOAT(GET_VALUE(args, 2))))), rindex);
      except
        on E:Exception do
          begin
            Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
          end;
      end;
    end;
  end;
end;

function gravity_createguid(vm: Pgravity_vm; args: Pgravity_value_t; nargs: UInt16; rindex: UInt32): Boolean; cdecl;
var
  AGuid: TGUID;
begin
  with GravityEng do
  try
    CreateGUID(AGuid);
    Result := RETURN_VALUE(vm, VALUE_FROM_STRING(vm,
      AnsiString(GUIDToString(AGuid))), rindex);
  except
    on E:Exception do
      begin
        Result := RETURN_ERROR(vm, AnsiString(E.Message), rindex);
      end;
  end;
end;

procedure GravitySysUtilsInit;
var
  AMeta: Pgravity_class_t;
begin
  if sysutils_initialized then
    exit;
  sysutils_initialized := True;
  with GravityEng do
  begin
    GravitySysUtilsClass := gravity_class_new_pair(nil, GravitySysUtilsClassName, nil, 0, 0);
    AMeta := gravity_class_get_meta(GravitySysUtilsClass);
    gravity_class_bind(AMeta, 'date', NEW_CLOSURE_VALUE(gravity_date));
    gravity_class_bind(AMeta, 'time', NEW_CLOSURE_VALUE(gravity_time));
    gravity_class_bind(AMeta, 'today', NEW_CLOSURE_VALUE(gravity_date));
    gravity_class_bind(AMeta, 'now', NEW_CLOSURE_VALUE(gravity_now));
    gravity_class_bind(AMeta, 'yearof', NEW_CLOSURE_VALUE(gravity_yearof));
    gravity_class_bind(AMeta, 'monthof', NEW_CLOSURE_VALUE(gravity_monthof));
    gravity_class_bind(AMeta, 'dayof', NEW_CLOSURE_VALUE(gravity_dayof));
    gravity_class_bind(AMeta, 'hourof', NEW_CLOSURE_VALUE(gravity_hourof));
    gravity_class_bind(AMeta, 'minof', NEW_CLOSURE_VALUE(gravity_minof));
    gravity_class_bind(AMeta, 'secof', NEW_CLOSURE_VALUE(gravity_secof));
    gravity_class_bind(AMeta, 'msecof', NEW_CLOSURE_VALUE(gravity_msecof));
    gravity_class_bind(AMeta, 'encodedate', NEW_CLOSURE_VALUE(gravity_encodedate));
    gravity_class_bind(AMeta, 'encodetime', NEW_CLOSURE_VALUE(gravity_encodetime));
    gravity_class_bind(AMeta, 'encodedatetime', NEW_CLOSURE_VALUE(gravity_encodedatetime));
    gravity_class_bind(AMeta, 'incmonth', NEW_CLOSURE_VALUE(gravity_incmonth));
    gravity_class_bind(AMeta, 'currentyear', NEW_CLOSURE_VALUE(gravity_curryear));
    gravity_class_bind(AMeta, 'replacedate', NEW_CLOSURE_VALUE(gravity_replacedate));
    gravity_class_bind(AMeta, 'replacetime', NEW_CLOSURE_VALUE(gravity_replacetime));
    gravity_class_bind(AMeta, 'strtodate', NEW_CLOSURE_VALUE(gravity_strtodate));
    gravity_class_bind(AMeta, 'strtotime', NEW_CLOSURE_VALUE(gravity_strtotime));
    gravity_class_bind(AMeta, 'strtodatetime', NEW_CLOSURE_VALUE(gravity_strtodatetime));
    gravity_class_bind(AMeta, 'formatdatetime', NEW_CLOSURE_VALUE(gravity_formatdatetime));

    gravity_class_bind(AMeta, 'createguid', NEW_CLOSURE_VALUE(gravity_createguid));

    SETMETA_INITED(GravitySysUtilsClass);

    RegisterGravityName(String(GravitySysUtilsClassName));
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
    gravity_class_free(nil, gravity_class_get_meta(GravitySysUtilsClass));
    gravity_class_free(nil, GravitySysUtilsClass);
    GravitySysUtilsClass := nil;
  end;
  sysutils_initialized := False;
end;



end.
