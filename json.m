:- module json.
:- interface.

:- import_module bool.

% TODO: figure out how arrays work! More than half of our time is spent appending to lists.
:- import_module list.

:- type json.value.
:- type json.property.

% `null' is technically an object, but it is MUCH simpler to implement
%  it as a possible state of json.value
:- type json.integer ---> json.integer(integer_val::int).
:- type json.number  ---> json.number(number_val::float).
:- type json.string  ---> json.string(string_val::string).
:- type json.boolean ---> json.boolean(boolean_val::bool).
:- type json.array   ---> json.array(array_val::list(json.value)).
:- type json.object  ---> json.object(object_val::list(json.property)).

:- type json.property ---> json.property(name::json.string, val::json.value).

:- type json.value --->
    null
    ; json.object(value_object::list(json.property))
    ; json.array(value_array::list(json.value))
    ; json.integer(value_integer::int)
    ; json.number(value_number::float)
    ; json.string(value_string::string)
    ; json.boolean(value_boolean::bool).

% TODO: Is null really ok?
:- type json.root --->
    null
    ; json.object(root_object::list(json.property))
    ; json.array(root_array::list(json.value)).

:- type json.error ---> json.error(line::int, expected::string, unexpected::string).

:- type json.result(T) ---> ok(ok_field::T) ; json.error(line_number::int, char_number::int, expected_string::string, unexpected_string::string).

% Finding values

:- pred json.find_object(json.object::in, string::in, json.object::out) is semidet.
:- pred json.find_array(json.object::in, string::in, json.array::out) is semidet.
:- pred json.find_integer(json.object::in, string::in, json.integer::out) is semidet.
:- pred json.find_number(json.object::in, string::in, json.number::out) is semidet.
:- pred json.find_string(json.object::in, string::in, json.string::out) is semidet.
:- pred json.find_boolean(json.object::in, string::in, json.boolean::out) is semidet.
:- pred json.find_null(json.object::in, string::in) is semidet.

% Less useful, but still useful sometimes. Notably they are used to implement the other find_* predicates
:- pred json.find_value(json.object::in, string::in, json.value::out) is semidet.
:- pred json.find_value_list(list(json.property)::in, string::in, json.value::out) is semidet.

:- pred json.find_property(json.object::in, string::in, json.property::out) is semidet.
:- pred json.find_property_list(list(json.property)::in, string::in, json.property::out) is semidet.

% Parsing

:- pred json.parse_value(string::in, int::in, int::out, int::in, json.result(json.value)::out) is det.
:- pred json.parse_property(string::in, int::in, int::out, int::in, json.result(json.property)::out) is det.

:- pred json.parse_integer(string::in, int::in, int::out, int::in, json.result(json.integer)::out) is det.
:- pred json.parse_number(string::in, int::in, int::out, int::in, json.result(json.number)::out) is det.
:- pred json.parse_string(string::in, int::in, int::out, int::in, json.result(json.string)::out) is det.

:- pred json.parse_array(string::in, int::in, int::out, int::in, json.result(json.array)::out) is det.
:- pred json.parse_object(string::in, int::in, int::out, int::in, json.result(json.object)::out) is det.

:- pred json.parse(string::in, json.result(json.root)::out) is det.

% Writing

:- import_module io.

% The predicates that have an extra integer parameter are for pretty printing. Otherwise, it's dumped to file.
% Faster and simpler, but not as nice looking.
% Invoked using write_pretty, as opposed to write
:- pred json.write_value(json.value::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_value(json.value::in, string::uo) is det.
:- pred json.write_property(json.property::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_property(json.property::in, string::uo) is det.

:- pred json.write_integer(json.integer::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_integer(json.integer::in, string::uo) is det.

:- pred json.write_number(json.number::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_number(json.number::in, string::uo) is det.

:- pred json.write_string(json.string::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_string(json.string::in, string::uo) is det.

:- pred json.write_array(json.array::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_array(json.array::in, string::uo) is det.
:- pred json.write_array(json.array::in, int::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_array(json.array::in, int::in, string::uo) is det.
:- pred json.write_object(json.object::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_object(json.object::in, string::uo) is det.
:- pred json.write_object(json.object::in, int::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_object(json.object::in, int::in, string::uo) is det.

:- pred json.write(json.root::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write(json.root::in, string::uo) is det.
:- pred json.write_pretty(json.root::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_pretty(json.root::in, string::uo) is det.

% Implementation
:- implementation.

:- pragma foreign_export("Java", json.parse_value(in, in, out, in, out), "ParseValue").
:- pragma foreign_export("C",    json.parse_value(in, in, out, in, out), "ParseValue").
:- pragma foreign_export("Java", json.parse_property(in, in, out, in, out), "ParseProperty").
:- pragma foreign_export("C",    json.parse_property(in, in, out, in, out), "ParseProperty").
:- pragma foreign_export("Java", json.parse_integer(in, in, out, in, out), "ParseInteger").
:- pragma foreign_export("C",    json.parse_integer(in, in, out, in, out), "ParseInteger").
:- pragma foreign_export("Java", json.parse_number(in, in, out, in, out), "ParseFloat").
:- pragma foreign_export("C",    json.parse_number(in, in, out, in, out), "ParseFloat").
:- pragma foreign_export("Java", json.parse_string(in, in, out, in, out), "ParseString").
:- pragma foreign_export("C",    json.parse_string(in, in, out, in, out), "ParseString").
:- pragma foreign_export("Java", json.parse_object(in, in, out, in, out), "ParseObject").
:- pragma foreign_export("C",    json.parse_object(in, in, out, in, out), "ParseObject").
:- pragma foreign_export("Java", json.parse_array(in, in, out, in, out), "ParseArray").
:- pragma foreign_export("C",    json.parse_array(in, in, out, in, out), "ParseArray").
:- pragma foreign_export("Java", json.parse(in, out), "ParseRoot").
:- pragma foreign_export("C",    json.parse(in, out), "ParseRoot").

:- pragma foreign_export("Java", json.write_value(in, in, di, uo), "WriteValue").
:- pragma foreign_export("C",    json.write_value(in, in, di, uo), "WriteValueIO").
:- pragma foreign_export("Java", json.write_value(in, uo), "WriteValue").
:- pragma foreign_export("C",    json.write_value(in, uo), "WriteValueStr").
:- pragma foreign_export("Java", json.write_property(in, in, di, uo), "WriteProperty").
:- pragma foreign_export("C",    json.write_property(in, in, di, uo), "WritePropertyIO").
:- pragma foreign_export("Java", json.write_property(in, uo), "WriteProperty").
:- pragma foreign_export("C",    json.write_property(in, uo), "WritePropertyStr").
:- pragma foreign_export("Java", json.write_integer(in, in, di, uo), "WriteInteger").
:- pragma foreign_export("C",    json.write_integer(in, in, di, uo), "WriteIntegerIO").
:- pragma foreign_export("Java", json.write_integer(in, uo), "WriteInteger").
:- pragma foreign_export("C",    json.write_integer(in, uo), "WriteIntegerStr").
:- pragma foreign_export("Java", json.write_number(in, in, di, uo), "WriteFloat").
:- pragma foreign_export("C",    json.write_number(in, in, di, uo), "WriteFloatIO").
:- pragma foreign_export("Java", json.write_number(in, uo), "WriteFloat").
:- pragma foreign_export("C",    json.write_number(in, uo), "WriteFloatStr").
:- pragma foreign_export("Java", json.write_string(in, in, di, uo), "WriteString").
:- pragma foreign_export("C",    json.write_string(in, in, di, uo), "WriteStringIO").
:- pragma foreign_export("Java", json.write_string(in, uo), "WriteString").
:- pragma foreign_export("C",    json.write_string(in, uo), "WriteStringStr").
:- pragma foreign_export("Java", json.write_object(in, in, di, uo), "WriteObject").
:- pragma foreign_export("C",    json.write_object(in, in, di, uo), "WriteObjectIO").
:- pragma foreign_export("Java", json.write_object(in, uo), "WriteObject").
:- pragma foreign_export("C",    json.write_object(in, uo), "WriteObjectStr").
:- pragma foreign_export("Java", json.write_array(in, in, di, uo), "WriteArray").
:- pragma foreign_export("C",    json.write_array(in, in, di, uo), "WriteArrayIO").
:- pragma foreign_export("Java", json.write_array(in, uo), "WriteArray").
:- pragma foreign_export("C",    json.write_array(in, uo), "WriteArrayStr").
:- pragma foreign_export("Java", json.write(in, in, di, uo), "WriteRoot").
:- pragma foreign_export("C",    json.write(in, in, di, uo), "WriteRootIO").
:- pragma foreign_export("Java", json.write(in, uo), "WriteRoot").
:- pragma foreign_export("C",    json.write(in, uo), "WriteRootStr").

:- pragma foreign_export("Java", json.find_object(in, in, out), "FindObject").
:- pragma foreign_export("C",    json.find_object(in, in, out), "FindObject").
:- pragma foreign_export("Java", json.find_array(in, in, out), "FindArray").
:- pragma foreign_export("C",    json.find_array(in, in, out), "FindArray").
:- pragma foreign_export("Java", json.find_integer(in, in, out), "FindInteger").
:- pragma foreign_export("C",    json.find_integer(in, in, out), "FindInteger").
:- pragma foreign_export("Java", json.find_number(in, in, out), "FindNumber").
:- pragma foreign_export("C",    json.find_number(in, in, out), "FindNumber").
:- pragma foreign_export("Java", json.find_string(in, in, out), "FindString").
:- pragma foreign_export("C",    json.find_string(in, in, out), "FindString").
:- pragma foreign_export("Java", json.find_boolean(in, in, out), "FindBoolean").
:- pragma foreign_export("C",    json.find_boolean(in, in, out), "FindBoolean").
:- pragma foreign_export("Java", json.find_null(in, in), "FindNull").
:- pragma foreign_export("C",    json.find_null(in, in), "FindNull").
:- pragma foreign_export("Java", json.find_value(in, in, out), "FindValue").
:- pragma foreign_export("C",    json.find_value(in, in, out), "FindValue").
:- pragma foreign_export("Java", json.find_property(in, in, out), "FindProperty").
:- pragma foreign_export("C",    json.find_property(in, in, out), "FindProperty").

% Finding values

json.find_object(Object, Name, json.object(O)) :-
    json.find_value(Object, Name, json.object(O)).
    
json.find_array(Object, Name, json.array(A)) :-
    json.find_value(Object, Name, json.array(A)).

json.find_integer(Object, Name, json.integer(I)) :-
    json.find_value(Object, Name, json.integer(I)).

json.find_number(Object, Name, json.number(N)) :-
    json.find_value(Object, Name, json.number(N)).

json.find_string(Object, Name, json.string(S)) :-
    json.find_value(Object, Name, json.string(S)).

json.find_boolean(Object, Name, json.boolean(B)) :-
    json.find_value(Object, Name, json.boolean(B)).

json.find_null(Object, Name) :-
    json.find_value(Object, Name, null).

json.find_value(json.object(Properties), Name, Value) :-
    json.find_value_list(Properties, Name, Value).

% Intentionally fails on an empty list. That means we didn't find the value.
json.find_value_list([Element | List], Name, Value) :-
    if Element ^ name = json.string(Name)
    then Value = Element ^ val
    else json.find_value_list(List, Name, Value).

json.find_property(json.object(Properties), Name, Property) :-
    json.find_property_list(Properties, Name, Property).

% Intentionally fails on an empty list. That means we didn't find the value.
json.find_property_list([Element | List], Name, Property) :-
    if Element ^ name = json.string(Name)
    then Property = Element
    else json.find_property_list(List, Name, Property).

% Writing functions

:- pred json.write_indent(int::in, io.output_stream::in, io::di, io::uo) is det.

json.write_indent(Step, Stream, !IO) :-
    ( if Step < 1
      then
        io.write_char(Stream, ' ', !IO)
      else
        io.write_char(Stream, ' ', !IO),
        json.write_indent(Step-1, Stream, !IO)
    ).

json.write_value(Value, Stream, !IO) :-
    (
        Value = null,
        io.write_string(Stream, "null", !IO)
    ;
        Value = json.integer(Integer),
        json.write_integer(json.integer(Integer), Stream, !IO)
    ;
        Value = json.number(Float),
        json.write_number(json.number(Float), Stream, !IO)
    ;
        Value = json.string(String),
        json.write_string(json.string(String), Stream, !IO)
    ;
        Value = json.array(Array),
        json.write_array(json.array(Array), Stream, !IO)
    ;
        Value = json.object(Object),
        json.write_object(json.object(Object), Stream, !IO)
    ;
        Value = json.boolean(Bool),
        (
            Bool = yes,
            io.write_string(Stream, "true", !IO)
        ;
            Bool = no,
            io.write_string(Stream, "false", !IO)
        )

    ).


json.write_value(Value, String) :-
    (
        Value = null,
        String = "null"
    ;
        Value = json.integer(Integer),
        json.write_integer(json.integer(Integer), String)
    ;
        Value = json.number(Float),
        json.write_number(json.number(Float), String)
    ;
        Value = json.string(S),
        json.write_string(json.string(S), String)
    ;
        Value = json.array(Array),
        json.write_array(json.array(Array), String)
    ;
        Value = json.object(Object),
        json.write_object(json.object(Object), String)
    ;
        Value = json.boolean(Bool),
        (
            Bool = yes,
            NString = "tru"
        ;
            Bool = no,
            NString = "fals"
        ),
        String = NString ++ "e"
    ).

json.write_property(Property, Stream, !IO) :-
    json.write_string(Property ^ name, Stream, !IO),
    io.write_char(Stream, ':', !IO),
    json.write_value(Property ^ val, Stream, !IO),
    io.nl(Stream, !IO).
    
json.write_property(Property, String) :-
    json.write_string(Property ^ name, NameString),
    json.write_value(Property ^ val, ValString),
    String = NameString ++ ":" ++ ValString ++ "\n".

json.write_integer(JSInteger, Stream, !IO) :-
    JSInteger = json.integer(I),
    io.write_int(Stream, I, !IO).

json.write_integer(json.integer(I), string.from_int(I)).

json.write_number(JSNumber, Stream, !IO) :-
    JSNumber = json.number(N),
    io.write_float(Stream, N, !IO).

json.write_number(json.number(F), string.from_float(F)).

json.write_string(JSString, Stream, !IO) :-
    JSString = json.string(String),
    io.write_char(Stream, '"', !IO),
    io.write_string(Stream, String, !IO),
    io.write_char(Stream, '"', !IO).

json.write_string(json.string(String), "\"" ++ String ++ "\"").

:- pred json.write_array_element(json.value::in, io.output_stream::in, io.output_stream::out, io::di, io::uo) is det.
:- pred json.write_array_element(json.value::in, int::in, int::out, io.output_stream::in, io.output_stream::out, io::di, io::uo) is det.

:- pred json.write_array_element_str(json.value::in, string::in, string::out) is det.
:- pred json.write_array_element_str(json.value::in, int::in, int::out, string::in, string::out) is det.

json.write_array_element(Value, Stream, Out, !IO) :-
    json.write_value(Value, Stream, !IO),
    io.write_char(Stream, ',', !IO),
    Out = Stream.

json.write_array_element_str(Value, StringIn, StringOut) :-
    json.write_value(Value, ValString),
    StringOut = StringIn ++ ValString ++ ",".

json.write_array_element(Value, Indent, IndentOut, Stream, StreamOut, !IO) :-
    json.write_indent(Indent, Stream, !IO),
    json.write_value(Value, Stream, !IO),
    io.write_char(Stream, ',', !IO),
    io.nl(Stream, !IO),
    StreamOut = Stream,
    IndentOut = Indent.
    
json.write_array_element_str(Value, Indent, IndentOut, StringIn, StringOut) :-
    json.write_value(Value, ValString),
    StringOut = StringIn ++ string.pad_left(ValString, ' ', Indent) ++ ",\n",
    IndentOut = Indent.

:- pred json.write_object_element(json.property::in, io.output_stream::in, io.output_stream::out, io::di, io::uo) is det.
:- pred json.write_object_element(json.property::in, int::in, int::out, io.output_stream::in, io.output_stream::out, io::di, io::uo) is det.

:- pred json.write_object_element_str(json.property::in, string::in, string::out) is det.
:- pred json.write_object_element_str(json.property::in, int::in, int::out, string::in, string::out) is det.

json.write_object_element(Property,Stream, Out, !IO) :-
    json.write_property(Property, Stream, !IO),
    io.write_char(Stream, ',', !IO),
     Out = Stream.

json.write_object_element_str(Value, StringIn, StringOut) :-
    json.write_property(Value, ValString),
    StringOut = StringIn ++ ValString ++ ",".

json.write_object_element(Property, Indent, IndentOut, Stream, StreamOut, !IO) :-
    json.write_indent(Indent, Stream, !IO),
    json.write_property(Property, Stream, !IO),
    io.write_char(Stream, ',', !IO),
    io.nl(Stream, !IO),
    StreamOut = Stream,
    IndentOut = Indent.

json.write_object_element_str(Value, Indent, IndentOut, StringIn, StringOut) :-
    json.write_property(Value, ValString),
    StringOut = StringIn ++ string.pad_left(ValString, ' ', Indent) ++ ",\n",
    IndentOut = Indent.

json.write_array(JSArray, Stream, !IO) :-
    JSArray = json.array(Array),
    ( if list.split_last(Array, NewArray, Last)
      then
        io.write_char(Stream, '[', !IO),
        list.foldl2(json.write_array_element, NewArray, Stream, _, !IO),
        json.write_value(Last, Stream, !IO),
        io.write_char(Stream, ']', !IO)
      else
        io.write_char(Stream, '[', !IO), io.write_char(Stream, ']', !IO)
    ),
    io.nl(Stream, !IO).


json.write_array(JSArray, String) :-
    JSArray = json.array(Array),
    ( if list.split_last(Array, NewArray, Last)
      then
        list.foldl(json.write_array_element_str, NewArray, "[", NewString),
        json.write_value(Last, ValString),
        String = NewString ++ ValString ++ "]\n"
      else
        String = "[]\n"
    ).

json.write_array(JSArray, Step, Stream, !IO) :-
    JSArray = json.array(Array),
    json.write_indent(Step, Stream, !IO),
    ( if list.split_last(Array, NewArray, Last)
      then
        io.write_char(Stream, '[', !IO),
        list.foldl3(json.write_array_element, NewArray, Step+1, _, Stream, _, !IO),
        json.write_indent(Step, Stream, !IO),
        json.write_value(Last, Stream, !IO),
        json.write_indent(Step, Stream, !IO),
        io.write_char(Stream, ']', !IO)
      else
        io.write_char(Stream, '[', !IO), io.write_char(Stream, ']', !IO)
    ),
    io.nl(Stream, !IO).

json.write_array(JSArray, Step, String) :-
    JSArray = json.array(Array),
    ( if list.split_last(Array, NewArray, Last)
      then
        list.foldl2(json.write_array_element_str, NewArray, Step+1, _, string.pad_left("[", ' ', Step), NewString),
        json.write_value(Last, ValString),
        String = NewString ++ string.pad_right(string.pad_left(ValString, ' ', Step) ++ "\n", ' ', Step) ++ "]\n"
      else
        String = string.duplicate_char(' ', Step) ++ "[]\n"
    ).

json.write_object(JSObject, Stream, !IO) :-
    JSObject = json.object(Object),
    ( if list.split_last(Object, NewObject, Last)
      then
        io.write_char(Stream, '{', !IO),
        list.foldl2(json.write_object_element, NewObject, Stream, _, !IO),
        json.write_property(Last, Stream, !IO),
        io.write_char(Stream, '}', !IO)
      else
        io.write_char(Stream, '{', !IO), io.write_char(Stream, '}', !IO)
    ),
    io.nl(Stream, !IO).
    
json.write_object(JSObject, String) :-
    JSObject = json.object(Object),
    ( if list.split_last(Object, NewObject, Last)
      then
        list.foldl(json.write_object_element_str, NewObject, "{", NewString),
        json.write_property(Last, PropString),
        String = NewString ++ PropString ++ "}\n"
      else
        String = "{}\n"
    ).

json.write_object(JSObject, Step, Stream, !IO) :-
    JSObject = json.object(Object),
    json.write_indent(Step, Stream, !IO),
    ( if list.split_last(Object, NewObject, Last)
      then
        io.write_char(Stream, '{', !IO),
        list.foldl3(json.write_object_element, NewObject, Step+1, _, Stream, _, !IO),
        json.write_indent(Step, Stream, !IO),
        json.write_property(Last, Stream, !IO),
        json.write_indent(Step, Stream, !IO),
        io.write_char(Stream, '}', !IO)
      else
        io.write_char(Stream, '{', !IO), io.write_char(Stream, '}', !IO)
    ),
    io.write_char(Stream, ',', !IO),
    io.nl(Stream, !IO).

json.write_object(JSObject, Step, String) :-
    JSObject = json.object(Object),
    ( if list.split_last(Object, NewObject, Last)
      then
        list.foldl2(json.write_object_element_str, NewObject, Step+1, _, string.pad_left("[", ' ', Step), NewString),
        json.write_property(Last, PropString),
        String = NewString ++ string.pad_right(string.pad_left(PropString, ' ', Step) ++ "\n", ' ', Step) ++ "]\n"
      else
        String = string.duplicate_char(' ', Step) ++ "{}\n"
    ).

json.write(Result, Stream, !IO) :-
    (
        Result = null,
        io.write_string(Stream, "null", !IO)
    ;
        Result = json.array(A),
        json.write_array(json.array(A), Stream, !IO)
    ;
        Result = json.object(A),
        json.write_object(json.object(A), Stream, !IO)
    ),
    io.nl(Stream, !IO).

json.write(Result, String) :-
    (
        Result = null,
        String = "null"
    ;
        Result = json.array(A),
        json.write_array(json.array(A), String)
    ;
        Result = json.object(A),
        json.write_object(json.object(A), String)
    ).

json.write_pretty(Result, Stream, !IO) :-
    (
        Result = null,
        io.write_string(Stream, "null", !IO)
    ;
        Result = json.array(A),
        json.write_array(json.array(A), 0, Stream, !IO)
    ;
        Result = json.object(A),
        json.write_object(json.object(A), 0, Stream, !IO)
    ),
    io.nl(Stream, !IO).

json.write_pretty(Result, String) :-
    (
        Result = null,
        String = "null\n"
    ;
        Result = json.array(A),
        json.write_array(json.array(A), 0, String)
    ;
        Result = json.object(A),
        json.write_object(json.object(A), 0, String)
    ).

:- import_module char.
:- import_module int.
:- import_module float.
:- import_module string.

:- pred json.char_is_breaking(char::in) is semidet.
:- pred json.char_is_space(char::in) is semidet.
:- pred json.char_is_quote(char::in) is semidet.

json.char_is_quote('\'').
json.char_is_quote('"').

json.char_is_space(' ').
json.char_is_space('\t').
json.char_is_space('\n').
json.char_is_space('\r').

json.char_is_breaking(' ').
json.char_is_breaking('\t').
json.char_is_breaking('\n').
json.char_is_breaking('\r').
json.char_is_breaking('(').
json.char_is_breaking(')').
json.char_is_breaking('{').
json.char_is_breaking('}').
json.char_is_breaking('[').
json.char_is_breaking(']').
json.char_is_breaking('"').
json.char_is_breaking('\'').
json.char_is_breaking(',').
json.char_is_breaking(':').

:- pred json.is_negative_sign(char::in) is semidet.

json.is_negative_sign('-').

:- pred json.get_space_end(string::in, int::in, int::in, int::out) is det.

json.get_space_end(String, Index, Max, End) :-
    if Index=Max
    then
        End = Index
    else if string.unsafe_index(String, Index, C), char_is_space(C)
    then
        json.get_space_end(String, Index+1, Max, End)
    else
        End = Index.

:- pred json.char_digit_hex(char::in, int::out) is semidet.
:- pred json.char_digit_dec(char::in, int::out) is semidet.
:- pred json.char_digit_oct(char::in, int::out) is semidet.

:- pred json.parse_integer(string, int, int, int, pred(char, int, int), int, int).
:- mode json.parse_integer(in, in, out, in, pred(in, in, out) is semidet, in, out) is semidet.

% Generally unsafe. You must ensure that the first param translates to a single
%  digit in the base.
:- pred json.append_digit_hex(int::in, int::in, int::out) is det.
:- pred json.append_digit_dec(int::in, int::in, int::out) is det.
:- pred json.append_digit_oct(int::in, int::in, int::out) is det.

:- pred json.parse_digit_hex(char::in, int::in, int::out) is semidet.
:- pred json.parse_digit_dec(char::in, int::in, int::out) is semidet.
:- pred json.parse_digit_oct(char::in, int::in, int::out) is semidet.

json.char_digit_hex('0', 0).
json.char_digit_hex('1', 1).
json.char_digit_hex('2', 2).
json.char_digit_hex('3', 3).
json.char_digit_hex('4', 4).
json.char_digit_hex('5', 5).
json.char_digit_hex('6', 6).
json.char_digit_hex('7', 7).
json.char_digit_hex('8', 8).
json.char_digit_hex('9', 9).
json.char_digit_hex('A', 10).
json.char_digit_hex('a', 10).
json.char_digit_hex('B', 11).
json.char_digit_hex('b', 11).
json.char_digit_hex('C', 12).
json.char_digit_hex('c', 12).
json.char_digit_hex('D', 13).
json.char_digit_hex('d', 13).
json.char_digit_hex('E', 14).
json.char_digit_hex('e', 14).
json.char_digit_hex('F', 15).
json.char_digit_hex('f', 15).

json.char_digit_dec('0', 0).
json.char_digit_dec('1', 1).
json.char_digit_dec('2', 2).
json.char_digit_dec('3', 3).
json.char_digit_dec('4', 4).
json.char_digit_dec('5', 5).
json.char_digit_dec('6', 6).
json.char_digit_dec('7', 7).
json.char_digit_dec('8', 8).
json.char_digit_dec('9', 9).

json.char_digit_oct('0', 0).
json.char_digit_oct('1', 1).
json.char_digit_oct('2', 2).
json.char_digit_oct('3', 3).
json.char_digit_oct('4', 4).
json.char_digit_oct('5', 5).
json.char_digit_oct('6', 6).
json.char_digit_oct('7', 7).

json.append_digit_hex(X, N, (N*16)+X).
json.append_digit_dec(X, N, (N*10)+X).
json.append_digit_oct(X, N, (N*8) +X).

json.parse_digit_hex(Char::in, N::in, Out::out) :-
    json.char_digit_hex(Char, X),
    json.append_digit_hex(X, N, Out).

json.parse_digit_dec(Char::in, N::in, Out::out) :-
    json.char_digit_dec(Char, X),
    json.append_digit_dec(X, N, Out).

json.parse_digit_oct(Char::in, N::in, Out::out) :-
    json.char_digit_oct(Char, X),
    json.append_digit_oct(X, N, Out).

json.parse_integer(String, Index, End, Max, Op, In, Out) :-
    Index<Max,
    string.unsafe_index(String, Index, C),
    ( if Op(C, In, X)
      then
        json.parse_integer(String, Index+1, End, Max, Op, X, Out)
      else
        json.char_is_breaking(C),
        Out = In,
        End = Index
    ).
    
json.parse_integer(String::in, Start::in, End::out, Max::in, JSInteger::out) :-
    ( if Start<Max,
        ( if string.unsafe_index(String, Start, '0')
          then
            ( if Start<Max-2, ( string.unsafe_index(String, Start+1, 'x') ; string.unsafe_index(String, Start+1, 'X') )
              then
                json.parse_integer(String, Start+2, IntEnd, Max, json.parse_digit_hex, 0, N)
              else
                json.parse_integer(String, Start+1, IntEnd, Max, json.parse_digit_oct, 0, N)
            )
          else 
            json.parse_integer(String, Start, IntEnd, Max, json.parse_digit_dec, 0, N)
        )
      then
        json.get_space_end(String, IntEnd, Max, End),
        JSInteger = ok(json.integer(N))
      else
        JSInteger = json.error(0, Start, "Number", string.between(String, Start, End)),
        End-1 = Start
    ).

json.parse_number(String::in, Start::in, End::out, Max::in, JSNumber::out) :-
    ( if
        json.parse_integer(String, Start, WholeEnd, Max, json.parse_digit_dec, 0, W),
        Max>WholeEnd,
        string.unsafe_index(String, WholeEnd, '.'),
        json.parse_integer(String, WholeEnd+1, DecimalEnd, Max, json.parse_digit_dec, 0, D),
        M = float(D)/float(WholeEnd+1-DecimalEnd)
      then
        json.get_space_end(String, DecimalEnd, Max, End),
        JSNumber = ok(json.number(float(W) + M))
      else
        JSNumber = json.error(0, Start, "Number", string.between(String, Start, End)),
        End-1 = Start
    ).
    

:- pred json.find_string_end(string::in, char::in, int::in, int::in, int::out) is semidet.

json.find_string_end(String::in, C::in, Start::in, Max::in, End::out) :-
    Start<Max,
    ( if string.unsafe_index(String, Start, C)
      then 
        End = Start
      else if string.unsafe_index(String, Start, '\\')
      then
        json.find_string_end(String, C, Start+2, Max, End)
      else
        json.find_string_end(String, C, Start+1, Max, End)
    ).

json.parse_string(String::in, Start::in, End::out, Max::in, JSString::out) :-
    ( if Start<Max, string.unsafe_index(String, Start, C), json.char_is_quote(C)
      then
        ( if json.find_string_end(String, C, Start+1, Max, LiteralEnd), LiteralEnd<Max
          then
            string.unsafe_between(String, Start+1, LiteralEnd, OutString),
            JSString = ok(json.string(OutString)),
            json.get_space_end(String, LiteralEnd+1, Max, End)
          else
            JSString = json.error(0, Start, "String Literal", string.between(String, Start, End)),
            End-1 = Start
        )
      else
        JSString = json.error(0, Start, "Open Quote", string.between(String, Start, End)),
        End-1 = Start
    ).

:- pred json.array_element(string::in, int::in, int::out, int::in, list(json.value)::in, json.result(list(json.value))::out) is det.

json.array_element(String::in, Start::in, End::out, Max::in, ArrayIn::in, ArrayOut::out) :-
    json.parse_value(String, Start, TermEnd, Max, ValueResult), 
    (
        ValueResult = json.error(L, At, E, U), ArrayOut = json.error(L, At, E, U),
        End = TermEnd
    ;   
        ValueResult = ok(Value),
        json.get_space_end(String, TermEnd, Max, SpaceEnd),
         % Parse another term?
        ( if SpaceEnd<Max, string.unsafe_index(String, SpaceEnd, ',')
          then
            json.get_space_end(String, SpaceEnd+1, Max, NextTermStart),
            json.array_element(String, NextTermStart, End, Max, ArrayIn++[Value], ArrayOut)
          else
            ArrayOut = ok(ArrayIn++[Value]),
            End = SpaceEnd
        )
    ).

:- pred json.object_element(string::in, int::in, int::out, int::in, list(json.property)::in, json.result(list(json.property))::out) is det.

json.object_element(String::in, Start::in, End::out, Max::in, PropertiesIn::in, PropertiesOut::out) :-

    json.parse_property(String, Start, TermEnd, Max, PropertyResult),
    (
        PropertyResult = json.error(L, At, E, U), PropertiesOut = json.error(L, At, E, U),
        End = TermEnd
    ;   
        PropertyResult = ok(Property),
        json.get_space_end(String, TermEnd, Max, SpaceEnd),
        % Parse another property?
        ( if SpaceEnd<Max, string.unsafe_index(String, SpaceEnd, ',')
          then
            json.get_space_end(String, SpaceEnd+1, Max, NextTermStart),
            json.object_element(String, NextTermStart, End, Max, PropertiesIn++[Property], PropertiesOut)
          else
            PropertiesOut = ok(PropertiesIn++[Property]),
            End = SpaceEnd
        )
    ).

json.parse_array(String::in, Start::in, End::out, Max::in, Array::out) :-
    ( if Start<Max, not string.unsafe_index(String, Start, '[')
      then
        Array = json.error(0, Start, "[", string.between(String, Start, End)),
        End-1 = Start
      else
        json.get_space_end(String, Start+1, Max, TermStart),
        ( if TermStart<Max, not string.unsafe_index(String, TermStart, ']')
          then
            json.array_element(String, TermStart, ListEnd, Max, [], ValueArrayResult)
          else
            ListEnd = TermStart,
            ValueArrayResult = ok([])
        ),
        (
            ValueArrayResult = json.error(L, At, E, U), Array = json.error(L, At, E, U),
            End = TermStart
        ;
            ValueArrayResult = ok(ValueArray),
            json.get_space_end(String, ListEnd, Max, ArrayEnd),
            ( if ArrayEnd<Max, not string.unsafe_index(String, ArrayEnd, ']')
              then
                Array = json.error(0, ArrayEnd, "]", string.between(String, ArrayEnd, End)),
                End-1 = ArrayEnd
              else
                json.get_space_end(String, ArrayEnd+1, Max, End),
                Array = ok(json.array(ValueArray))
            )
        )
    ).

json.parse_object(String::in, Start::in, End::out, Max::in, Object::out) :-
    ( if Start<Max, not string.unsafe_index(String, Start, '{')
      then
        Object = json.error(0, Start, "{", string.between(String, Start, End)),
        End-1 = Start
      else
        json.get_space_end(String, Start+1, Max, TermStart),
        ( if TermStart<Max, string.unsafe_index(String, TermStart, '}')
          then
            ListEnd = TermStart,
            PropertiesResult = ok([])
          else
            json.object_element(String, TermStart, ListEnd, Max, [], PropertiesResult)
        ),
        (
            PropertiesResult = json.error(L, At, E, U), Object = json.error(L, At, E, U),
            End = TermStart
        ;
            PropertiesResult = ok(Properties),
            json.get_space_end(String, ListEnd, Max, CloseBracket),
            ( if CloseBracket<Max, string.unsafe_index(String, CloseBracket, '}')
              then
                Object = ok(json.object(Properties)),
                json.get_space_end(String, CloseBracket+1, Max, End)
              else
                Object = json.error(0, CloseBracket, "}", string.between(String, CloseBracket, End)),
                End-1 = CloseBracket
            )
        )
    ).
    
:- pred json.number_literal(string::in, int::in, int::out, int::in, json.result(json.value)::out) is det.

json.number_literal(String::in, Start::in, End::out, Max::in, Value::out) :-
    ( if Start<Max, string.unsafe_index(String, 0, '-')
      then
        C = -1,
        TermStart = Start+1
      else
        C = 1,
        TermStart = Start
    ),
    ( if json.parse_number(String, TermStart, T, Max, ok(json.number(N)))
      then
        Value = ok(json.number(N * float(C))),
        json.get_space_end(String, T, Max, End)
      else if json.parse_integer(String, TermStart, T, Max, ok(json.integer(I)))
      then
        Value = ok(json.integer(I * C)),
        json.get_space_end(String, T, Max, End)
      else
        Value = json.error(0, Start, "Number Literal", string.between(String, Start, End)),
        End-1 = Start
    ).

:- pred json.char_is_array_start(char::in) is semidet.
json.char_is_array_start('[').

:- pred json.char_is_object_start(char::in) is semidet.
json.char_is_object_start('{').

json.parse_property(String::in, Start::in, End::out, Max::in, Property::out) :-
    json.parse_string(String, Start, NameEnd, Max, NameResult),
    (
        NameResult = json.error(L, At, E, U), Property = json.error(L, At, E, U),
        End = NameEnd
    ;
        NameResult = ok(Name),
        json.get_space_end(String, NameEnd, Max, DelimiterStart),
        ( if DelimiterStart<Max, not string.unsafe_index(String, DelimiterStart, ':')
          then
            Property = json.error(0, DelimiterStart, ":", string.between(String, DelimiterStart, End)),
            End-1 = DelimiterStart
          else
            json.get_space_end(String, DelimiterStart+1, Max, TermStart),
            json.parse_value(String, TermStart, TermEnd, Max, ValueResult),
            (
                ValueResult = json.error(L, At, E, U), Property = json.error(L, At, E, U),
                End = TermEnd
            ;
                ValueResult = ok(Value), Property = ok(json.property(Name, Value)),
                json.get_space_end(String, TermEnd, Max, End)
            )
       )
    ).

json.parse_value(String::in, Start::in, End::out, Max::in, Result::out) :-
    ( if Start<Max, string.unsafe_index(String, Start, C)
      then
        ( if json.is_negative_sign(C) ; json.char_digit_dec(C, _)
          then 
            json.number_literal(String, Start, TermEnd, Max, Value)
          else if json.char_is_quote(C)
          then % String
            json.parse_string(String, Start, TermEnd, Max, S),
            (
                S = ok(json.string(T)), Value = ok(json.string(T))
            ;
                S = json.error(L1, At1, E1, U1), Value = json.error(L1, At1, E1, U1)
            )
          else if json.char_is_array_start(C)
          then % Array
            json.parse_array(String, Start, TermEnd, Max, A),
            (
                A = ok(json.array(R)), Value = ok(json.array(R))
            ;
                A = json.error(L2, At2, E2, U2), Value = json.error(L2, At2, E2, U2)
            )
          else if json.char_is_object_start(C)
          then % Object
            json.parse_object(String, Start, TermEnd, Max, O),
            (
                O = ok(json.object(J)), Value = ok(json.object(J))
            ;
                O = json.error(L3, At3, E3, U3), Value = json.error(L3, At3, E3, U3)
            )
          else if Start+4<Max, string.unsafe_between(String, Start, Start+4, "true")
          then % Boolean literal "true"
            Value = ok(json.boolean(yes)),
            TermEnd-4 = Start
          else if Start+5<Max, string.unsafe_between(String, Start, Start+5, "false")
          then % Boolean literal "false"
            Value = ok(json.boolean(no)),
            TermEnd-5 = Start
          else if Start+4<Max, string.unsafe_between(String, Start, Start+4, "null")
          then % Object literal "null"
            Value = ok(null),
            TermEnd-4 = Start
          else
            TermEnd-1 = Start,
            Value = ok(null)
        ),
        (
            Value = ok(K), Result = ok(K)
        ;
            Value = json.error(L4, At4, E4, U4), Result = json.error(L4, At4, E4, U4)
        ),
        json.get_space_end(String, TermEnd, Max, End)
      else
        % Line really IS zero here.
        Result = json.error(0, Start, "", "End of input"),
        End-1 = Start
    ).

json.parse(String::in, Result::out) :-
    json.parse_value(String, 0, _, string.length(String), ValueResult),
    (
        ValueResult = json.error(L, At, E, U), Result = json.error(L, At, E, U)
    ;
        ValueResult = ok(Value),
        (
            Value = json.array(A),
            Result = ok(json.array(A))
        ;
            Value = json.object(A),
            Result = ok(json.object(A))
        ;
            Value = null,
            Result = ok(null)
        ; 
            (
                Value = json.boolean(_)
            ;
                Value = json.number(_)
            ;
                Value = json.integer(_)
            ;
                Value = json.string(_)
            ),
            Result = json.error(0, 0, "Object or Array", "")
        )
    ).
