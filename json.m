:- module json.
:- interface.

:- import_module bool.
:- import_module list.

:- type json.value.
:- type json.property.

% `null' is technically an object, but it is MUCH simpler to implement
%  it as a possible state of json.value
:- type json.integer ---> json.integer(int).
:- type json.number  ---> json.number(float).
:- type json.string  ---> json.string(string).
:- type json.boolean ---> json.boolean(bool).
:- type json.array   ---> json.array(list(json.value)).
:- type json.object  ---> json.object(list(json.property)).

:- type json.property ---> json.property(name::json.string, val::json.value).

:- type json.value --->
    null
    ; json.object(list(json.property))
    ; json.array(list(json.value))
    ; json.integer(int)
    ; json.number(float)
    ; json.string(string)
    ; json.boolean(bool).

% TODO: Is null really ok?
:- type json.result --->
    null
    ; json.object(list(json.property))
    ; json.array(list(json.value)).

% Parsing

:- pred json.parse_value(string::in, int::in, int::out, json.value::out) is semidet.
:- pred json.parse_property(string::in, int::in, int::out, json.property::out) is semidet.

:- pred json.parse_integer(string::in, int::in, int::out, json.integer::out) is semidet.
:- pred json.parse_number(string::in, int::in, int::out, json.number::out) is semidet.
:- pred json.parse_string(string::in, int::in, int::out, json.string::out) is semidet.

:- pred json.parse_array(string::in, int::in, int::out, json.array::out) is semidet.
:- pred json.parse_object(string::in, int::in, int::out, json.object::out) is semidet.

:- pred json.parse(string::in, json.result::out) is semidet.

% Writing

:- import_module io.

% The predicates that have an extra integer parameter are for pretty printing. Otherwise, it's dumped to file.
% Faster and simpler, but not as nice looking.
% Invoked using write_pretty, as opposed to write
:- pred json.write_value(json.value::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_property(json.property::in, io.output_stream::in, io::di, io::uo) is det.

:- pred json.write_integer(json.integer::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_number(json.number::in, io.output_stream::in, io::di, io::uo) is det.

:- pred json.write_string(json.string::in, io.output_stream::in, io::di, io::uo) is det.

:- pred json.write_array(json.array::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_array(json.array::in, int::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_object(json.object::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_object(json.object::in, int::in, io.output_stream::in, io::di, io::uo) is det.

:- pred json.write(json.result::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_pretty(json.result::in, io.output_stream::in, io::di, io::uo) is det.

% Implementation
:- implementation.

:- import_module stream.string_writer.

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

json.write_property(Property, Stream, !IO) :-
    json.write_string(Property ^ name, Stream, !IO),
    io.write_char(Stream, ':', !IO),
    json.write_value(Property ^ val, Stream, !IO),
    io.nl(Stream, !IO).

json.write_integer(JSInteger, Stream, !IO) :-
    JSInteger = json.integer(I),
    io.write_int(Stream, I, !IO).

json.write_number(JSNumber, Stream, !IO) :-
    JSNumber = json.number(N),
    io.write_float(Stream, N, !IO).

json.write_string(JSString, Stream, !IO) :-
    JSString = json.string(String),
    io.write_char(Stream, '"', !IO),
    io.write_string(Stream, String, !IO),
    io.write_char(Stream, '"', !IO).

:- pred json.write_array_element(json.value::in, io.output_stream::in, io.output_stream::out, io::di, io::uo) is det.
:- pred json.write_array_element(json.value::in, int::in, int::out, io.output_stream::in, io.output_stream::out, io::di, io::uo) is det.

json.write_array_element(Value, Stream, Out, !IO) :-
    json.write_value(Value, Stream, !IO),
    io.write_char(Stream, ',', !IO),
    io.write_char(Stream, ' ', !IO),
    Out = Stream.

json.write_array_element(Value, Indent, IndentOut, Stream, StreamOut, !IO) :-
    json.write_indent(Indent, Stream, !IO),
    json.write_value(Value, Stream, !IO),
    io.write_char(Stream, ',', !IO),
    io.nl(Stream, !IO),
    StreamOut = Stream,
    IndentOut = Indent.

:- pred json.write_object_element(json.property::in, io.output_stream::in, io.output_stream::out, io::di, io::uo) is det.
:- pred json.write_object_element(json.property::in, int::in, int::out, io.output_stream::in, io.output_stream::out, io::di, io::uo) is det.

json.write_object_element(Property, Stream, Out, !IO) :-
    json.write_property(Property, Stream, !IO),
    io.write_char(Stream, ',', !IO),
    io.write_char(Stream, ' ', !IO),
    Out = Stream.

json.write_object_element(Property, Indent, IndentOut, Stream, StreamOut, !IO) :-
    json.write_indent(Indent, Stream, !IO),
    json.write_property(Property, Stream, !IO),
    io.write_char(Stream, ',', !IO),
    io.nl(Stream, !IO),
    StreamOut = Stream,
    IndentOut = Indent.
     

json.write_array(JSArray, Stream, !IO) :-
    JSArray = json.array(Array),
    io.write_char(Stream, '[', !IO),
    list.foldl2(json.write_array_element, Array, Stream, _, !IO),
    io.write_char(Stream, ']', !IO),
    io.nl(Stream, !IO).

json.write_array(JSArray, Step, Stream, !IO) :-
    JSArray = json.array(Array),
    json.write_indent(Step, Stream, !IO),
    io.write_char(Stream, '[', !IO),
    list.foldl3(json.write_array_element, Array, Step+1, _, Stream, _, !IO),
    json.write_indent(Step, Stream, !IO),
    io.write_char(Stream, ']', !IO),
    io.nl(Stream, !IO).

json.write_object(JSObject, Stream, !IO) :-
    JSObject = json.object(Object),
    io.write_char(Stream, '{', !IO),
    list.foldl2(json.write_object_element, Object, Stream, _, !IO),
    io.write_char(Stream, '}', !IO),
    io.nl(Stream, !IO).

json.write_object(JSObject, Step, Stream, !IO) :-
    JSObject = json.object(Object),
    json.write_indent(Step, Stream, !IO),
    io.write_char(Stream, '{', !IO),
    list.foldl3(json.write_object_element, Object, Step+1, _, Stream, _, !IO),
    json.write_indent(Step, Stream, !IO),
    io.write_char(Stream, '}', !IO),
    io.nl(Stream, !IO).

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

:- pred json.get_space_end(string::in, int::in, int::out) is semidet.

json.get_space_end(String, Index, End) :-
    if string.index(String, Index, C), char_is_space(C)
    then
        End = Index
    else
        json.get_space_end(String, Index+1, End).

:- pred json.char_digit_hex(char::in, int::out) is semidet.
:- pred json.char_digit_dec(char::in, int::out) is semidet.
:- pred json.char_digit_oct(char::in, int::out) is semidet.

:- pred json.parse_integer(string, int, int, pred(char, int, int), int, int).
:- mode json.parse_integer(in, in, out, pred(in, in, out) is semidet, in, out) is semidet.

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

json.parse_integer(String, Index, End, Op, In, Out) :-
    ( if string.index(String, Index, C)
      then
        ( if Op(C, In, X)
          then
            json.parse_integer(String, Index+1, End, Op, X, Out)
          else
            json.char_is_breaking(C),
            Out = In,
            End = Index
        )
      else
        Out = In,
        End = Index
    ).

json.parse_integer(String::in, Start::in, End::out, JSInteger::out) :-
    ( if string.index(String, Start, '0')
      then
        ( if string.index(String, Start+1, 'x') ; string.index(String, Start+1, 'X')
          then
            json.parse_integer(String, Start+2, IntEnd, json.parse_digit_hex, 0, N)
          else
            json.parse_integer(String, Start+1, IntEnd, json.parse_digit_oct, 0, N)
        )
      else 
        json.parse_integer(String, Start, IntEnd, json.parse_digit_dec, 0, N)
    ),
    json.get_space_end(String, IntEnd, End),
    JSInteger = json.integer(N).

json.parse_number(String::in, Start::in, End::out, JSNumber::out) :-
    json.parse_integer(String, Start, WholeEnd, json.parse_digit_dec, 0, W),
    string.index(String, WholeEnd, '.'),
    json.parse_integer(String, WholeEnd+1, DecimalEnd, json.parse_digit_dec, 0, D),
    M = float(D)/float(WholeEnd+1-DecimalEnd),
    json.get_space_end(String, DecimalEnd, End),
    JSNumber = json.number(float(W) + M).

:- pred json.find_string_end(string::in, char::in, int::in, int::out) is semidet.

json.find_string_end(String::in, C::in, Start::in, End::out) :-
    ( if string.index(String, Start, C)
      then 
        End = Start
      else if string.index(String, Start, '\\')
      then
        json.find_string_end(String, C, Start+2, End)
      else
        json.find_string_end(String, C, Start+1, End)
    ).

json.parse_string(String::in, Start::in, End::out, JSString::out) :-

    string.index(String, Start, C),
    json.char_is_quote(C),
    json.find_string_end(String, C, Start, LiteralEnd),
    string.index(String, LiteralEnd, C),

    string.between(String, Start+1, LiteralEnd, OutString),
    JSString = json.string(OutString),
    json.get_space_end(String, LiteralEnd+1, End).

:- pred json.array_element(string::in, int::in, int::out, list(json.value)::in, list(json.value)::out) is semidet.

json.array_element(String::in, Start::in, End::out, ArrayIn::in, ArrayOut::out) :-
    json.parse_value(String, Start, TermEnd, Value),
    
    % Parse another term?

    json.get_space_end(String, TermEnd, SpaceEnd),
    ( if string.index(String, SpaceEnd, ',')
      then
        json.get_space_end(String, SpaceEnd+1, NextTermStart),
        json.array_element(String, NextTermStart, End, list.cons(Value, ArrayIn), ArrayOut)
      else
        list.cons(Value, ArrayIn, ArrayOut),
        End = SpaceEnd
    ).

:- pred json.object_element(string::in, int::in, int::out, list(json.property)::in, list(json.property)::out) is semidet.

json.object_element(String::in, Start::in, End::out, PropertiesIn::in, PropertiesOut::out) :-

    json.parse_property(String, Start, TermEnd, Property),

    % Parse another property?

    json.get_space_end(String, TermEnd, SpaceEnd),
    ( if string.index(String, SpaceEnd, ',')
      then
        json.get_space_end(String, SpaceEnd+1, NextTermStart),
        json.object_element(String, NextTermStart, End, list.cons(Property, PropertiesIn), PropertiesOut)
      else
        list.cons(Property, PropertiesIn, PropertiesOut),
        End = SpaceEnd
    ).

json.parse_array(String::in, Start::in, End::out, Array::out) :-
    string.index(String, Start, '['),

    json.get_space_end(String, Start+1, TermStart),
    ( if string.index(String, TermStart, ']')
      then
        ListEnd = TermStart,
        ValueArray = []
      else
        json.array_element(String, TermStart, ListEnd, [], ValueArray)
    ),
    Array = json.array(ValueArray),
    json.get_space_end(String, ListEnd, ArrayEnd),
    string.index(String, ArrayEnd, ']'),
    json.get_space_end(String, ArrayEnd+1, End).

json.parse_object(String::in, Start::in, End::out, Object::out) :-
    string.index(String, Start, '{'),
    json.get_space_end(String, Start+1, TermStart),
    ( if string.index(String, TermStart, '}')
      then
        ListEnd = TermStart,
        Properties = []
      else
        json.object_element(String, TermStart, ListEnd, [], Properties)
    ),
    Object = json.object(Properties),

    json.get_space_end(String, ListEnd, CloseBracket),
    string.index(String, CloseBracket, '}'),
    json.get_space_end(String, CloseBracket+1, End).



:- pred json.char_is_array_start(char::in) is semidet.
json.char_is_array_start('[').

:- pred json.char_is_object_start(char::in) is semidet.
json.char_is_object_start('{').

json.parse_property(String::in, Start::in, End::out, Property::out) :-
    json.parse_string(String, Start, NameEnd, Name),
    json.get_space_end(String, NameEnd, DelimiterStart),
    string.index(String, DelimiterStart, ':'),
    json.get_space_end(String, DelimiterStart+1, TermStart),
    json.parse_value(String, TermStart, TermEnd, Value),
    Property = json.property(Name, Value),

    json.get_space_end(String, TermEnd, End).
    
:- pred json.number_literal(string::in, int::in, int::out, json.value::out) is semidet.

json.number_literal(String::in, Start::in, End::out, Value::out) :-
    ( if string.index(String, 0, '-')
      then
        C = -1,
        TermStart = Start+1
      else
        C = 1,
        TermStart = Start
    ),
    ( if json.parse_number(String, TermStart, T, json.number(N))
      then
        Value = json.number(N * float(C)),
        TermEnd = T
      else
        json.parse_integer(String, TermStart, TermEnd, json.integer(I)),
        Value = json.integer(I * C)
    ),    
    json.get_space_end(String, TermEnd, End).

json.parse_value(String::in, Start::in, End::out, Value::out) :-
    string.index(String, Start, C),
    ( if json.is_negative_sign(C) ; json.char_digit_dec(C, _)
      then 
        json.number_literal(String, Start, TermEnd, Value)
      else if json.char_is_quote(C)
      then % String
        json.parse_string(String, Start, TermEnd, json.string(S)),
        Value = json.string(S)
      else if json.char_is_array_start(C)
      then % Array
        json.parse_array(String, Start, TermEnd, json.array(A)),
        Value = json.array(A)
      else if json.char_is_object_start(C)
      then % Object
        json.parse_object(String, Start, TermEnd, json.object(O)),
        Value = json.object(O)
      else if string.between(String, Start, Start+4, "true")
      then % Boolean literal "true"
        Value = json.boolean(yes),
        TermEnd-4 = Start
      else if string.between(String, Start, Start+5, "false")
      then % Boolean literal "false"
        Value = json.boolean(no),
        TermEnd-5 = Start
      else if string.between(String, Start, Start+4, "null")
      then % Object literal "null"
        Value = null,
        TermEnd-4 = Start
      else
        TermEnd-1 = Start,
        Value = null
    ),
    json.get_space_end(String, TermEnd, End).

json.parse(String::in, Result::out) :-
    json.parse_value(String, 0, _, Value),
    (
        Value = json.array(A),
        Result = json.array(A)
    ;
        Value = json.object(A),
        Result = json.object(A)
    ;
        Value = null,
        Result = null
    ).
