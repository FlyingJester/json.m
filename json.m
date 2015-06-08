:- module json.
:- interface.

:- import_module bool.

% TODO: figure out how arrays work! More than half of our time is spent appending to lists.
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
:- type json.root --->
    null
    ; json.object(list(json.property))
    ; json.array(list(json.value)).

:- type json.error ---> json.error(line::int, expected::string, unexpected::string).

:- type json.result(T) ---> ok(T) ; json.error(int, int, string, string).

% Quick and Dirty Parsing
%:- pred json.parse_value(string::in, int::in, int::out, json.value::out) is semidet.
%:- pred json.parse_property(string::in, int::in, int::out, json.property::out) is semidet.

%:- pred json.parse_integer(string::in, int::in, int::out, json.integer::out) is semidet.
%:- pred json.parse_number(string::in, int::in, int::out, json.number::out) is semidet.
%:- pred json.parse_string(string::in, int::in, int::out, json.string::out) is semidet.

%:- pred json.parse_array(string::in, int::in, int::out, json.array::out) is semidet.
%:- pred json.parse_object(string::in, int::in, int::out, json.object::out) is semidet.

%:- pred json.parse(string::in, json.result::out) is semidet.

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
:- pred json.write_property(json.property::in, io.output_stream::in, io::di, io::uo) is det.

:- pred json.write_integer(json.integer::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_number(json.number::in, io.output_stream::in, io::di, io::uo) is det.

:- pred json.write_string(json.string::in, io.output_stream::in, io::di, io::uo) is det.

:- pred json.write_array(json.array::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_array(json.array::in, int::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_object(json.object::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_object(json.object::in, int::in, io.output_stream::in, io::di, io::uo) is det.

:- pred json.write(json.root::in, io.output_stream::in, io::di, io::uo) is det.
:- pred json.write_pretty(json.root::in, io.output_stream::in, io::di, io::uo) is det.

% For testing purposes
:- pred main(io::di, io::uo) is det.


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

:- pred json.write_array_element(json.value::in, json.value::in, json.value::out, io.output_stream::in, io.output_stream::out, io::di, io::uo) is det.
:- pred json.write_array_element(json.value::in, json.value::in, json.value::out, int::in, int::out, io.output_stream::in, io.output_stream::out, io::di, io::uo) is det.

json.write_array_element(Value, Last, Z, Stream, Out, !IO) :-
    json.write_value(Value, Stream, !IO),
    ( if Value=Last
      then
        Z=Value,
        io.write_char(Stream, ' ', !IO)
      else
        Z=Last,
        io.write_char(Stream, ',', !IO)
    ),
    Out = Stream.

json.write_array_element(Value, Last, Z, Indent, IndentOut, Stream, StreamOut, !IO) :-
    json.write_indent(Indent, Stream, !IO),
    json.write_value(Value, Stream, !IO),
    ( if Value=Last
      then
        Z=Value
      else
        Z=Last,
        io.write_char(Stream, ',', !IO)
    ),
    io.nl(Stream, !IO),
    StreamOut = Stream,
    IndentOut = Indent.

:- pred json.write_object_element(json.property::in, json.property::in, json.property::out, io.output_stream::in, io.output_stream::out, io::di, io::uo) is det.
:- pred json.write_object_element(json.property::in, json.property::in, json.property::out, int::in, int::out, io.output_stream::in, io.output_stream::out, io::di, io::uo) is det.

json.write_object_element(Property, Last, Z, Stream, Out, !IO) :-
    json.write_property(Property, Stream, !IO),
    ( if Property=Last
      then
        Z=Property,
        io.write_char(Stream, ' ', !IO)
      else
        Z=Last,
        io.write_char(Stream, ',', !IO)
    ),
    Out = Stream.

json.write_object_element(Property, Last, Z, Indent, IndentOut, Stream, StreamOut, !IO) :-
    json.write_indent(Indent, Stream, !IO),
    json.write_property(Property, Stream, !IO),
    ( if Property=Last
      then
        Z=Property
      else
        Z=Last,
        io.write_char(Stream, ',', !IO)
    ),
    io.nl(Stream, !IO),
    StreamOut = Stream,
    IndentOut = Indent.

json.write_array(JSArray, Stream, !IO) :-
    JSArray = json.array(Array),
    ( if list.last(Array, Last)
      then
        io.write_char(Stream, '[', !IO),
        list.foldl3(json.write_array_element, Array, Last, _, Stream, _, !IO),
        io.write_char(Stream, ']', !IO)
      else
        io.write_char(Stream, '[', !IO), io.write_char(Stream, ']', !IO)
    ),
    io.nl(Stream, !IO).

json.write_array(JSArray, Step, Stream, !IO) :-
    JSArray = json.array(Array),
    json.write_indent(Step, Stream, !IO),
    ( if list.last(Array, Last)
      then
        io.write_char(Stream, '[', !IO),
        list.foldl4(json.write_array_element, Array, Last, _, Step+1, _, Stream, _, !IO),
        json.write_indent(Step, Stream, !IO),
        io.write_char(Stream, ']', !IO)
      else
        io.write_char(Stream, '[', !IO), io.write_char(Stream, ']', !IO)
    ),
    io.nl(Stream, !IO).

json.write_object(JSObject, Stream, !IO) :-
    JSObject = json.object(Object),
    ( if list.last(Object, Last)
      then
        io.write_char(Stream, '{', !IO),
        list.foldl3(json.write_object_element, Object, Last, _, Stream, _, !IO),
        io.write_char(Stream, '}', !IO)
      else
        io.write_char(Stream, '{', !IO), io.write_char(Stream, '}', !IO)
    ),
    io.nl(Stream, !IO).

json.write_object(JSObject, Step, Stream, !IO) :-
    JSObject = json.object(Object),
    json.write_indent(Step, Stream, !IO),
    ( if list.last(Object, Last)
      then
        io.write_char(Stream, '{', !IO),
        list.foldl4(json.write_object_element, Object, Last, _, Step+1, _, Stream, _, !IO),
        json.write_indent(Step, Stream, !IO),
        io.write_char(Stream, '}', !IO)
      else
        io.write_char(Stream, '{', !IO), io.write_char(Stream, '}', !IO)
    ),
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

main(!IO) :-
    open_input("instruments.json", InResult, !IO),
    open_output("new_instruments.json", OutResult, !IO),
    ( if InResult = ok(InStream), OutResult = ok(OutStream)
      then
        io.read_file_as_string(InStream, StringResult, !IO),
        ( if StringResult = ok(String)
          then
            json.parse(String, Result),
            (
                Result = ok(Tree),
                io.write_string("Parse successful for ops.json\n", !IO),
                json.write(Tree, OutStream, !IO)
            ;
                Result = error(L, At, E, U),
                io.write_string("Could not parse file ops.json\n", !IO),
                io.format("Error at line %i, total character %i: Expected %s, not %s\n", [i(L), i(At), s(E), s(U)], !IO)
            )
          else
            io.write_string("Could not read file ops.json\n", !IO)
        ),
        close_input(InStream, !IO),
        close_output(OutStream, !IO)
      else
        (
            InResult = ok(_)
        ;
            InResult = error(Err1),
            io.format("IO Error with input stream ops.json\n%s\n", [s(error_message(Err1))], !IO)
        ),
        (
            OutResult = ok(_)
        ;
            OutResult = error(Err2), 
            io.format("IO Error with output stream new_ops.json\n%s\n", [s(error_message(Err2))], !IO)
        )
    ).
      
    
    
