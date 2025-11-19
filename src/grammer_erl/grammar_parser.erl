-file("src/grammer_erl/grammar_parser.yrl", 0).
-module(grammar_parser).
-file("src/grammer_erl/grammar_parser.erl", 3).
-export([parse/1, parse_and_scan/1, format_error/1]).

-file("c:/Program Files/Erlang OTP/lib/parsetools-2.6/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2024. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-ifdef (YECC_PARSE_DOC).
-doc ?YECC_PARSE_DOC.
-endif.
-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_location}, 0, [], []).

-ifdef (YECC_PARSE_AND_SCAN_DOC).
-doc ?YECC_PARSE_AND_SCAN_DOC.
-endif.
-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_location}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_location}, 0, [], []).

-ifdef (YECC_FORMAT_ERROR_DOC).
-doc ?YECC_FORMAT_ERROR_DOC.
-endif.
-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(erl_anno:location(), any()) -> no_return().
return_error(Location, Message) ->
    throw({error, {Location, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error: Stacktrace ->
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Location, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Location}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, EndLocation} ->
            yeccpars1(Tokens, {{F, A}, EndLocation}, State, States, Vstack);
        {eof, EndLocation} ->
            yeccpars1([], {no_func, EndLocation}, State, States, Vstack);
        {error, Descriptor, _EndLocation} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_location}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, EndLocation}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(EndLocation), [],
              {no_func, EndLocation}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Location}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_location}) ->
    Location = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Location), [], {no_func, Location});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Location}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Location), [], {no_func, Location}).

%% For internal use only.
yecc_end(Location) ->
    {'$end', Location}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string(Token) ->
    try
        yecctoken2string1(Token)
    catch
        _:_ ->
            io_lib:format("~tp", [Token])
    end.

-compile({nowarn_unused_function, yecctoken2string1/1}).
yecctoken2string1({atom, _, A}) -> io_lib:write_atom(A);
yecctoken2string1({integer,_,N}) -> io_lib:write(N);
yecctoken2string1({float,_,F}) -> io_lib:write(F);
yecctoken2string1({char,_,C}) -> io_lib:write_char(C);
yecctoken2string1({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string1({string,_,S}) -> io_lib:write_string(S);
yecctoken2string1({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string1({_Cat, _, Val}) -> io_lib:format("~tp", [Val]);
yecctoken2string1({dot, _}) -> "'.'";
yecctoken2string1({'$end', _}) -> [];
yecctoken2string1({Other, _}) when is_atom(Other) ->
    io_lib:write_atom(Other);
yecctoken2string1(Other) ->
    io_lib:format("~tp", [Other]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("src/grammer_erl/grammar_parser.erl", 194).

-dialyzer({nowarn_function, yeccpars2/7}).
-compile({nowarn_unused_function,  yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

-dialyzer({nowarn_function, yeccpars2_0/7}).
-compile({nowarn_unused_function,  yeccpars2_0/7}).
yeccpars2_0(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'atom', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'head', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'ife', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'ifz', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'rec', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'ref', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'tail', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_1/7}).
-compile({nowarn_unused_function,  yeccpars2_1/7}).
yeccpars2_1(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_1(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_2/7}).
-compile({nowarn_unused_function,  yeccpars2_2/7}).
yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_2_(Stack),
 yeccgoto_elements(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_3/7}).
-compile({nowarn_unused_function,  yeccpars2_3/7}).
yeccpars2_3(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, 'atom', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, 'head', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, 'ife', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, 'ifz', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, 'rec', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, 'ref', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(S, 'tail', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_3_(Stack),
 yeccgoto_term(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_4/7}).
-compile({nowarn_unused_function,  yeccpars2_4/7}).
yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_4_(Stack),
 yeccgoto_elements(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_5: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_6/7}).
-compile({nowarn_unused_function,  yeccpars2_6/7}).
yeccpars2_6(S, 'atom', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_7(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_8(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_9/7}).
-compile({nowarn_unused_function,  yeccpars2_9/7}).
yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_9_(Stack),
 yeccgoto_elements(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_10: see yeccpars2_0

%% yeccpars2_11: see yeccpars2_0

%% yeccpars2_12: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_13/7}).
-compile({nowarn_unused_function,  yeccpars2_13/7}).
yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_13_(Stack),
 yeccgoto_elements(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_14/7}).
-compile({nowarn_unused_function,  yeccpars2_14/7}).
yeccpars2_14(S, 'atom', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_15: see yeccpars2_0

%% yeccpars2_16: see yeccpars2_0

%% yeccpars2_17: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_18/7}).
-compile({nowarn_unused_function,  yeccpars2_18/7}).
yeccpars2_18(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, 'atom', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, 'ife', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, 'ifz', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_18_#'(Stack),
 yeccgoto_elements(hd(Nss), '#', Nss, NewStack, T, Ts, Tzr);
yeccpars2_18(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_18_$end'(Stack),
 yeccgoto_elements(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_18(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_18_)'(Stack),
 yeccgoto_elements(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_18(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_18_,'(Stack),
 yeccgoto_elements(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_18(_S, ']', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_18_]'(Stack),
 yeccgoto_elements(hd(Nss), ']', Nss, NewStack, T, Ts, Tzr);
yeccpars2_18(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_18_else(Stack),
 yeccgoto_elements(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_18(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_18_in(Stack),
 yeccgoto_elements(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_18(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_18_then(Stack),
 yeccgoto_elements(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_18(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_19/7}).
-compile({nowarn_unused_function,  yeccpars2_19/7}).
yeccpars2_19(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, 'atom', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, 'ife', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, 'ifz', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_19_#'(Stack),
 yeccgoto_apply(hd(Nss), '#', Nss, NewStack, T, Ts, Tzr);
yeccpars2_19(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_19_$end'(Stack),
 yeccgoto_apply(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_19(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_19_)'(Stack),
 yeccgoto_apply(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_19(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_19_,'(Stack),
 yeccgoto_apply(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_19(_S, ']', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_19_]'(Stack),
 yeccgoto_apply(hd(Nss), ']', Nss, NewStack, T, Ts, Tzr);
yeccpars2_19(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_19_else(Stack),
 yeccgoto_apply(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_19(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_19_in(Stack),
 yeccgoto_apply(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_19(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_19_then(Stack),
 yeccgoto_apply(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_19(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_20: see yeccpars2_0

%% yeccpars2_21: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_22/7}).
-compile({nowarn_unused_function,  yeccpars2_22/7}).
yeccpars2_22(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, 'atom', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, 'ife', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, 'ifz', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_22_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_23/7}).
-compile({nowarn_unused_function,  yeccpars2_23/7}).
yeccpars2_23(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, 'atom', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, 'ife', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, 'ifz', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_23_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_24/7}).
-compile({nowarn_unused_function,  yeccpars2_24/7}).
yeccpars2_24(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, 'atom', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, 'ife', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, 'ifz', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_24_#'(Stack),
 yeccgoto_elements(hd(Nss), '#', Nss, NewStack, T, Ts, Tzr);
yeccpars2_24(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_24_$end'(Stack),
 yeccgoto_elements(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_24(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_24_)'(Stack),
 yeccgoto_elements(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_24(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_24_,'(Stack),
 yeccgoto_elements(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_24(_S, ']', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_24_]'(Stack),
 yeccgoto_elements(hd(Nss), ']', Nss, NewStack, T, Ts, Tzr);
yeccpars2_24(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_24_else(Stack),
 yeccgoto_elements(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_24(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_24_in(Stack),
 yeccgoto_elements(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_24(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_24_then(Stack),
 yeccgoto_elements(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_24(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_25/7}).
-compile({nowarn_unused_function,  yeccpars2_25/7}).
yeccpars2_25(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, 'atom', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, 'ife', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, 'ifz', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_25_#'(Stack),
 yeccgoto_elements(hd(Nss), '#', Nss, NewStack, T, Ts, Tzr);
yeccpars2_25(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_25_$end'(Stack),
 yeccgoto_elements(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_25(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_25_)'(Stack),
 yeccgoto_elements(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_25(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_25_,'(Stack),
 yeccgoto_elements(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_25(_S, ']', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_25_]'(Stack),
 yeccgoto_elements(hd(Nss), ']', Nss, NewStack, T, Ts, Tzr);
yeccpars2_25(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_25_else(Stack),
 yeccgoto_elements(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_25(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_25_in(Stack),
 yeccgoto_elements(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_25(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_25_then(Stack),
 yeccgoto_elements(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_25(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_26/7}).
-compile({nowarn_unused_function,  yeccpars2_26/7}).
yeccpars2_26(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_26(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_27: see yeccpars2_0

yeccpars2_28(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, 'in', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_29: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_30/7}).
-compile({nowarn_unused_function,  yeccpars2_30/7}).
yeccpars2_30(S, 'ife', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, 'ifz', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_30_!'(Stack),
 yeccgoto_elements(hd(Nss), '!', Nss, NewStack, T, Ts, Tzr);
yeccpars2_30(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_30_#'(Stack),
 yeccgoto_elements(hd(Nss), '#', Nss, NewStack, T, Ts, Tzr);
yeccpars2_30(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_30_$end'(Stack),
 yeccgoto_elements(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_30(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_30_('(Stack),
 yeccgoto_elements(hd(Nss), '(', Nss, NewStack, T, Ts, Tzr);
yeccpars2_30(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_30_)'(Stack),
 yeccgoto_elements(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_30(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_30_+'(Stack),
 yeccgoto_elements(hd(Nss), '+', Nss, NewStack, T, Ts, Tzr);
yeccpars2_30(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_30_,'(Stack),
 yeccgoto_elements(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_30(_S, ':=', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_30_:='(Stack),
 yeccgoto_elements(hd(Nss), ':=', Nss, NewStack, T, Ts, Tzr);
yeccpars2_30(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_30_['(Stack),
 yeccgoto_elements(hd(Nss), '[', Nss, NewStack, T, Ts, Tzr);
yeccpars2_30(_S, ']', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_30_]'(Stack),
 yeccgoto_elements(hd(Nss), ']', Nss, NewStack, T, Ts, Tzr);
yeccpars2_30(_S, 'atom', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_30_atom(Stack),
 yeccgoto_elements(hd(Nss), 'atom', Nss, NewStack, T, Ts, Tzr);
yeccpars2_30(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_30_else(Stack),
 yeccgoto_elements(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_30(_S, 'head', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_30_head(Stack),
 yeccgoto_elements(hd(Nss), 'head', Nss, NewStack, T, Ts, Tzr);
yeccpars2_30(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_30_in(Stack),
 yeccgoto_elements(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_30(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_30_integer(Stack),
 yeccgoto_elements(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_30(_S, 'rec', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_30_rec(Stack),
 yeccgoto_elements(hd(Nss), 'rec', Nss, NewStack, T, Ts, Tzr);
yeccpars2_30(_S, 'ref', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_30_ref(Stack),
 yeccgoto_elements(hd(Nss), 'ref', Nss, NewStack, T, Ts, Tzr);
yeccpars2_30(_S, 'tail', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_30_tail(Stack),
 yeccgoto_elements(hd(Nss), 'tail', Nss, NewStack, T, Ts, Tzr);
yeccpars2_30(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_30_then(Stack),
 yeccgoto_elements(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_30(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_31(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(S, 'then', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_32: see yeccpars2_0

yeccpars2_33(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, 'else', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_34: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_35/7}).
-compile({nowarn_unused_function,  yeccpars2_35/7}).
yeccpars2_35(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_35_!'(Stack),
 yeccgoto_elements(hd(Nss), '!', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_35_#'(Stack),
 yeccgoto_elements(hd(Nss), '#', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_35_$end'(Stack),
 yeccgoto_elements(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_35_('(Stack),
 yeccgoto_elements(hd(Nss), '(', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_35_)'(Stack),
 yeccgoto_elements(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_35_+'(Stack),
 yeccgoto_elements(hd(Nss), '+', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_35_,'(Stack),
 yeccgoto_elements(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, ':=', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_35_:='(Stack),
 yeccgoto_elements(hd(Nss), ':=', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_35_['(Stack),
 yeccgoto_elements(hd(Nss), '[', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, ']', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_35_]'(Stack),
 yeccgoto_elements(hd(Nss), ']', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, 'atom', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_35_atom(Stack),
 yeccgoto_elements(hd(Nss), 'atom', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_35_else(Stack),
 yeccgoto_elements(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, 'head', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_35_head(Stack),
 yeccgoto_elements(hd(Nss), 'head', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_35_in(Stack),
 yeccgoto_elements(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_35_integer(Stack),
 yeccgoto_elements(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_35_let(Stack),
 yeccgoto_elements(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, 'rec', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_35_rec(Stack),
 yeccgoto_elements(hd(Nss), 'rec', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, 'ref', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_35_ref(Stack),
 yeccgoto_elements(hd(Nss), 'ref', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, 'tail', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_35_tail(Stack),
 yeccgoto_elements(hd(Nss), 'tail', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_35_then(Stack),
 yeccgoto_elements(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_35(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_36(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, 'then', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_37: see yeccpars2_0

yeccpars2_38(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, 'else', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_39: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_40/7}).
-compile({nowarn_unused_function,  yeccpars2_40/7}).
yeccpars2_40(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_!'(Stack),
 yeccgoto_elements(hd(Nss), '!', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_#'(Stack),
 yeccgoto_elements(hd(Nss), '#', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_$end'(Stack),
 yeccgoto_elements(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_('(Stack),
 yeccgoto_elements(hd(Nss), '(', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_)'(Stack),
 yeccgoto_elements(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_+'(Stack),
 yeccgoto_elements(hd(Nss), '+', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_,'(Stack),
 yeccgoto_elements(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, ':=', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_:='(Stack),
 yeccgoto_elements(hd(Nss), ':=', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_['(Stack),
 yeccgoto_elements(hd(Nss), '[', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, ']', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = 'yeccpars2_40_]'(Stack),
 yeccgoto_elements(hd(Nss), ']', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, 'atom', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_40_atom(Stack),
 yeccgoto_elements(hd(Nss), 'atom', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_40_else(Stack),
 yeccgoto_elements(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, 'head', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_40_head(Stack),
 yeccgoto_elements(hd(Nss), 'head', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_40_in(Stack),
 yeccgoto_elements(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_40_integer(Stack),
 yeccgoto_elements(hd(Nss), 'integer', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, 'let', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_40_let(Stack),
 yeccgoto_elements(hd(Nss), 'let', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, 'rec', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_40_rec(Stack),
 yeccgoto_elements(hd(Nss), 'rec', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, 'ref', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_40_ref(Stack),
 yeccgoto_elements(hd(Nss), 'ref', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, 'tail', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_40_tail(Stack),
 yeccgoto_elements(hd(Nss), 'tail', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_40_then(Stack),
 yeccgoto_elements(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_40(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_41/7}).
-compile({nowarn_unused_function,  yeccpars2_41/7}).
yeccpars2_41(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, 'atom', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, 'ife', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, 'ifz', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_41_#'(Stack),
 yeccgoto_elements(hd(Nss), '#', Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_41_$end'(Stack),
 yeccgoto_elements(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_41_)'(Stack),
 yeccgoto_elements(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_41_,'(Stack),
 yeccgoto_elements(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, ']', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_41_]'(Stack),
 yeccgoto_elements(hd(Nss), ']', Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_41_else(Stack),
 yeccgoto_elements(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_41_in(Stack),
 yeccgoto_elements(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_41_then(Stack),
 yeccgoto_elements(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_41(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_42/7}).
-compile({nowarn_unused_function,  yeccpars2_42/7}).
yeccpars2_42(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_43/7}).
-compile({nowarn_unused_function,  yeccpars2_43/7}).
yeccpars2_43(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, 'atom', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, 'head', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, 'ife', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, 'ifz', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, 'rec', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, 'ref', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, 'tail', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_43_(Stack),
 yeccgoto_list_expr(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_44/7}).
-compile({nowarn_unused_function,  yeccpars2_44/7}).
yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_44_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_45: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_46/7}).
-compile({nowarn_unused_function,  yeccpars2_46/7}).
yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_46_(Stack),
 yeccgoto_list_expr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_47/7}).
-compile({nowarn_unused_function,  yeccpars2_47/7}).
yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_47_(Stack),
 yeccgoto_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_48(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_49/7}).
-compile({nowarn_unused_function,  yeccpars2_49/7}).
yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_49_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_50/7}).
-compile({nowarn_unused_function,  yeccpars2_50/7}).
yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_50_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_51/7}).
-compile({nowarn_unused_function,  yeccpars2_51/7}).
yeccpars2_51(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_52: see yeccpars2_0

-dialyzer({nowarn_function, yeccpars2_53/7}).
-compile({nowarn_unused_function,  yeccpars2_53/7}).
yeccpars2_53(S, 'ife', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, 'ifz', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_53_(Stack),
 yeccgoto_elements(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_54/7}).
-compile({nowarn_unused_function,  yeccpars2_54/7}).
yeccpars2_54(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, 'atom', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, 'ife', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, 'ifz', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, 'integer', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_54_#'(Stack),
 yeccgoto_elements(hd(Nss), '#', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_54_$end'(Stack),
 yeccgoto_elements(hd(Nss), '$end', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_54_)'(Stack),
 yeccgoto_elements(hd(Nss), ')', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_54_,'(Stack),
 yeccgoto_elements(hd(Nss), ',', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, ']', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_54_]'(Stack),
 yeccgoto_elements(hd(Nss), ']', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, 'else', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_54_else(Stack),
 yeccgoto_elements(hd(Nss), 'else', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, 'head', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_54_head(Stack),
 yeccgoto_elements(hd(Nss), 'head', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, 'in', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_54_in(Stack),
 yeccgoto_elements(hd(Nss), 'in', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, 'rec', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_54_rec(Stack),
 yeccgoto_elements(hd(Nss), 'rec', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, 'ref', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_54_ref(Stack),
 yeccgoto_elements(hd(Nss), 'ref', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, 'tail', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_54_tail(Stack),
 yeccgoto_elements(hd(Nss), 'tail', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_S, 'then', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_54_then(Stack),
 yeccgoto_elements(hd(Nss), 'then', Nss, NewStack, T, Ts, Tzr);
yeccpars2_54(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccgoto_apply/7}).
-compile({nowarn_unused_function,  yeccgoto_apply/7}).
yeccgoto_apply(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(3=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(5=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(10=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(16=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(20=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(21=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(22=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(33=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(34=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(38=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(39=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(40=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(41=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(45=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(48=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_apply(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_elements/7}).
-compile({nowarn_unused_function,  yeccgoto_elements/7}).
yeccgoto_elements(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(3, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(5, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(7, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(48, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(10, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(41, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(11, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(12, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(31, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(15, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(25, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(16, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(24, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(17, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(18, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(18, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(19, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(20, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(23, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(21, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(22, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(22, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(23, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(24, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(25, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(27, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(28, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(28, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(29, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(30, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(30, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(31, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(32, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(33, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(34, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(35, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(35, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(36, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(37, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(38, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(38, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(39, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(40, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(40, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(41, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(43, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(45, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(43, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(48, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(52, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(53, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_elements(54, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(19, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_list/7}).
-compile({nowarn_unused_function,  yeccgoto_list/7}).
yeccgoto_list(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(3=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(5=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(7=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(8=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(10=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(11=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(12=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(15=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(16=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(17=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(20=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(21=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(22=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(23=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(25=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(27=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(29=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(30=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(31=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(33=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(34=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(35=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(36=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(37=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(38=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(39=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(40=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(41=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(43=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(45=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(48=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(52=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list(54=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_list_expr/7}).
-compile({nowarn_unused_function,  yeccgoto_list_expr/7}).
yeccgoto_list_expr(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_expr(45=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_term/7}).
-compile({nowarn_unused_function,  yeccgoto_term/7}).
yeccgoto_term(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_2_/1}).
-dialyzer({nowarn_function, yeccpars2_2_/1}).
-compile({nowarn_unused_function,  yeccpars2_2_/1}).
-file("src/grammer_erl/grammar_parser.yrl", 31).
yeccpars2_2_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   ___1
  end | __Stack].

-compile({inline,yeccpars2_3_/1}).
-dialyzer({nowarn_function, yeccpars2_3_/1}).
-compile({nowarn_unused_function,  yeccpars2_3_/1}).
-file("src/grammer_erl/grammar_parser.yrl", 15).
yeccpars2_3_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   ___1
  end | __Stack].

-compile({inline,yeccpars2_4_/1}).
-dialyzer({nowarn_function, yeccpars2_4_/1}).
-compile({nowarn_unused_function,  yeccpars2_4_/1}).
-file("src/grammer_erl/grammar_parser.yrl", 26).
yeccpars2_4_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                    ___1
  end | __Stack].

-compile({inline,yeccpars2_9_/1}).
-dialyzer({nowarn_function, yeccpars2_9_/1}).
-compile({nowarn_unused_function,  yeccpars2_9_/1}).
-file("src/grammer_erl/grammar_parser.yrl", 32).
yeccpars2_9_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                   {var, ___1}
  end | __Stack].

-compile({inline,yeccpars2_13_/1}).
-dialyzer({nowarn_function, yeccpars2_13_/1}).
-compile({nowarn_unused_function,  yeccpars2_13_/1}).
-file("src/grammer_erl/grammar_parser.yrl", 33).
yeccpars2_13_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                      {int, ___1}
  end | __Stack].

-compile({inline,'yeccpars2_18_#'/1}).
-dialyzer({nowarn_function, 'yeccpars2_18_#'/1}).
-compile({nowarn_unused_function,  'yeccpars2_18_#'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 24).
'yeccpars2_18_#'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              {tail, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_18_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_18_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_18_$end'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 24).
'yeccpars2_18_$end'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              {tail, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_18_)'/1}).
-dialyzer({nowarn_function, 'yeccpars2_18_)'/1}).
-compile({nowarn_unused_function,  'yeccpars2_18_)'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 24).
'yeccpars2_18_)'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              {tail, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_18_,'/1}).
-dialyzer({nowarn_function, 'yeccpars2_18_,'/1}).
-compile({nowarn_unused_function,  'yeccpars2_18_,'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 24).
'yeccpars2_18_,'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              {tail, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_18_]'/1}).
-dialyzer({nowarn_function, 'yeccpars2_18_]'/1}).
-compile({nowarn_unused_function,  'yeccpars2_18_]'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 24).
'yeccpars2_18_]'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              {tail, ___2}
  end | __Stack].

-compile({inline,yeccpars2_18_else/1}).
-dialyzer({nowarn_function, yeccpars2_18_else/1}).
-compile({nowarn_unused_function,  yeccpars2_18_else/1}).
-file("src/grammer_erl/grammar_parser.yrl", 24).
yeccpars2_18_else(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              {tail, ___2}
  end | __Stack].

-compile({inline,yeccpars2_18_in/1}).
-dialyzer({nowarn_function, yeccpars2_18_in/1}).
-compile({nowarn_unused_function,  yeccpars2_18_in/1}).
-file("src/grammer_erl/grammar_parser.yrl", 24).
yeccpars2_18_in(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              {tail, ___2}
  end | __Stack].

-compile({inline,yeccpars2_18_then/1}).
-dialyzer({nowarn_function, yeccpars2_18_then/1}).
-compile({nowarn_unused_function,  yeccpars2_18_then/1}).
-file("src/grammer_erl/grammar_parser.yrl", 24).
yeccpars2_18_then(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              {tail, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_19_#'/1}).
-dialyzer({nowarn_function, 'yeccpars2_19_#'/1}).
-compile({nowarn_unused_function,  'yeccpars2_19_#'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 35).
'yeccpars2_19_#'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {app, ___1, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_19_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_19_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_19_$end'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 35).
'yeccpars2_19_$end'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {app, ___1, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_19_)'/1}).
-dialyzer({nowarn_function, 'yeccpars2_19_)'/1}).
-compile({nowarn_unused_function,  'yeccpars2_19_)'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 35).
'yeccpars2_19_)'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {app, ___1, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_19_,'/1}).
-dialyzer({nowarn_function, 'yeccpars2_19_,'/1}).
-compile({nowarn_unused_function,  'yeccpars2_19_,'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 35).
'yeccpars2_19_,'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {app, ___1, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_19_]'/1}).
-dialyzer({nowarn_function, 'yeccpars2_19_]'/1}).
-compile({nowarn_unused_function,  'yeccpars2_19_]'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 35).
'yeccpars2_19_]'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {app, ___1, ___2}
  end | __Stack].

-compile({inline,yeccpars2_19_else/1}).
-dialyzer({nowarn_function, yeccpars2_19_else/1}).
-compile({nowarn_unused_function,  yeccpars2_19_else/1}).
-file("src/grammer_erl/grammar_parser.yrl", 35).
yeccpars2_19_else(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {app, ___1, ___2}
  end | __Stack].

-compile({inline,yeccpars2_19_in/1}).
-dialyzer({nowarn_function, yeccpars2_19_in/1}).
-compile({nowarn_unused_function,  yeccpars2_19_in/1}).
-file("src/grammer_erl/grammar_parser.yrl", 35).
yeccpars2_19_in(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {app, ___1, ___2}
  end | __Stack].

-compile({inline,yeccpars2_19_then/1}).
-dialyzer({nowarn_function, yeccpars2_19_then/1}).
-compile({nowarn_unused_function,  yeccpars2_19_then/1}).
-file("src/grammer_erl/grammar_parser.yrl", 35).
yeccpars2_19_then(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {app, ___1, ___2}
  end | __Stack].

-compile({inline,yeccpars2_22_/1}).
-dialyzer({nowarn_function, yeccpars2_22_/1}).
-compile({nowarn_unused_function,  yeccpars2_22_/1}).
-file("src/grammer_erl/grammar_parser.yrl", 19).
yeccpars2_22_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                     {assign, ___1, ___3}
  end | __Stack].

-compile({inline,yeccpars2_23_/1}).
-dialyzer({nowarn_function, yeccpars2_23_/1}).
-compile({nowarn_unused_function,  yeccpars2_23_/1}).
-file("src/grammer_erl/grammar_parser.yrl", 28).
yeccpars2_23_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                    {add, ___1, ___3}
  end | __Stack].

-compile({inline,'yeccpars2_24_#'/1}).
-dialyzer({nowarn_function, 'yeccpars2_24_#'/1}).
-compile({nowarn_unused_function,  'yeccpars2_24_#'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 21).
'yeccpars2_24_#'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {ref, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_24_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_24_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_24_$end'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 21).
'yeccpars2_24_$end'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {ref, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_24_)'/1}).
-dialyzer({nowarn_function, 'yeccpars2_24_)'/1}).
-compile({nowarn_unused_function,  'yeccpars2_24_)'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 21).
'yeccpars2_24_)'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {ref, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_24_,'/1}).
-dialyzer({nowarn_function, 'yeccpars2_24_,'/1}).
-compile({nowarn_unused_function,  'yeccpars2_24_,'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 21).
'yeccpars2_24_,'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {ref, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_24_]'/1}).
-dialyzer({nowarn_function, 'yeccpars2_24_]'/1}).
-compile({nowarn_unused_function,  'yeccpars2_24_]'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 21).
'yeccpars2_24_]'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {ref, ___2}
  end | __Stack].

-compile({inline,yeccpars2_24_else/1}).
-dialyzer({nowarn_function, yeccpars2_24_else/1}).
-compile({nowarn_unused_function,  yeccpars2_24_else/1}).
-file("src/grammer_erl/grammar_parser.yrl", 21).
yeccpars2_24_else(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {ref, ___2}
  end | __Stack].

-compile({inline,yeccpars2_24_in/1}).
-dialyzer({nowarn_function, yeccpars2_24_in/1}).
-compile({nowarn_unused_function,  yeccpars2_24_in/1}).
-file("src/grammer_erl/grammar_parser.yrl", 21).
yeccpars2_24_in(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {ref, ___2}
  end | __Stack].

-compile({inline,yeccpars2_24_then/1}).
-dialyzer({nowarn_function, yeccpars2_24_then/1}).
-compile({nowarn_unused_function,  yeccpars2_24_then/1}).
-file("src/grammer_erl/grammar_parser.yrl", 21).
yeccpars2_24_then(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {ref, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_25_#'/1}).
-dialyzer({nowarn_function, 'yeccpars2_25_#'/1}).
-compile({nowarn_unused_function,  'yeccpars2_25_#'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 22).
'yeccpars2_25_#'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {rec, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_25_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_25_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_25_$end'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 22).
'yeccpars2_25_$end'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {rec, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_25_)'/1}).
-dialyzer({nowarn_function, 'yeccpars2_25_)'/1}).
-compile({nowarn_unused_function,  'yeccpars2_25_)'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 22).
'yeccpars2_25_)'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {rec, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_25_,'/1}).
-dialyzer({nowarn_function, 'yeccpars2_25_,'/1}).
-compile({nowarn_unused_function,  'yeccpars2_25_,'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 22).
'yeccpars2_25_,'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {rec, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_25_]'/1}).
-dialyzer({nowarn_function, 'yeccpars2_25_]'/1}).
-compile({nowarn_unused_function,  'yeccpars2_25_]'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 22).
'yeccpars2_25_]'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {rec, ___2}
  end | __Stack].

-compile({inline,yeccpars2_25_else/1}).
-dialyzer({nowarn_function, yeccpars2_25_else/1}).
-compile({nowarn_unused_function,  yeccpars2_25_else/1}).
-file("src/grammer_erl/grammar_parser.yrl", 22).
yeccpars2_25_else(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {rec, ___2}
  end | __Stack].

-compile({inline,yeccpars2_25_in/1}).
-dialyzer({nowarn_function, yeccpars2_25_in/1}).
-compile({nowarn_unused_function,  yeccpars2_25_in/1}).
-file("src/grammer_erl/grammar_parser.yrl", 22).
yeccpars2_25_in(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {rec, ___2}
  end | __Stack].

-compile({inline,yeccpars2_25_then/1}).
-dialyzer({nowarn_function, yeccpars2_25_then/1}).
-compile({nowarn_unused_function,  yeccpars2_25_then/1}).
-file("src/grammer_erl/grammar_parser.yrl", 22).
yeccpars2_25_then(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                             {rec, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_30_!'/1}).
-dialyzer({nowarn_function, 'yeccpars2_30_!'/1}).
-compile({nowarn_unused_function,  'yeccpars2_30_!'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 25).
'yeccpars2_30_!'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    {letexpr, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_30_#'/1}).
-dialyzer({nowarn_function, 'yeccpars2_30_#'/1}).
-compile({nowarn_unused_function,  'yeccpars2_30_#'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 25).
'yeccpars2_30_#'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    {letexpr, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_30_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_30_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_30_$end'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 25).
'yeccpars2_30_$end'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    {letexpr, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_30_('/1}).
-dialyzer({nowarn_function, 'yeccpars2_30_('/1}).
-compile({nowarn_unused_function,  'yeccpars2_30_('/1}).
-file("src/grammer_erl/grammar_parser.yrl", 25).
'yeccpars2_30_('(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    {letexpr, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_30_)'/1}).
-dialyzer({nowarn_function, 'yeccpars2_30_)'/1}).
-compile({nowarn_unused_function,  'yeccpars2_30_)'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 25).
'yeccpars2_30_)'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    {letexpr, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_30_+'/1}).
-dialyzer({nowarn_function, 'yeccpars2_30_+'/1}).
-compile({nowarn_unused_function,  'yeccpars2_30_+'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 25).
'yeccpars2_30_+'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    {letexpr, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_30_,'/1}).
-dialyzer({nowarn_function, 'yeccpars2_30_,'/1}).
-compile({nowarn_unused_function,  'yeccpars2_30_,'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 25).
'yeccpars2_30_,'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    {letexpr, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_30_:='/1}).
-dialyzer({nowarn_function, 'yeccpars2_30_:='/1}).
-compile({nowarn_unused_function,  'yeccpars2_30_:='/1}).
-file("src/grammer_erl/grammar_parser.yrl", 25).
'yeccpars2_30_:='(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    {letexpr, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_30_['/1}).
-dialyzer({nowarn_function, 'yeccpars2_30_['/1}).
-compile({nowarn_unused_function,  'yeccpars2_30_['/1}).
-file("src/grammer_erl/grammar_parser.yrl", 25).
'yeccpars2_30_['(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    {letexpr, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_30_]'/1}).
-dialyzer({nowarn_function, 'yeccpars2_30_]'/1}).
-compile({nowarn_unused_function,  'yeccpars2_30_]'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 25).
'yeccpars2_30_]'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    {letexpr, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_30_atom/1}).
-dialyzer({nowarn_function, yeccpars2_30_atom/1}).
-compile({nowarn_unused_function,  yeccpars2_30_atom/1}).
-file("src/grammer_erl/grammar_parser.yrl", 25).
yeccpars2_30_atom(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    {letexpr, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_30_else/1}).
-dialyzer({nowarn_function, yeccpars2_30_else/1}).
-compile({nowarn_unused_function,  yeccpars2_30_else/1}).
-file("src/grammer_erl/grammar_parser.yrl", 25).
yeccpars2_30_else(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    {letexpr, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_30_head/1}).
-dialyzer({nowarn_function, yeccpars2_30_head/1}).
-compile({nowarn_unused_function,  yeccpars2_30_head/1}).
-file("src/grammer_erl/grammar_parser.yrl", 25).
yeccpars2_30_head(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    {letexpr, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_30_in/1}).
-dialyzer({nowarn_function, yeccpars2_30_in/1}).
-compile({nowarn_unused_function,  yeccpars2_30_in/1}).
-file("src/grammer_erl/grammar_parser.yrl", 25).
yeccpars2_30_in(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    {letexpr, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_30_integer/1}).
-dialyzer({nowarn_function, yeccpars2_30_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_30_integer/1}).
-file("src/grammer_erl/grammar_parser.yrl", 25).
yeccpars2_30_integer(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    {letexpr, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_30_rec/1}).
-dialyzer({nowarn_function, yeccpars2_30_rec/1}).
-compile({nowarn_unused_function,  yeccpars2_30_rec/1}).
-file("src/grammer_erl/grammar_parser.yrl", 25).
yeccpars2_30_rec(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    {letexpr, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_30_ref/1}).
-dialyzer({nowarn_function, yeccpars2_30_ref/1}).
-compile({nowarn_unused_function,  yeccpars2_30_ref/1}).
-file("src/grammer_erl/grammar_parser.yrl", 25).
yeccpars2_30_ref(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    {letexpr, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_30_tail/1}).
-dialyzer({nowarn_function, yeccpars2_30_tail/1}).
-compile({nowarn_unused_function,  yeccpars2_30_tail/1}).
-file("src/grammer_erl/grammar_parser.yrl", 25).
yeccpars2_30_tail(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    {letexpr, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_30_then/1}).
-dialyzer({nowarn_function, yeccpars2_30_then/1}).
-compile({nowarn_unused_function,  yeccpars2_30_then/1}).
-file("src/grammer_erl/grammar_parser.yrl", 25).
yeccpars2_30_then(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                    {letexpr, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_35_!'/1}).
-dialyzer({nowarn_function, 'yeccpars2_35_!'/1}).
-compile({nowarn_unused_function,  'yeccpars2_35_!'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 18).
'yeccpars2_35_!'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ifz, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_35_#'/1}).
-dialyzer({nowarn_function, 'yeccpars2_35_#'/1}).
-compile({nowarn_unused_function,  'yeccpars2_35_#'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 18).
'yeccpars2_35_#'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ifz, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_35_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_35_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_35_$end'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 18).
'yeccpars2_35_$end'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ifz, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_35_('/1}).
-dialyzer({nowarn_function, 'yeccpars2_35_('/1}).
-compile({nowarn_unused_function,  'yeccpars2_35_('/1}).
-file("src/grammer_erl/grammar_parser.yrl", 18).
'yeccpars2_35_('(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ifz, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_35_)'/1}).
-dialyzer({nowarn_function, 'yeccpars2_35_)'/1}).
-compile({nowarn_unused_function,  'yeccpars2_35_)'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 18).
'yeccpars2_35_)'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ifz, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_35_+'/1}).
-dialyzer({nowarn_function, 'yeccpars2_35_+'/1}).
-compile({nowarn_unused_function,  'yeccpars2_35_+'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 18).
'yeccpars2_35_+'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ifz, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_35_,'/1}).
-dialyzer({nowarn_function, 'yeccpars2_35_,'/1}).
-compile({nowarn_unused_function,  'yeccpars2_35_,'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 18).
'yeccpars2_35_,'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ifz, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_35_:='/1}).
-dialyzer({nowarn_function, 'yeccpars2_35_:='/1}).
-compile({nowarn_unused_function,  'yeccpars2_35_:='/1}).
-file("src/grammer_erl/grammar_parser.yrl", 18).
'yeccpars2_35_:='(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ifz, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_35_['/1}).
-dialyzer({nowarn_function, 'yeccpars2_35_['/1}).
-compile({nowarn_unused_function,  'yeccpars2_35_['/1}).
-file("src/grammer_erl/grammar_parser.yrl", 18).
'yeccpars2_35_['(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ifz, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_35_]'/1}).
-dialyzer({nowarn_function, 'yeccpars2_35_]'/1}).
-compile({nowarn_unused_function,  'yeccpars2_35_]'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 18).
'yeccpars2_35_]'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ifz, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_35_atom/1}).
-dialyzer({nowarn_function, yeccpars2_35_atom/1}).
-compile({nowarn_unused_function,  yeccpars2_35_atom/1}).
-file("src/grammer_erl/grammar_parser.yrl", 18).
yeccpars2_35_atom(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ifz, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_35_else/1}).
-dialyzer({nowarn_function, yeccpars2_35_else/1}).
-compile({nowarn_unused_function,  yeccpars2_35_else/1}).
-file("src/grammer_erl/grammar_parser.yrl", 18).
yeccpars2_35_else(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ifz, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_35_head/1}).
-dialyzer({nowarn_function, yeccpars2_35_head/1}).
-compile({nowarn_unused_function,  yeccpars2_35_head/1}).
-file("src/grammer_erl/grammar_parser.yrl", 18).
yeccpars2_35_head(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ifz, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_35_in/1}).
-dialyzer({nowarn_function, yeccpars2_35_in/1}).
-compile({nowarn_unused_function,  yeccpars2_35_in/1}).
-file("src/grammer_erl/grammar_parser.yrl", 18).
yeccpars2_35_in(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ifz, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_35_integer/1}).
-dialyzer({nowarn_function, yeccpars2_35_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_35_integer/1}).
-file("src/grammer_erl/grammar_parser.yrl", 18).
yeccpars2_35_integer(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ifz, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_35_let/1}).
-dialyzer({nowarn_function, yeccpars2_35_let/1}).
-compile({nowarn_unused_function,  yeccpars2_35_let/1}).
-file("src/grammer_erl/grammar_parser.yrl", 18).
yeccpars2_35_let(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ifz, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_35_rec/1}).
-dialyzer({nowarn_function, yeccpars2_35_rec/1}).
-compile({nowarn_unused_function,  yeccpars2_35_rec/1}).
-file("src/grammer_erl/grammar_parser.yrl", 18).
yeccpars2_35_rec(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ifz, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_35_ref/1}).
-dialyzer({nowarn_function, yeccpars2_35_ref/1}).
-compile({nowarn_unused_function,  yeccpars2_35_ref/1}).
-file("src/grammer_erl/grammar_parser.yrl", 18).
yeccpars2_35_ref(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ifz, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_35_tail/1}).
-dialyzer({nowarn_function, yeccpars2_35_tail/1}).
-compile({nowarn_unused_function,  yeccpars2_35_tail/1}).
-file("src/grammer_erl/grammar_parser.yrl", 18).
yeccpars2_35_tail(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ifz, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_35_then/1}).
-dialyzer({nowarn_function, yeccpars2_35_then/1}).
-compile({nowarn_unused_function,  yeccpars2_35_then/1}).
-file("src/grammer_erl/grammar_parser.yrl", 18).
yeccpars2_35_then(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ifz, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_40_!'/1}).
-dialyzer({nowarn_function, 'yeccpars2_40_!'/1}).
-compile({nowarn_unused_function,  'yeccpars2_40_!'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 17).
'yeccpars2_40_!'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ife, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_40_#'/1}).
-dialyzer({nowarn_function, 'yeccpars2_40_#'/1}).
-compile({nowarn_unused_function,  'yeccpars2_40_#'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 17).
'yeccpars2_40_#'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ife, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_40_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_40_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_40_$end'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 17).
'yeccpars2_40_$end'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ife, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_40_('/1}).
-dialyzer({nowarn_function, 'yeccpars2_40_('/1}).
-compile({nowarn_unused_function,  'yeccpars2_40_('/1}).
-file("src/grammer_erl/grammar_parser.yrl", 17).
'yeccpars2_40_('(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ife, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_40_)'/1}).
-dialyzer({nowarn_function, 'yeccpars2_40_)'/1}).
-compile({nowarn_unused_function,  'yeccpars2_40_)'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 17).
'yeccpars2_40_)'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ife, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_40_+'/1}).
-dialyzer({nowarn_function, 'yeccpars2_40_+'/1}).
-compile({nowarn_unused_function,  'yeccpars2_40_+'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 17).
'yeccpars2_40_+'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ife, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_40_,'/1}).
-dialyzer({nowarn_function, 'yeccpars2_40_,'/1}).
-compile({nowarn_unused_function,  'yeccpars2_40_,'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 17).
'yeccpars2_40_,'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ife, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_40_:='/1}).
-dialyzer({nowarn_function, 'yeccpars2_40_:='/1}).
-compile({nowarn_unused_function,  'yeccpars2_40_:='/1}).
-file("src/grammer_erl/grammar_parser.yrl", 17).
'yeccpars2_40_:='(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ife, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_40_['/1}).
-dialyzer({nowarn_function, 'yeccpars2_40_['/1}).
-compile({nowarn_unused_function,  'yeccpars2_40_['/1}).
-file("src/grammer_erl/grammar_parser.yrl", 17).
'yeccpars2_40_['(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ife, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_40_]'/1}).
-dialyzer({nowarn_function, 'yeccpars2_40_]'/1}).
-compile({nowarn_unused_function,  'yeccpars2_40_]'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 17).
'yeccpars2_40_]'(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ife, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_40_atom/1}).
-dialyzer({nowarn_function, yeccpars2_40_atom/1}).
-compile({nowarn_unused_function,  yeccpars2_40_atom/1}).
-file("src/grammer_erl/grammar_parser.yrl", 17).
yeccpars2_40_atom(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ife, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_40_else/1}).
-dialyzer({nowarn_function, yeccpars2_40_else/1}).
-compile({nowarn_unused_function,  yeccpars2_40_else/1}).
-file("src/grammer_erl/grammar_parser.yrl", 17).
yeccpars2_40_else(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ife, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_40_head/1}).
-dialyzer({nowarn_function, yeccpars2_40_head/1}).
-compile({nowarn_unused_function,  yeccpars2_40_head/1}).
-file("src/grammer_erl/grammar_parser.yrl", 17).
yeccpars2_40_head(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ife, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_40_in/1}).
-dialyzer({nowarn_function, yeccpars2_40_in/1}).
-compile({nowarn_unused_function,  yeccpars2_40_in/1}).
-file("src/grammer_erl/grammar_parser.yrl", 17).
yeccpars2_40_in(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ife, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_40_integer/1}).
-dialyzer({nowarn_function, yeccpars2_40_integer/1}).
-compile({nowarn_unused_function,  yeccpars2_40_integer/1}).
-file("src/grammer_erl/grammar_parser.yrl", 17).
yeccpars2_40_integer(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ife, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_40_let/1}).
-dialyzer({nowarn_function, yeccpars2_40_let/1}).
-compile({nowarn_unused_function,  yeccpars2_40_let/1}).
-file("src/grammer_erl/grammar_parser.yrl", 17).
yeccpars2_40_let(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ife, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_40_rec/1}).
-dialyzer({nowarn_function, yeccpars2_40_rec/1}).
-compile({nowarn_unused_function,  yeccpars2_40_rec/1}).
-file("src/grammer_erl/grammar_parser.yrl", 17).
yeccpars2_40_rec(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ife, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_40_ref/1}).
-dialyzer({nowarn_function, yeccpars2_40_ref/1}).
-compile({nowarn_unused_function,  yeccpars2_40_ref/1}).
-file("src/grammer_erl/grammar_parser.yrl", 17).
yeccpars2_40_ref(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ife, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_40_tail/1}).
-dialyzer({nowarn_function, yeccpars2_40_tail/1}).
-compile({nowarn_unused_function,  yeccpars2_40_tail/1}).
-file("src/grammer_erl/grammar_parser.yrl", 17).
yeccpars2_40_tail(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ife, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,yeccpars2_40_then/1}).
-dialyzer({nowarn_function, yeccpars2_40_then/1}).
-compile({nowarn_unused_function,  yeccpars2_40_then/1}).
-file("src/grammer_erl/grammar_parser.yrl", 17).
yeccpars2_40_then(__Stack0) ->
 [___6,___5,___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                                             {ife, ___2, ___4, ___6}
  end | __Stack].

-compile({inline,'yeccpars2_41_#'/1}).
-dialyzer({nowarn_function, 'yeccpars2_41_#'/1}).
-compile({nowarn_unused_function,  'yeccpars2_41_#'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 23).
'yeccpars2_41_#'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              {head, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_41_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_41_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_41_$end'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 23).
'yeccpars2_41_$end'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              {head, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_41_)'/1}).
-dialyzer({nowarn_function, 'yeccpars2_41_)'/1}).
-compile({nowarn_unused_function,  'yeccpars2_41_)'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 23).
'yeccpars2_41_)'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              {head, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_41_,'/1}).
-dialyzer({nowarn_function, 'yeccpars2_41_,'/1}).
-compile({nowarn_unused_function,  'yeccpars2_41_,'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 23).
'yeccpars2_41_,'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              {head, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_41_]'/1}).
-dialyzer({nowarn_function, 'yeccpars2_41_]'/1}).
-compile({nowarn_unused_function,  'yeccpars2_41_]'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 23).
'yeccpars2_41_]'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              {head, ___2}
  end | __Stack].

-compile({inline,yeccpars2_41_else/1}).
-dialyzer({nowarn_function, yeccpars2_41_else/1}).
-compile({nowarn_unused_function,  yeccpars2_41_else/1}).
-file("src/grammer_erl/grammar_parser.yrl", 23).
yeccpars2_41_else(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              {head, ___2}
  end | __Stack].

-compile({inline,yeccpars2_41_in/1}).
-dialyzer({nowarn_function, yeccpars2_41_in/1}).
-compile({nowarn_unused_function,  yeccpars2_41_in/1}).
-file("src/grammer_erl/grammar_parser.yrl", 23).
yeccpars2_41_in(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              {head, ___2}
  end | __Stack].

-compile({inline,yeccpars2_41_then/1}).
-dialyzer({nowarn_function, yeccpars2_41_then/1}).
-compile({nowarn_unused_function,  yeccpars2_41_then/1}).
-file("src/grammer_erl/grammar_parser.yrl", 23).
yeccpars2_41_then(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                              {head, ___2}
  end | __Stack].

-compile({inline,yeccpars2_43_/1}).
-dialyzer({nowarn_function, yeccpars2_43_/1}).
-compile({nowarn_unused_function,  yeccpars2_43_/1}).
-file("src/grammer_erl/grammar_parser.yrl", 40).
yeccpars2_43_(__Stack0) ->
 [___1 | __Stack] = __Stack0,
 [begin
                        ___1
  end | __Stack].

-compile({inline,yeccpars2_44_/1}).
-dialyzer({nowarn_function, yeccpars2_44_/1}).
-compile({nowarn_unused_function,  yeccpars2_44_/1}).
-file("src/grammer_erl/grammar_parser.yrl", 37).
yeccpars2_44_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                  {empty}
  end | __Stack].

-compile({inline,yeccpars2_46_/1}).
-dialyzer({nowarn_function, yeccpars2_46_/1}).
-compile({nowarn_unused_function,  yeccpars2_46_/1}).
-file("src/grammer_erl/grammar_parser.yrl", 41).
yeccpars2_46_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                      {cons, ___1, ___3}
  end | __Stack].

-compile({inline,yeccpars2_47_/1}).
-dialyzer({nowarn_function, yeccpars2_47_/1}).
-compile({nowarn_unused_function,  yeccpars2_47_/1}).
-file("src/grammer_erl/grammar_parser.yrl", 38).
yeccpars2_47_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                            ___2
  end | __Stack].

-compile({inline,yeccpars2_49_/1}).
-dialyzer({nowarn_function, yeccpars2_49_/1}).
-compile({nowarn_unused_function,  yeccpars2_49_/1}).
-file("src/grammer_erl/grammar_parser.yrl", 30).
yeccpars2_49_(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                      {unit}
  end | __Stack].

-compile({inline,yeccpars2_50_/1}).
-dialyzer({nowarn_function, yeccpars2_50_/1}).
-compile({nowarn_unused_function,  yeccpars2_50_/1}).
-file("src/grammer_erl/grammar_parser.yrl", 29).
yeccpars2_50_(__Stack0) ->
 [___3,___2,___1 | __Stack] = __Stack0,
 [begin
                               ___2
  end | __Stack].

-compile({inline,yeccpars2_53_/1}).
-dialyzer({nowarn_function, yeccpars2_53_/1}).
-compile({nowarn_unused_function,  yeccpars2_53_/1}).
-file("src/grammer_erl/grammar_parser.yrl", 27).
yeccpars2_53_(__Stack0) ->
 [___4,___3,___2,___1 | __Stack] = __Stack0,
 [begin
                                    {abs, ___2, ___4}
  end | __Stack].

-compile({inline,'yeccpars2_54_#'/1}).
-dialyzer({nowarn_function, 'yeccpars2_54_#'/1}).
-compile({nowarn_unused_function,  'yeccpars2_54_#'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 20).
'yeccpars2_54_#'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           {deref, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_54_$end'/1}).
-dialyzer({nowarn_function, 'yeccpars2_54_$end'/1}).
-compile({nowarn_unused_function,  'yeccpars2_54_$end'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 20).
'yeccpars2_54_$end'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           {deref, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_54_)'/1}).
-dialyzer({nowarn_function, 'yeccpars2_54_)'/1}).
-compile({nowarn_unused_function,  'yeccpars2_54_)'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 20).
'yeccpars2_54_)'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           {deref, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_54_,'/1}).
-dialyzer({nowarn_function, 'yeccpars2_54_,'/1}).
-compile({nowarn_unused_function,  'yeccpars2_54_,'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 20).
'yeccpars2_54_,'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           {deref, ___2}
  end | __Stack].

-compile({inline,'yeccpars2_54_]'/1}).
-dialyzer({nowarn_function, 'yeccpars2_54_]'/1}).
-compile({nowarn_unused_function,  'yeccpars2_54_]'/1}).
-file("src/grammer_erl/grammar_parser.yrl", 20).
'yeccpars2_54_]'(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           {deref, ___2}
  end | __Stack].

-compile({inline,yeccpars2_54_else/1}).
-dialyzer({nowarn_function, yeccpars2_54_else/1}).
-compile({nowarn_unused_function,  yeccpars2_54_else/1}).
-file("src/grammer_erl/grammar_parser.yrl", 20).
yeccpars2_54_else(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           {deref, ___2}
  end | __Stack].

-compile({inline,yeccpars2_54_head/1}).
-dialyzer({nowarn_function, yeccpars2_54_head/1}).
-compile({nowarn_unused_function,  yeccpars2_54_head/1}).
-file("src/grammer_erl/grammar_parser.yrl", 20).
yeccpars2_54_head(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           {deref, ___2}
  end | __Stack].

-compile({inline,yeccpars2_54_in/1}).
-dialyzer({nowarn_function, yeccpars2_54_in/1}).
-compile({nowarn_unused_function,  yeccpars2_54_in/1}).
-file("src/grammer_erl/grammar_parser.yrl", 20).
yeccpars2_54_in(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           {deref, ___2}
  end | __Stack].

-compile({inline,yeccpars2_54_rec/1}).
-dialyzer({nowarn_function, yeccpars2_54_rec/1}).
-compile({nowarn_unused_function,  yeccpars2_54_rec/1}).
-file("src/grammer_erl/grammar_parser.yrl", 20).
yeccpars2_54_rec(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           {deref, ___2}
  end | __Stack].

-compile({inline,yeccpars2_54_ref/1}).
-dialyzer({nowarn_function, yeccpars2_54_ref/1}).
-compile({nowarn_unused_function,  yeccpars2_54_ref/1}).
-file("src/grammer_erl/grammar_parser.yrl", 20).
yeccpars2_54_ref(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           {deref, ___2}
  end | __Stack].

-compile({inline,yeccpars2_54_tail/1}).
-dialyzer({nowarn_function, yeccpars2_54_tail/1}).
-compile({nowarn_unused_function,  yeccpars2_54_tail/1}).
-file("src/grammer_erl/grammar_parser.yrl", 20).
yeccpars2_54_tail(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           {deref, ___2}
  end | __Stack].

-compile({inline,yeccpars2_54_then/1}).
-dialyzer({nowarn_function, yeccpars2_54_then/1}).
-compile({nowarn_unused_function,  yeccpars2_54_then/1}).
-file("src/grammer_erl/grammar_parser.yrl", 20).
yeccpars2_54_then(__Stack0) ->
 [___2,___1 | __Stack] = __Stack0,
 [begin
                           {deref, ___2}
  end | __Stack].


