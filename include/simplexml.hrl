%%
%% @doc Constants and records for XML
%%

-define(is_whitespace(C), C =:= 16#20; C =:= 16#9; C =:= 16#D; C =:= 16#A).
-define(is_namestartchar(C),
    C =:= $:; C =:= $_; (C >= $a andalso C =< $z); (C >= $A andalso C =< $Z)).
-define(is_namechar(C),
    ?is_namestartchar(C); C =:= $-; C =:= $.; (C >= $0 andalso C =< $9)).
-define(is_attrvaluechar(C, Q), C =/= $<, C =/= Q).
-define(is_quote(C), C =:= $'; C =:= $").

%% File location information
-record(location, {
    source=unknown,
    line=1,
    column=1
    }).

-define(inc_col(L, N), L#location{column=L#location.column + N}).
-define(inc_line(L), L#location{line=L#location.line + 1, column=1}).
