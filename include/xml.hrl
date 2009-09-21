%%
%% @doc Constants for XML
%%

-define(is_whitespace(C), C =:= 16#20; C =:= 16#9; C =:= 16#D; C =:= 16#A).
-define(is_namestartchar(C),
    C =:= $:; C =:= $_; (C >= $a andalso C =< $z); (C >= $A andalso C =< $Z)).
-define(is_namechar(C),
    ?is_namestartchar(C); C =:= $-; C =:= $.; (C >= $0 andalso C =< $9)).
