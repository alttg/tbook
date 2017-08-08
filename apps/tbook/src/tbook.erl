-module(tbook).


-export([to_list/1,to_binary/1,to_integer/1,to_atom/1]).


to_list(String) when is_list(String) -> String;
to_list(Float) when is_float(Float) -> float_to_list(Float);
to_list(Int) when is_integer(Int) -> integer_to_list(Int);
to_list(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_list(Binary) when is_binary(Binary) -> binary_to_list(Binary).

to_binary(Integer) when is_integer(Integer) -> to_binary(integer_to_list(Integer));
to_binary(Binary) when is_binary(Binary) -> Binary;
to_binary(String) when is_list(String) -> list_to_binary(String).

to_integer(Val) when is_integer(Val) -> Val;
to_integer(Val) when is_binary(Val) -> to_integer(binary_to_list(Val));
to_integer(Val) when is_list(Val) -> list_to_integer(Val);
to_integer(Val) when (Val == null) -> 0.

to_atom(A) when is_atom(A) -> A;
to_atom(B) when is_binary(B) -> binary_to_atom(B, latin1);
to_atom(S) when is_integer(hd(S)) -> list_to_atom(S).