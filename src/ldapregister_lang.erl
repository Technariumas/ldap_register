-module(ldapregister_lang).

-define(VALID_LANG,["lt","en"]).

-export([get_strings/1]).

get_strings(Lang) when is_atom(Lang) ->
    get_strings(atom_to_list(Lang));
get_strings(Lang) ->
    case lists:member(Lang,?VALID_LANG) of
        true ->
            {ok,[Strings]} = file:consult(code:priv_dir(ldapregister)++"/lang/" ++ Lang),
            Strings;
        false -> []
    end.


