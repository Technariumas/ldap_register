-module(ldapregister_template).
-export([render_file/2, render/2,
         generate_user_registration_url/2]).

-include("ldap_register.hrl").



render_file(FileName,Model) ->
    {ok,Template } = file:read_file(code:priv_dir(ldapregister)++"/templates/"++ FileName),
    render(Template,Model).


render(View,Model) when is_binary(View) ->
    ListView=unicode:characters_to_list(View,utf8),
    render(ListView,Model);

render(View,Model) ->
    Replacer = fun
        ($$,{copy,Acumulator,Processed}) ->
            {replace,Acumulator,Processed};
        ($$,{replace,Acumulator,Processed}) ->
            Replacement = proplists:get_value(lists:reverse(Acumulator),Model,"MISSSING VAR:" ++ lists:reverse(Acumulator)),
            {copy,[],prepend(Replacement,Processed)};
        (Char,{replace,Acumulator,Processed}) ->
            {replace,[Char|Acumulator],Processed};
        (Char,{copy,Acumulator,Processed}) when length(Acumulator)==0->
            {copy,Acumulator,[Char|Processed]}
    end,
    case lists:foldl(Replacer,{copy,[],[]},View) of
        {copy,[],ReversedResult} -> {ok,lists:reverse(ReversedResult)};
        {replace,Val,_} -> {error,{missing_end_of_val,lists:reverse(Val)}};
        Else -> {error,{unspecified_returnval,Else}}
    end.

prepend([],Processed) -> Processed;
prepend([H|T],Processed) -> prepend(T,[H|Processed]).

generate_user_registration_url(Lang,#ldap_member{ticket_id=Ticket_id}) ->
    case application:get_env(ldapregister,base_url,'$none') of
        '$none' -> throw(base_url_not_defined_in_app_config);
        BaseURL -> BaseURL ++ "/register/" ++ atom_to_list(Lang) ++ "/" ++ Ticket_id
    end.




