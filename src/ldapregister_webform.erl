-module(ldapregister_webform).

-export([init/3,handle/2,terminate/3]).
-include("ldap_register.hrl").

-record(state,{
    lang,
    member}).

-define(HTML,{<<"content-type">>,<<"text/html; charset=utf-8">>}).
-define(PLAIN,{<<"content-type">>,<<"text/plain; charset=utf-8">>}).


init({ssl,_},Req,_Opts) ->
    {Lang,Req1} = cowboy_req:binding(lang,Req,no_value),
    {Ticket_id,Req2} = cowboy_req:binding(ticket_id,Req1,no_ticket),
    case {valid_lang(Lang), ticket_status(binary_to_list(Ticket_id))} of
        {{true,UsedLang},{online,Member}} ->
             {ok,Req,#state{lang=UsedLang,member=Member}};
        {{true,en},{activated,_Member}} ->
            {ok,Req2_1} = cowboy_req:reply(200,[?PLAIN], uni("Thank you ! We succesfuly received the data"),Req2),
            {shutdown,Req2_1,no_state};
        {{true,lt},{activated,_Member}} ->
            {ok,Req2_1} = cowboy_req:reply(200,[?PLAIN], uni("Ačiū ! Jūsų duomenys priimti sėkmingai"),Req2),
            {shutdown,Req2_1,no_state};
        {false,_} ->
            {ok,Req2_1} = cowboy_req:reply(500,[?PLAIN], uni("I haz no tongue for your language, try english ???"),Req2),
            {shutdown,Req2_1,no_state};
        {O1,O2} ->
            lager:debug("Other(~tp,~tp)",[O1,O2]),
            {ok,Req2_1} = cowboy_req:reply(500,[?PLAIN], uni("Something went South ..."),Req2),
            {shutdown,Req2_1,no_state}
    end.

handle(Req,State = #state{member=Member,lang=Lang}) ->
    lager:debug("req:~tp,~n state:~tp",[Req,State]),
    {Method, Req2} = cowboy_req:method(Req),
    {Resp,Headers,Body} = case Method of
        <<"GET">>  -> generate_form(Lang,Member);
        <<"POST">> -> validate_and_respond(Lang,Member,Req);
        _Else -> {405,[?PLAIN], uni("I haz no way to respond ...")}
    end,
    lager:debug("~n~nResp code:~n~tp~n,Headers:~n~tp~nBody:~n~tp",[Resp,Headers,Body]),
    {ok,Req_final} = cowboy_req:reply(Resp,Headers,Body,Req2),
    {ok,Req_final,State}.

terminate(_Reason,_Req,_State) -> ok.

% Internal mess
valid_lang(<<"lt">>) -> {true,lt};
valid_lang(<<"en">>) -> {true,en};
valid_lang(_) -> false.

ticket_status(not_valid) -> not_valid;
ticket_status(Ticket_id) ->
    case ldapregister_members:get_member(Ticket_id) of
        {ok,M = #ldap_member{progress=P}} when P == online -> {online,M};
        {ok,M = #ldap_member{progress=P}} when P == activated -> {activated,M};
        not_found -> not_found
    end.

uni(Txt) -> unicode:characters_to_binary(Txt,utf8).

generate_form(Lang,Member) ->
    generate_form(Lang,Member,#{},[]).

generate_form(Lang,#ldap_member{name=MName,surname=MSurname,mobile=MMobile,email=MEmail},EnteredFields,ValidationErrors) ->
    MemberDetails = [{"m_name",MName},{"m_surname",MSurname},{"m_mobile_nr",MMobile},{"m_email",MEmail}],
    AllFields = [v_username, v_password1,v_password2,v_confirm],
    AllErrors = [error_username_too_short, error_username_bad_chars,error_username_not_uniq,
                 error_password_too_short,error_password_bad_chars,error_password_not_equal,
                 error_confirm_not_checked],
%% Populate CSS style model to display:inline; for DIV's corresponding to error msgs
    Errors = lists:map(
        fun(Err) ->
            case lists:member(Err,ValidationErrors) of
                true -> {atom_to_list(Err),"inline"};
                false -> {atom_to_list(Err),"none"}
            end
        end,
        AllErrors),
%% Prepopulate already entered fields 
    Fields = lists:map( fun(Param) -> {atom_to_list(Param),maps:get(Param,EnteredFields,"")} end, AllFields),
    LocalisedText = ldapregister_lang:get_strings(Lang),
    Model = Fields ++ Errors ++ MemberDetails ++ LocalisedText,
    {ok,Rendered} = ldapregister_template:render_file("registration_form_html",Model),
    {200,[?HTML],uni(Rendered)}.

validate_and_respond(Lang,Member,Req) ->
    {ok,KeyVals,_} = cowboy_req:body_qs(Req),
    FormFields = [v_username,v_password1,v_password2,v_confirm],
    Fields = lists:foldl(
                fun(Field,Acc) ->
                     maps:put(Field,unicode:characters_to_list(proplists:get_value(atom_to_binary(Field,latin1),KeyVals,<<"">>),utf8),Acc)
                end,
                maps:new(),
                FormFields),
    lager:debug("form fields:~tp",[Fields]),

%%        {Group,[{CheckFun,ErrorCode}|...],[SavedFields]} where CheckFun is Fun(Group,Fields) -> true, false
    ValidationRules = [
        {username,
         [{fun is_username_bad_chars/2,error_username_bad_chars},
          {fun is_username_too_short/2,error_username_too_short},
          {fun is_username_not_unique/2,error_username_not_unique}],
         [v_username] },
        {password,
         [{fun is_password_bad_chars/2,error_password_bad_chars},
          {fun is_password_too_short/2,error_password_too_short},
          {fun is_password_not_equal/2,error_password_not_equal}],
         [v_password1,v_password2] },
        {confirm,
         [{fun is_confirm_not_checked/2,error_confirm_not_checked}],
         [v_confirm] }],
    case form_data_validation(Fields,ValidationRules) of
       {ok,ValidFields} ->
            lager:debug("Validation SUCCESS with Fields:~tp",[ValidFields]),
            #{v_username:=Username,v_password1:=Password} = ValidFields,
                case ldapregister_members:set_userpass(Member#ldap_member.ticket_id,Username,Password) of
                    not_unique -> generate_form(Lang,Member,ValidFields,[error_username_not_unique]);
                    ok ->  {303,[{<<"location">>,uni(ldapregister_template:generate_user_registration_url(Lang,Member))}],uni("")}
                end;
        {fail,ValidFields,Errors} ->
            lager:debug("Validation FAILED with Fields:~tp, Errors:~tp",[ValidFields,Errors]),
            generate_form(Lang,Member,ValidFields,Errors)
    end.

form_data_validation(Fields,Rules) ->
    ValidateEachGroup = fun ({GroupName,Tests,CandidateFields},{GFields,GErrors}) ->
        Test = fun 
            ({TestFun,ErrorName},{ok,TErrors}) ->
                case TestFun(GroupName,Fields) of
                    false -> {ok,TErrors};
                    true -> {fail,TErrors++[ErrorName]}
                end;
            ({_,_},{fail,Errors}) -> {fail,Errors}
        end,

        case lists:foldl(Test,{ok,[]},Tests) of
            {ok,[]} -> {add_fields_to_map(CandidateFields,GFields,Fields),GErrors};
            {fail,AfterTestErrors} ->{GFields,GErrors++AfterTestErrors}
        end
    end,
    case lists:foldl(ValidateEachGroup,{maps:new(),[]},Rules) of
        {ValidFields,[]} -> {ok,ValidFields};
        {ValidFields,Errors} -> {fail,ValidFields,Errors}
    end.

add_fields_to_map([],Map,_Fields) -> Map;
add_fields_to_map([Field|Rest],Map,Fields) ->
    #{Field:=Value} = Fields,
    add_fields_to_map(Rest,Map#{Field=>Value},Fields).


is_username_bad_chars(_G,#{v_username:=Username}) ->
    not is_valid_username_chars(Username).

is_username_too_short(_G,#{v_username:=Username}) when length(Username) < 2 -> true;
is_username_too_short(_G,_) -> false.

is_username_not_unique(_G,#{v_username:=Username}) ->
    ldapregister_members:user_exists(Username).

is_password_bad_chars(_G,#{v_password1:=Pass}) ->
    not is_valid_password_chars(Pass).

is_password_too_short(_G,#{v_password1:=Pass}) when length(Pass) < 6 -> true;
is_password_too_short(_G,_) -> false.

is_password_not_equal(_G,#{v_password1:=P,v_password2:=P}) -> false;
is_password_not_equal(_G,_) -> true.

is_confirm_not_checked(_G,#{v_confirm:="checked"}) -> false;
is_confirm_not_checked(_G,_) -> true.

is_valid_username_chars(Username) ->
    is_valid_username_chars(Username,true).
is_valid_username_chars([],true) -> true;
is_valid_username_chars([U|Sername],true) when (U >=48 andalso U =< 57) ; (U >=65 andalso U=<90);( U>=97 andalso U =< 122) -> % alphanumeric
    is_valid_username_chars(Sername,true);
is_valid_username_chars(_,_) -> false.

is_valid_password_chars(Pass) ->
    is_valid_password_chars(Pass,true).
is_valid_password_chars([],true) -> true;
is_valid_password_chars([P|Ass],true) when P >= 32 andalso P < 127 ->
    is_valid_password_chars(Ass,true);
is_valid_password_chars(_,_) -> false.

