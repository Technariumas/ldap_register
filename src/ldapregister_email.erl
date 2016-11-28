-module(ldapregister_email).
-include("ldap_register.hrl").
-export([send_link/1]).

%Added ldapregister ENV mailFrom, relay,relay_user,relay_pass

send_link(#ldap_member{name=Name,surname=Surname,email=Email}=Member) ->
    {ok,MailFrom}  = application:get_env(mailFrom),
    {ok,Relay}     = application:get_env(relay),
    {ok,RelayUser} = application:get_env(relay_user),
    {ok,RelayPass} = application:get_env(relay_pass),
    {From,To,Mime} = mail_plain( 
        unicode:characters_to_binary(MailFrom),
        unicode:characters_to_binary(Name ++" " ++ Surname ++ "<"++Email++">"),
        "Kviečiu užsiregistruoti vieningoje Technarium autentikavimoso sistemoje / Please register on unified Technarium authentication system",
        generate_invitation_body(Member),
        []),
    gen_smtp_client:send(
        {From,To,Mime},
        [{relay,Relay},{username,RelayUser},{password,RelayPass},{port,25},{tls,always}]).

generate_invitation_body(Member) ->
    URL_en = ldapregister_template:generate_user_registration_url(en,Member),
    URL_lt = ldapregister_template:generate_user_registration_url(lt,Member),
    Data = [{"user_registration_url_lt",URL_lt},{"user_registration_url_en",URL_en}],
    {ok,Text} = ldapregister_template:render_file("please_register_mail",Data),
    unicode:characters_to_binary(Text).


mail_plain(From, To, Subject, Body, Opts) ->
    AddHeaders = proplists:get_value(headers, Opts, []),
    Mimemail =
        {<<"text">>, <<"plain">>,
         [
          {<<"From">>, From},
          {<<"To">>, To},
          {<<"Subject">>, <<"=?UTF-8?B?",(base64:encode(unicode:characters_to_binary(Subject)))/binary,"?=">>},
          {<<"Content-Type">>, <<"text/plain; charset=utf-8">>}
          | AddHeaders],
         [{<<"transfer-encoding">>, <<"base64">>}],
         Body},
    FromAddr = extract_addr_rfc822(From),
    {FromAddr, [extract_addr_rfc822(To)], mimemail:encode(Mimemail)}.

extract_addr_rfc822(Rfc822) ->
    {ok, [{_, Addr}]} = smtp_util:parse_rfc822_addresses(Rfc822),
    list_to_binary(Addr).


