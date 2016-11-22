-module(ldapregister_email).
-include("ldap_register.hrl").
-export([send_link/1]).

%Added ldapregister ENV mailFrom, relay,relay_user,relay_pass

send_link(#ldap_member{name=Name,surname=Surname,email=Email,ticket_id=TID}) ->
    {ok,MailFrom}  = application:get_env(mailFrom),
    {ok,Relay}     = application:get_env(relay),
    {ok,RelayUser} = application:get_env(relay_user),
    {ok,RelayPass} = application:get_env(relay_pass),
    {From,To,Mime} = mail_plain( 
        unicode:characters_to_binary(MailFrom),
        unicode:characters_to_binary(Name ++" " ++ Surname ++ "<"++Email++">"),
        "Kviečiu užsiregistruoti vieningoje Technarium autentikavimoso sistemoje / Please register on unified Technarium authentication system",
        generate_body(TID),
        []),
    gen_smtp_client:send(
        {From,To,Mime},
        [{relay,Relay},{username,RelayUser},{password,RelayPass},{port,25},{tls,always}]).

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

generate_body(TID) ->
Text =
"Sveiki,

šiuo metu Technariume diegiame vieningą autentikavimąsi prie Technariumo
teikiamų elektroninių paslaugų (Wiki, cloud ir t.t.). Jau greitai visomis
Technarium teikiamomis elektroninėmis paslaugomis bus galima naudotis
prisijungiant tik jum skirtu vartotojo vardu ir slaptažodžiu. Vartotojo
vardą ir slaptažodį galite susigalvoti patys, tačiau vartotojo vardas
turi būti unikalus ir jo pasikeisti nebus galima.
Prašau užsiregistruokite sekdami nuorodą:
https://ldap.technarium.lt:8080/register/lt/" ++ TID ++ "
Nuoroda bus deaktyvuota po sėkmingos registracijos.


Hi,

we are deploying a unified autenthication method to all electronic services
(Wiki, cloud, etc.) provided by Technarium hackerspace. Soon you will be able to
login to all of our services using your dedicated username and password. You
can choose your username and password. Your username should be unique and you
will not be able to change it later. 

Please register by folowing this link:
https://ldap.technarium.lt:8080/register/en/" ++ TID ++
"
This link will be de-actived after succesful registration.

Pagarbiai / Best Regards
Ričardas Pocius
(su Technarium IT admino kepure  / with IT admin hat)",
unicode:characters_to_binary(Text).



