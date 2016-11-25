-module(ldapregister_app).

-behaviour(application).

%% User API
-export([start/0,stop/0]).

%% Application callbacks
-export([start/2, stop/1, start_phase/3]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-include("ldap_register.hrl").

start() ->
    {ok,_} = application:ensure_all_started(ldapregister).

stop() ->
    ok = application:stop(ldapregister).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ldapregister_sup:start_link().

stop(_State) ->
    ok.

start_phase(create_schema, _StarType,[]) ->
    _ = application:stop(mnesia),
    Node = node(),
    case mnesia:create_schema([Node]) of
        ok ->
            _ = application:ensure_all_started(mnesia),
            create_tables([Node]),
            ok;
        {error,{Node,{already_exists,Node}}} ->
            _ = application:ensure_all_started(mnesia),
            ok
    end;

start_phase(app_start,_StartType,[]) ->
    supervisor:start_child(ldapregister_sup,?CHILD(ldapregister_members,worker)),

    Dispatch = cowboy_router:compile(
        [%host list
         {'_',
          [ %path list
           {"/register/:lang/:ticket_id", ldapregister_webform, []},
           {"/static/[...]", cowboy_static, {dir, code:priv_dir(ldapregister)++"/static"}}
          ]}
        ]),

    {ok,_Pid} = cowboy:start_https(
        https, % ranch supervisor child id
        10, % How many listeners
        [
         {port,application:get_env(ldapregister,tls_port,8081)},
         {cacertfile,application:get_env(ldapregister,tls_cacertfile,"cacert.crt")},
         {certfile,application:get_env(ldapregister,tls_certfile,"cert.crt")},
         {keyfile,application:get_env(ldapregister,tls_keyfile,"keyfile.key")},
         {versions,['tlsv1.2', 'tlsv1.1', 'tlsv1']},
         {ciphers, ["ECDHE-ECDSA-AES256-GCM-SHA384","ECDHE-RSA-AES256-GCM-SHA384",
            "ECDHE-ECDSA-AES256-SHA384","ECDHE-RSA-AES256-SHA384", "ECDHE-ECDSA-DES-CBC3-SHA",
            "ECDH-ECDSA-AES256-GCM-SHA384","ECDH-RSA-AES256-GCM-SHA384","ECDH-ECDSA-AES256-SHA384",
            "ECDH-RSA-AES256-SHA384","DHE-DSS-AES256-GCM-SHA384","DHE-DSS-AES256-SHA256",
            "AES256-GCM-SHA384","AES256-SHA256","ECDHE-ECDSA-AES128-GCM-SHA256",
            "ECDHE-RSA-AES128-GCM-SHA256","ECDHE-ECDSA-AES128-SHA256","ECDHE-RSA-AES128-SHA256",
            "ECDH-ECDSA-AES128-GCM-SHA256","ECDH-RSA-AES128-GCM-SHA256","ECDH-ECDSA-AES128-SHA256",
            "ECDH-RSA-AES128-SHA256","DHE-DSS-AES128-GCM-SHA256","DHE-DSS-AES128-SHA256",
            "AES128-GCM-SHA256","AES128-SHA256","ECDHE-ECDSA-AES256-SHA",
            "ECDHE-RSA-AES256-SHA","DHE-DSS-AES256-SHA","ECDH-ECDSA-AES256-SHA",
            "ECDH-RSA-AES256-SHA","AES256-SHA","ECDHE-ECDSA-AES128-SHA",
            "ECDHE-RSA-AES128-SHA","DHE-DSS-AES128-SHA","ECDH-ECDSA-AES128-SHA",
            "ECDH-RSA-AES128-SHA","AES128-SHA"]},
        {secure_renegotiate, true},
        {reuse_sessions, true},
        {honor_cipher_order, true},
        {max_connections, infinity}
        ],
        [{max_keepalive, 1024}, {env, [{dispatch, Dispatch}]}]),
     ok.

create_tables(Nodes) ->
    {atomic,ok} = mnesia:create_table(
        ldap_member,
        [ {type,set},
          {attributes,record_info(fields,ldap_member)},
          {disc_copies,Nodes}]).

