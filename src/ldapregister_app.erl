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
    ok.

create_tables(Nodes) ->
    {atomic,ok} = mnesia:create_table(
        ldap_member,
        [ {type,set},
          {attributes,record_info(fields,ldap_member)},
          {disc_copies,Nodes}]).

