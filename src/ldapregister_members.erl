-module(ldapregister_members).

-behaviour(gen_server).

%% API
-export([start_link/0,add_members/1,add_member/4,process_candidates/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-include("ldap_register.hrl").
-record(state, {
    }).

add_member(Name,Surname,Mobile,Email) ->
    Member = #ldap_member{name=Name,surname=Surname,mobile=Mobile,email=Email},
    add_members([Member]).

add_members(ListOfMembers) ->
    gen_server:call(?MODULE,{add_members,ListOfMembers}).

process_candidates() ->
    gen_server:call(?MODULE,process_candidates).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Callbacks
init([]) ->
    {ok, #state{}}.

handle_call({add_members,ListOfMembers}, _From, State) ->
    {reply, add_candidates(ListOfMembers), State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

add_candidates(ListOfMembers) ->
    Adder = fun () ->
            [add_candidate(Member) || Member  <- ListOfMembers]
    end,
    case  mnesia:transaction(Adder,1) of
        {atomic,List} -> {ok,List};
        Else -> Else
    end.


add_candidate(Member) ->
    Hash = create_ticket(Member),
    case member_exists(Member) of
        {true,StoredTicket} -> 
            Member#ldap_member{ticket_id=StoredTicket};
        false ->
            Member1=Member#ldap_member{ticket_id=Hash,progress=added},
            ok = mnesia:write(Member1),
            Member1
    end.

create_ticket(#ldap_member{name=Name,surname=Surname,email=Email,mobile=Mobile}) ->
   {ok, Salt} = application:get_env(shasalt),
   Hash = crypto:hash(sha256,unicode:characters_to_binary(Salt ++ Name ++ Surname ++ Email ++ Mobile)),
   lists:flatten([io_lib:format("~2.16.2b",[Val]) || Val <- binary_to_list(Hash)]).

member_exists(#ldap_member{name=Name,surname=Surname}) ->
    Search = #ldap_member{ticket_id = '$1', name=Name,surname=Surname,_='_'},
    case mnesia:select(ldap_member,[{Search,[],['$1']}]) of
        [] -> false;
        [Hash] -> {true,Hash}
    end.


