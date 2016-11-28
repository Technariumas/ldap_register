-module(ldapregister_members).

-behaviour(gen_server).

%% API
-export([start_link/0,
         add_members/1,
         add_member/4,
         process_candidates/0,
         get_member/1,
         set_userpass/3,
         user_exists/1]).

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

get_member(Ticket_id) ->
    gen_server:call(?MODULE,{get_member,Ticket_id}).

user_exists(Username) ->
    gen_server:call(?MODULE,{check_user_exists,Username}).


set_userpass(Ticket_id,Username,Password) ->
    gen_server:call(?MODULE,{set_userpass,Ticket_id,Username,Password}).

    
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Callbacks
init([]) ->
    {ok, #state{}}.

handle_call({add_members,ListOfMembers}, _From, State) ->
    {reply, add_candidates(ListOfMembers), State};

handle_call(process_candidates, _From,State) ->
    gen_server:cast(self(), process_candidate),
    {reply,ok,State};


handle_call({get_member,Ticket_id}, _From,State) ->
    LookupFun = fun(Id) ->
        mnesia:read(ldap_member,Id)
    end,
    case mnesia:transaction(LookupFun,[Ticket_id],10) of
        {atomic,[M]} -> {reply,{ok,M},State};
        _Else -> {reply,not_found,State}
    end;

handle_call({set_userpass,Ticket_id,Username,Password}, _From,State) ->
    SetUserPassFun = fun() ->
        case mnesia:select(ldap_member,[{ #ldap_member{uid='$1',_='_'},[{'==','$1', Username}],['$1']}]) of
            [] -> 
                [M] = mnesia:read(ldap_member,Ticket_id),
                ok = mnesia:write(M#ldap_member{uid=Username,password=ldapregister_secrets:generate_ssha(Password),progress=activated});
            [_Else] ->not_unique
        end
    end,
    case mnesia:transaction(SetUserPassFun,10) of
        {atomic,ok} -> {reply,ok,State};
        {atomic,not_unique} -> {reply,ok,State};
        Else -> {reply,{error,Else},State}
    end;

handle_call({check_user_exists,Username}, _From,State) ->
    SearchFun = fun() ->
        mnesia:select(ldap_member,[{ #ldap_member{uid='$1',_='_'},[{'==','$1', Username}],['$1']}])
    end,
    case mnesia:transaction(SearchFun,10) of
        {atomic,[]} -> {reply,false,State};
        {atomic,[Username]} -> {reply,true,State};
        Else -> {reply,{error,Else},State}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(process_candidate, State) ->
    SelectFun = fun() ->
        mnesia:select(ldap_member,[{ #ldap_member{progress='$1',_='_'},[{'==','$1', added}],['$_']}])
    end,
    UpdateStatusFun = fun(M) ->
        mnesia:write(M#ldap_member{progress=online})
    end,
    case mnesia:transaction(SelectFun,10) of
        {atomic,[Member|_Rest]} ->
            gen_server:cast(self(),process_candidate),
            case ldapregister_email:send_link(Member) of
                {ok,_} ->
                   {atomic, ok} =  mnesia:transaction(UpdateStatusFun,[Member],10),
                    {noreply,State};
                Result  -> 
                    {stop,{send_link_failed,Result},State}
            end;
        {atomic,[]} ->
            {noreply,State};
        Result1 ->
               %Rise alarm ?
         {stop,{unexpected_result,Result1},State}
    end;
    
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





