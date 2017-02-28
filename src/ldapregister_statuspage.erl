-module(ldapregister_statuspage).
-export([init/3,handle/2,terminate/3]).

-define(PLAIN,{<<"content-type">>,<<"text/plain; charset=utf-8">>}).

init({ssl,_},Req,_Opts) ->
    cowboy_req:reply(200,[?PLAIN], uni("online"),Req),
    {shutdown,Req,no_state}.

handle(Req,State) ->
    {ok,Req,State}.

terminate(_Reason,_Req,_State) -> ok.

uni(Txt) -> unicode:characters_to_binary(Txt,utf8).


