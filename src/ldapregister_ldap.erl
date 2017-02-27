-module(ldapregister_ldap).
-include("ldap_register.hrl").
-compile(export_all).
-export([push_member/1,user_exists/1]).


push_member(#ldap_member{uid=UID}=Member) ->
    Conn = connect(),
    Result  = case uid_exists(Conn,UID) of
        false -> insert_member(Conn,Member);
        {true,DN} ->
            ok = case is_group_member(Conn,DN) of
                true -> ok;
                false -> add_to_group(Conn,DN)
            end,
            update_record(Conn,DN,Member)
    end,
    disconnect(Conn),
    Result.

-spec user_exists(UID :: list()) -> true | false.
user_exists(UID) ->
    Conn = connect(),
    Result = case uid_exists(Conn,UID) of
        false -> false;
        {true,UserDN} -> lager:info("user_exists:~tp",[UserDN]),
        true
    end,
    disconnect(Conn),
    Result.


connect() ->
    Server = application:get_env(ldapregister,ldap_hostname,"localhost"),
    BindUser = application:get_env(ldapregister,ldap_user,"baduser"),
    BindPass = application:get_env(ldapregister,ldap_pass,"badpass"),
    CACertOpt = case application:get_env(ldapregister,ldap_cacertfile, undefined) of
        undefined -> [];
        CACertFile -> [{sslopts,[{cacertfile,CACertFile}]}]
    end,
    DebugOpt = 
        case application:get_env(ldapregister,ldap_debug,false) of
            true ->
                [{log,fun(_Level,Format,Args) -> lager:debug("ldap_debug:" ++ Format,Args) end}];
            false -> []
        end,
    {ok,Handle} = eldap:open([Server],[{ssl,true}, {port,636}] ++ CACertOpt ++ DebugOpt),
    case eldap:simple_bind(Handle,BindUser,BindPass) of
        ok ->
            Handle;
        Else ->
            disconnect(Handle),
            Else
        end.

disconnect(Handle) ->
    eldap:close(Handle).

uid_exists(Conn,Uid) ->
    {ok,BaseDN} = application:get_env(ldapregister,ldap_members_dn),
    Filter = eldap:equalityMatch("uid",Uid),
    case eldap:search(Conn, [{base,BaseDN},{filter,Filter},{attributes,["cn"]}]) of
        {ok,{eldap_search_result,[],[]}} -> false;
        {ok,{eldap_search_result,[{eldap_entry,DN,_}|_Rest],[]}} ->
            {true,from_ldap(DN)};
        Else -> Else
    end.

insert_member(Conn,#ldap_member{uid=U,password=P,name=Name,surname=Surname,email=Email,mobile=Mobile}) ->

    {ok,MembersDN} = application:get_env(ldapregister,ldap_members_dn),
    {ok,GroupDN} = application:get_env(ldapregister,ldap_group_dn),
    DisplayName = Name ++ " " ++ Surname,
    DN = "cn="  ++DisplayName ++ "," ++ MembersDN,
    Rec = [
    {"objectclass",["top","inetorgperson"]},
    {"cn",[to_ldap(DisplayName)]},
    {"givenname",[to_ldap(Name)]},
    {"sn",[to_ldap(Surname)]},
    {"displayname",[to_ldap(DisplayName)]},
    {"mail",[Email]},
    {"mobile",[Mobile]},
    {"uid",[U]},
    {"userpassword",[P]}
    ],
    Mod = [eldap:mod_add("uniquemember",[to_ldap(DN)])],
    case eldap:add(Conn,to_ldap(DN),Rec) of
        ok -> eldap:modify(Conn,GroupDN,Mod);
        Else -> Else
    end.

update_record(Conn,DN,#ldap_member{email=Email,password=Password}) ->
    Mod = [eldap:mod_replace("mail",[Email]),eldap:mod_replace("userpassword",[Password])],
    eldap:modify(Conn,to_ldap(DN),Mod).

is_group_member(Conn,DN) ->
    {ok,GroupDN} = application:get_env(ldapregister,ldap_group_dn),
    Filter = eldap:equalityMatch("uniquemember",to_ldap(DN)),
    case eldap:search(Conn, [{base,GroupDN},{filter,Filter},{attributes,["dn"]}]) of
        {ok,{eldap_search_result,[],[]}} -> false;
        {ok,{eldap_search_result,[{eldap_entry,GroupDN,_}|_Rest],[]}} -> true;
        Else -> Else
    end.

add_to_group(Conn,DN) ->
    {ok,GroupDN} = application:get_env(ldapregister,ldap_group_dn),
    Mod = [eldap:mod_add("uniquemember",[to_ldap(DN)])],
    case eldap:modify(Conn,GroupDN,Mod) of
        {ok,_Result} -> ok;
        Else -> Else
    end.

to_ldap(String) ->
    binary_to_list(unicode:characters_to_binary(String)).

from_ldap(String) ->
    unicode:characters_to_list(list_to_binary(String),utf8).


