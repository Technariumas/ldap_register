-module(ldapregister_secrets).
-export([generate_ssha/1,validate_ssha/2,make_salt/0]).

generate_ssha(Pass) ->
    Salt = make_salt(),
    generate_ssha(Pass,Salt).

generate_ssha(Pass,Salt) ->
    EncodedHash = binary_to_list(crypto:hash(sha,Pass++Salt)),
    "{SSHA}" ++ binary_to_list(base64:encode(EncodedHash++Salt)).

make_salt() ->
    printable_sha(crypto:hash(sha,term_to_binary(erlang:make_ref()))).

printable_sha(SHA) ->
    lists:flatten([ io_lib:format("~2.16.2b",[Byte]) || Byte <- binary_to_list(SHA)]).

validate_ssha(ClearPassword, SshaHash) ->
    D64 = base64:decode(lists:nthtail(6, SshaHash)),
    {HashedData, Salt} = lists:split(20, binary_to_list(D64)),
    NewHash = crypto:hash(sha,list_to_binary(ClearPassword ++ Salt)),
    string:equal(binary_to_list(NewHash), HashedData).





