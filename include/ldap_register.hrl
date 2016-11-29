
-type ldapregister_progress()  ::  added | activated.

-record(ldap_member,{
    ticket_id :: list(),
    progress :: ldapregister_progress(),
    name :: list(),
    surname :: list(),
    uid = "undefined":: list(),
    password = "undefined" :: list(),
    email :: list(),
    mobile :: list(),
    synced = false :: atom()
    }).



-define(BASEURL,"https://ldap.technarium.lt").
-define(BASEPATH,"/register/").
