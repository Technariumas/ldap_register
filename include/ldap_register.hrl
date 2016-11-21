
-record(ldap_member,{
    ticket_id :: list(),
    progress = undefined :: atom(),
    name :: list(),
    surname :: list(),
    uid = "undefined":: list(),
    password = "undefined" :: list(),
    email :: list(),
    mobile :: list() 
    }).
