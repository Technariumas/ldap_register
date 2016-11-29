if ! getent passwd ldapregister > /dev/null; then
    adduser --system --quiet \
        --home /opt/ldapregister  --no-create-home \
        --shell /bin/bash --group --gecos "LDAP Registration system" ldapregister
fi

mkdir -p /opt/ldapregister/ssl

# These should not be world readable
chmod 0750 /opt/ldapregister

# And should be owned by the ldapregister user and group
chown -R ldapregister:ldapregister /opt/ldapregister


