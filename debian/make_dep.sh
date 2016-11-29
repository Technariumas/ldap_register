#!/bin/bash
rm ldapregister_*.deb
rm -rf deb
rm -rf rel/ldapregister
set -e
rebar get-deps compile generate
mkdir -p deb/root/opt/ldapregister/ssl
mkdir -p deb/root/etc/systemd/system
mv rel/ldapregister/* deb/root/opt/ldapregister
cat ./debian/ldapregister.config > deb/root/opt/ldapregister/ldapregister.config
cat ./debian/ldapregister.service > deb/root/etc/systemd/system/ldapregister.service
chmod 640 deb/root/etc/systemd/system/ldapregister.service
fpm -s dir -t deb \
  -n "ldapregister" \
  --deb-user root \
  --deb-group root \
  --config-files /opt/ldapregister/ldapregister.config \
  --after-install debian/post_install.sh \
  -a all \
  -v 0.0.1 \
  -m "Ricardas Pocius <cirka@cirka.lt>" \
  --description "Website for registration to Technarium LDAP" \
  -C ./deb/root opt etc
rm -rf deb




