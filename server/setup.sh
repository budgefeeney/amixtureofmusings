#!/bin/bash

# This is the server configuration script for the amixtureofmusings website.
# Given a totally new server, it eliminates password access, sets up a firewall,
# sets up an nginx webserver, configures mail forwarding, and sets up
# auto-updates.
#
# The setup presumes the server is fairly starved of resources, and so is pretty
# light, e.g. a single CPU worker for Nginx, no access-log and no SSL.
#
# The following pre-requisites need to exist
#
# 1. You're logged in as root
# 2. You have already set up an authorized_keys file to log in as root without
#    a password (see https://www.linode.com/docs/security/securing-your-server)
#
# This script needs to be run as root, and is interactive, so you need to keep
# an eye on it. Once it's done you'll need to double-check that you can log
# in as the non-root user with full sudo privileges, and then if so commit to
# the hardened SSH access by restarting the SSH daemon. A typical run looks like
#
# local> scp ~/.ssh/id_rsa.pub root@host:~/.ssh/authorized_keys
# local> scp server/setup.sh root@server:/root/setup.sh
# local> ssh root@server
#
# server> USER=...
# server> adduser $USER
# server> chmod +x setup.sh
# server> ./setup.sh
# server> systemctl restart sshd
#


#
# Configuration
#

# User
USER=bfeeney
SSH_PUB_AUTHKEY="/root/.ssh/authorized_keys"

# Network
SITE_NAME=amixtureofmusings
DOMAIN="$SITE_NAME.com"
HOSTNAME="www"



#
# Helpful functions
#
function warn () {
    echo "$0:" "$@" >&2
}

function die_with_error () {
    local return_code=$1
    shift
    warn "$@"
    exit $return_code
}

function die() {
  dieWithError 127 "$@"
}

function backup() {
  local day_stamp=`date +%Y-%m-%d`
  local suffix="-backup-$DAYSTAMP"
  local old_name=$1
  local new_name=$1$suffix

  cp -v $old_name $new_name
}

function set_config(){
    local cfg_file=$1
    local key=$2
    local new_val=$3

    echo "Update $cfg_file:$key = $new_val"
    # echo "sed -r -i s/^(\s*)#?( *$key) *(=| ).*/\1$key\3$new_val/ $cfg_file"
    sed -r -i "s/^(\s*)#?( *$key) *(=| ).*/\1$key\3$new_val/" $cfg_file
}


#
# Update the operating system to the latest version
#
apt-get update && apt-get upgrade -y


#
# Set up the non-root user, disable the root user, harden logins
#
USER_HOME=`getent passwd $USER | cut -f6 -d:`
test -f $SSH_PUB_KEY || die "No public key located in $SSH_PUB_KEY"
test -z $USER_HOME && die "User $USER does not exist"

echo "Enabling passwordless SSH access for $USER"
mkdir $USER_HOME/.ssh
cp $HOME/.ssh/authorized_keys $USER_HOME/.ssh
chown -R $USER.$USER $USER_HOME/.ssh
chmod 700 $USER_HOME/.ssh


echo "Creating sudoers config file for $USER"
adduser $USER sudo
echo "$USER    ALL=(ALL:ALL) ALL" > /etc/sudoers.d/$USER
chmod 440 /etc/sudoers.d/$USER


echo "Disabling password-based access and root-over-SSH"
SSH_CFG_FILE=/etc/ssh/sshd_config
test -f $SSH_CFG_FILE || die "No SSH config file at $SSH_CFG_FILE"
backup $SSH_CFG_FILE
set_config $SSH_CFG_FILE PasswordAuthentication no
set_config $SSH_CFG_FILE PermitRootLogin no

echo "Disabling SSH overIPv6"
echo 'AddressFamily inet' | tee -a $SSH_CFG_FILE

#systemctl restart sshd

echo "Installing Fail2Ban for SSH"
apt-get install -y fail2ban sendmail-bin sendmail

cat > /etc/fail2ban/fail2ban.local <<EOF
# Force logging to warning only, to avoid spamming the disk
loglevel = 2

EOF

systemctl restart fail2ban


#
# Set up the Firewall
#

cat > /tmp/ipv4 <<EOF
*filter

# Allow all loopback (lo0) traffic and reject traffic
# to localhost that does not originate from lo0.
-A INPUT -i lo -j ACCEPT
-A INPUT ! -i lo -s 127.0.0.0/8 -j REJECT

# Allow ping.
-A INPUT -p icmp -m state --state NEW --icmp-type 8 -j ACCEPT

# Allow SSH connections.
-A INPUT -p tcp --dport 22 -m state --state NEW -j ACCEPT

# Allow HTTP and HTTPS connections from anywhere
# (the normal ports for web servers).
-A INPUT -p tcp --dport 80 -m state --state NEW -j ACCEPT
-A INPUT -p tcp --dport 443 -m state --state NEW -j ACCEPT

# Allow inbound traffic from established connections.
# This includes ICMP error returns.
-A INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT

# Log what was incoming but denied (optional but useful).
-A INPUT -m limit --limit 5/min -j LOG --log-prefix "iptables_INPUT_denied: " --log-level 7

# Reject all other inbound.
-A INPUT -j REJECT

# Log any traffic that was sent to you
# for forwarding (optional but useful).
-A FORWARD -m limit --limit 5/min -j LOG --log-prefix "iptables_FORWARD_denied: " --log-level 7

# Reject all traffic forwarding.
-A FORWARD -j REJECT

COMMIT

EOF

cat >/tmp/ipv6 <<EOF
*filter

# Allow all loopback (lo0) traffic and reject traffic
# to localhost that does not originate from lo0.
-A INPUT -i lo -j ACCEPT
-A INPUT ! -i lo -s ::1/128 -j REJECT

# Allow ICMP
-A INPUT -p icmpv6 -j ACCEPT

# Allow HTTP and HTTPS connections from anywhere
# (the normal ports for web servers).
-A INPUT -p tcp --dport 80 -m state --state NEW -j ACCEPT
-A INPUT -p tcp --dport 443 -m state --state NEW -j ACCEPT

# Allow inbound traffic from established connections.
-A INPUT -m state --state ESTABLISHED,RELATED -j ACCEPT

# Log what was incoming but denied (optional but useful).
-A INPUT -m limit --limit 5/min -j LOG --log-prefix "ip6tables_INPUT_denied: " --log-level 7

# Reject all other inbound.
-A INPUT -j REJECT

# Log any traffic that was sent to you
# for forwarding (optional but useful).
-A FORWARD -m limit --limit 5/min -j LOG --log-prefix "ip6tables_FORWARD_denied: " --log-level 7

# Reject all traffic forwarding.
-A FORWARD -j REJECT

COMMIT

EOF

echo "Activating the firewall rules"
iptables-restore < /tmp/ipv4
ip6tables-restore < /tmp/ipv6

rm /tmp/ipv4 /tmp/ipv6

echo "Installing IPTables-Persistent to retain the rules on reboot"
apt-get install -y iptables-persistent


#
# Intrustion detection
#
# https://linode.com/docs/security/ossec-ids-debian-7


#
# Set up the web-server
#

# Most of this is taken from advice here:
# https://www.linode.com/docs/websites/nginx/configure-nginx-for-optimized-performance

# We use nginx-full to have access to connection & bandwidth limiting code
apt-get install -y nginx-full


# Ensure log-rotation is enabled
test -f /etc/logrotate.d/nginx  || die "Log rotation is not enabled for nginx"

# Amend the server configuration to disable access logging and reduce workers
NGINX_CFG=/etc/nginx/nginx.conf
backup $NGINX_CFG
set_config $NGINX_CFG worker_processes "auto;" # note nginx requires semi-colons in its config
set_config $NGINX_CFG access_log "off;"


# The site is basically the default site with a specific server name, and a
# slightly different path, to work around Hakyll / rsync weirdness
adduser $USER www-data
mkdir /var/www/html/_site
chown $USER.www-data  /var/www/html/_site

SITES_DIR="/etc/nginx/sites-available/"
NEW_SITE="$SITES_DIR/$SITE_NAME"

cat >$NEW_SITE <<EOF
server {
  listen 80 default_server;
  listen [::]:80 default_server;

  # SSL is disabled for performance reasons

  root /var/www/html/_site;

  # Add index.php to the list if you are using PHP
  index index.html index.htm;

  server_name _;

  location / {
    # First attempt to serve request as file, then
    # as directory, then fall back to displaying a 404.
    try_files \$uri \$uri/ =404;
  }

  # deny access to stray .htaccess files, if Apache's document root
  location ~ /\.ht {
    deny all;
  }

  # Have browsers aggressively cache static assets like images and fonts
  location ~*.(woff|eot|ttf|svg|mp4|webm|jpg|jpeg|png|gif|ico|css|js)$ {
    expires 365d;
  }
}

EOF

rm /etc/nginx/sites-enabled/default
ln -s $NEW_SITE /etc/nginx/sites-enabled/

# There is a lot more handcrafted config that can be done
# https://www.nginx.com/resources/admin-guide/logging-and-monitoring/

# In particular, there's nothing here about throttling users.


systemctl restart nginx


#
# Mail forwarding
#

# Note we already installed sendmail for fail2ban




#
# Final steps
#
echo "For the SSH hardening to take effect you need to restart the SSH daemon"
echo "> systemctl restart sshd"
echo ""
