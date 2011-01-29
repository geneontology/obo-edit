#! /bin/sh

### BEGIN INIT INFO
# Provides:             mysql-proxy
# Required-Start:       $syslog
# Required-Stop:        $syslog
# Default-Start:        2 3 4 5
# Default-Stop:         0 1 6
# Short-Description:    MySQL Proxy
### END INIT INFO

PATH=/bin:/usr/bin:/sbin:/usr/sbin
LUA_PATH="/usr/share/mysql-proxy/?.lua"
#LUA_PATH="/srv/mysql-proxy/simple_proxy.lua"

DAEMON="/usr/sbin/mysql-proxy"
PIDFILE="/var/run/mysql-proxy.pid"

test -f $DAEMON || exit 0

test ! -r /etc/default/mysql-proxy || . /etc/default/mysql-proxy

if ! $ENABLED ; then exit 0 ; fi

. /lib/lsb/init-functions

case "$1" in
  start)
    log_begin_msg "Starting MySQL Proxy daemon..."
    start-stop-daemon --start --quiet --pidfile $PIDFILE --make-pidfile --name mysql-proxy --startas $DAEMON -b -- $OPTIONS
    log_end_msg $?
    ;;
  stop)
    log_begin_msg "Stopping MySQL Proxy daemon..."
    start-stop-daemon --stop --quiet --retry 3 --exec $DAEMON --pidfile $PIDFILE
    log_end_msg $?
    ;;
  status)
    log_begin_msg "Checking MySQL Proxy daemon... "
    status_of_proc -p $PIDFILE /usr/sbin/mysql-proxy mysql-proxy && exit 0 || exit $?
    ;;
  restart|force-reload)
    log_begin_msg "Reloading MySQL Proxy daemon..."
    start-stop-daemon --stop --quiet --retry 3 --exec $DAEMON --pidfile $PIDFILE
    start-stop-daemon --start --quiet --pidfile $PIDFILE --make-pidfile --name mysql-proxy --startas $DAEMON -b -- $OPTIONS
    log_end_msg $?
    ;;
  *)
    log_success_msg "Usage: $0 {start|stop|status|restart|force-reload}"
    exit 1
esac

exit 0
