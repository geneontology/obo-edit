#!/bin/sh -e

### BEGIN INIT INFO
# Provides:		postgresql postgresql-8.4
# Required-Start:	$local_fs $remote_fs $network $time
# Required-Stop:	$local_fs $remote_fs $network $time
# Should-Start:		$syslog
# Should-Stop:		$syslog
# Default-Start:	2 3 4 5
# Default-Stop:		0 1 6
# Short-Description:	PostgreSQL 8.4 RDBMS server
### END INIT INFO

# Setting environment variables for the postmaster here does not work; please
# set them in /etc/postgresql/8.4/<cluster>/environment instead.

[ -r /usr/share/postgresql-common/init.d-functions ] || exit 0

. /usr/share/postgresql-common/init.d-functions

VERSION=8.4

case "$1" in
    start)
        start $VERSION
        initctl emit postgres_start
        ;;
    stop)
        stop "$VERSION"
        initctl emit postgres_stop
        ;;
    restart)
        initctl emit postgres_stop
	restart "$VERSION"
        initctl emit postgres_start
        ;;
    force-reload | reload)
        initctl emit postgres_stop
        reload $VERSION
        initctl emit postgres_start
        ;;
    status)
        status $VERSION
	;;
    *)
        echo "Usage: $0 {start|stop|restart|reload|force-reload|status|autovac-start|autovac-stop|autovac-restart}"
        exit 1
        ;;
esac

exit 0
