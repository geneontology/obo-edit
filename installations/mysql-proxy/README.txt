Enabling the PAINT proxy on a fresh Ubuntu 10.04 machine.

Luckily, mysql-proxy now comes with an init.d file installed in the
right place. The only thing that needs to be added are:

*) the lua proxy definition, put into /srv/mysql-proxy
*) a permissive log file at /srv/mysql-proxy/all.log
*) a good defaults options file, put into /etc/defaults/mysql-proxy

TODO: log rotation
