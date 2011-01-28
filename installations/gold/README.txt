Gold services on ladle are run as upstart jobs. Because the gold app
is not a true daemon in the unix sense (e.g. it doesn't fork away and
handle STDIN/STDOUT), the upstart job acts more as a wrapper under the
hood.

For the purposes of startup and shutdown, the gold upstart job waits
for the postgresql daemon to start and stop (which requires a small
modification to the postgresql /etc/init.d file adding upstart
events--see this directory). Restarting and reloading postgresql will
also have the effect of stopping and starting gold.

To handle gold independently of postgresql, the following command will
start the gold psuedo-daemon and emit the "gold_start" upstart event:

   swdev@ladle:~$ sudo start gold

Next, to stop it and emit the "gold_stop" upstart event:

   swdev@ladle:~$ sudo stop gold
