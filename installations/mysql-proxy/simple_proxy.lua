-- simple-proxy.lua 
-- If it's a query, rather some info about it and write it to the log.
-- Usage:
--    on proxy machine: mysql-proxy --proxy-lua-script=simple_proxy.lua --proxy-address=:4404 --proxy-backend-addresses=realmysqlhost:3306
--    on client machine: mysql -h fakemysqlhost -P 4404 -u user -ppassword go_latest -e 'select count(*) from term'

local log_file = '/srv/mysql-proxy/all.log'
local fh = io.open(log_file, "a+")
local okay_users = {go_select=true,paint=true}

-- Give a little information about traffic.
function read_query(packet)
   if string.byte(packet) == proxy.COM_QUERY then

      local date_str = os.date('%Y%m%d %H:%M:%S')
      local client_addr =  proxy.connection.client.src.name
      local client_thread = proxy.connection.server.thread_id
      local query_str =  string.sub(packet, 2)
      local backend_addr = proxy.global.backends[proxy.connection.backend_ndx].address

      -- Flush log line.
      fh:write( string.format("%s %s %s\n", date_str, client_addr, query_str) )
      fh:flush()
   end
end


-- Force the user to be who we say.
function read_auth()

   in_user = proxy.connection.client.username
   if not okay_users[in_user] then
      --fh:write(string.format("\tpass: %s\n", proxy.connection.client.username))
      --fh:write(string.format("\tpass: %q\n", proxy.connection.client.scrambled_password))
      --fh:flush()
      proxy.response.type = proxy.MYSQLD_PACKET_ERR
      proxy.response.errmsg = "Logins are not allowed for this user: " .. in_user
      return proxy.PROXY_SEND_RESULT
   end
end
