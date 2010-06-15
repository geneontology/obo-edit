-- simple-proxy.lua 
-- If it's a query, rather some info about it and write it to the log.
-- Usage:
--    on proxy machine: mysql-proxy --proxy-lua-script=simple_proxy.lua --proxy-address=:4404 --proxy-backend-addresses=realmysqlhost:3306
--    on client machine: mysql -h fakemysqlhost -P 4404 -u user -ppassword go_latest -e 'select count(*) from term'

local log_file = '/local/mysql-proxy/all.log'
local fh = io.open(log_file, "a+")

function read_query(packet)
  if string.byte(packet) == proxy.COM_QUERY then

      local date_str = os.date('%Y%m%d %H:%M:%S')
      local client_addr =  proxy.connection.client.address
      local client_thread = proxy.connection.server.thread_id
      local query_str =  string.sub(packet, 2)
      local backend_addr = proxy.backends[proxy.connection.backend_ndx].address

      fh:write( string.format("%s %s %s\n", date_str, client_addr, query_str) )
      fh:flush()
  end
end
