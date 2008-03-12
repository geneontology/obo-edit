package org.obd.ws.coreResource;

import java.sql.SQLException;
import java.util.Collection;
import java.util.LinkedList;

import org.obd.model.Graph;
import org.obd.parser.Parser;
import org.obd.query.Shard;
import org.obd.query.impl.MultiShard;
import org.obd.query.impl.OBDSQLShard;
import org.restlet.Component;
import org.restlet.data.Protocol;

public class OBDMain {

	static String defaultJdbcPath = "jdbc:postgresql://localhost:5432/obd_phenotype_mouse";
	protected Shard shard;

    public static void main(String[] args) throws Exception {
        // Create a component
        Component component = new Component();
        component.getServers().add(Protocol.HTTP, 8182);
        component.getClients().add(Protocol.FILE);
        String fmt = "homologene";

		String jdbcPath = defaultJdbcPath;

		for (int i = 0; i < args.length; i++)
			System.err.println("args[" + i + "] = |" + args[i] + "|");

		Collection<String> files = new LinkedList<String>();
		for (int i = 0; i < args.length; i++) {

			if (args[i].equals("-d")) {
				i++;
				jdbcPath = args[i];
			}
			else if (args[i].equals("--format")) {
				i++;
				fmt = args[i];
			}
			else {
				files.add(args[i]);
			}
		}
		      
        // TODO: configurable
        // for now we hardcode a multishard wrapping an obosession
        // and a SQL shard
        MultiShard multiShard = new MultiShard();
		try {
			
			OBDSQLShard obd = new OBDSQLShard();
			
			// Add username and password here....
			obd.connect(jdbcPath,null,null);
			multiShard.addShard(obd);
		} catch (SQLException e) {
			e.printStackTrace();
		}
		
		for (String file : files) {
			Parser p = Parser.createParser(fmt, file);
			p.parse();
			multiShard.putGraph(p.getGraph());
		}

    }
    
    public void storeGraph(Graph g) {
    	shard.putGraph(g);
    }

}
