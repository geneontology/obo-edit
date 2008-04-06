package org.bbop.server;



import org.bbop.client.RefGenomeService;
import org.bbop.client.model.NodeDTO;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;

public class RefGenomeServerImpl extends RemoteServiceServlet implements RefGenomeService {

	

	public Boolean checkUserPassword(String userId, String password) {
		// TODO Auto-generated method stub
		boolean logstatus = true;
		return new Boolean(logstatus);
		
	}
	public String[] fetchReferenceTargetIds() {
		return new String[6];
	}
	public NodeDTO[] fetchIdsByName(String searchTerm) {
		// TODO Auto-generated method stub
		return null;
	}
	
	
}
