package org.bbop.server;

;

import org.bbop.client.RefGenomeService;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;

public class RefGenomeServiceServerImpl extends RemoteServiceServlet implements RefGenomeService {

	

	public Boolean checkUserPassword(String userId, String password) {
		// TODO Auto-generated method stub
		boolean logstatus = true;
		return new Boolean(true);
		
	}

	
}
