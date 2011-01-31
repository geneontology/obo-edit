package org.bbop.server;

import org.bbop.client.AmiGOLookupService;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;

public class AmiGOLookupServiceImpl
	extends RemoteServiceServlet implements AmiGOLookupService {
	
	private static AmiGODBConnect godb;
	private static final long serialVersionUID = 1L;

	//
	public AmiGOLookupServiceImpl() {

		super();

		godb = new AmiGODBConnect();
	}


	// 
	public String[] getGPs(String identifier, int limit, int offset) {
		return godb.getGPs(identifier, limit, offset);
	}
	
	// 	// 
	public String[] getGPInfo(String id) {
		return godb.getGPInfo(id);
	}
	
	public String[] getTermInfo(String acc) {
		return godb.getTermInfo(acc);
	}
	
	//	 
	public Integer getGPCount(String identifier) {
		return godb.getGPCount(identifier);
	}
	
	// 
	public Boolean isTerm(String identifier) {
		return godb.isTerm(identifier);
	}
	
	// 
	public String getSniff() {
		return godb.getSniff();
	}
}
