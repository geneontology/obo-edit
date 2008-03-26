package org.bbop.server;

import org.bbop.client.GOLookupService;

import com.google.gwt.user.server.rpc.RemoteServiceServlet;

/**
 * @author  sid
 */
public class GOLookupServiceImpl
	extends RemoteServiceServlet implements GOLookupService {
	
	private static GODBConnect godb = new GODBConnect();
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	//private static Random gen = Random();
	
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
}
