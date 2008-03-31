package org.bbop.client;


public class WebSession {
		
	private String username = "";
	private String iface = "";
	private boolean authenticated_p = true;
	
	public boolean isAuthenticated () {
		return authenticated_p;
	}
}
