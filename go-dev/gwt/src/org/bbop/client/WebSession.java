package org.bbop.client;

import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.SimplePanel;


public class WebSession {
		
	private String username;
	private String capacity;
	private boolean authenticated_p ;
	
	private SimplePanel blockingPanel;
	
    // Create the object according to a session.
    public WebSession () {

    	//
    	username = "UERROR";
    	capacity = "CERROR";
    	authenticated_p = false;

    	//
    	blockingPanel = new SimplePanel();
    	blockingPanel.add(new HTML("Please be patient..."));
    	blockingPanel.setVisible(false);
    	blockingPanel.setStyleName("blocking-panel");
    }
	
	public boolean tryAuthentication (String uname, String pass, String cap) {

		boolean boo = false;

		if( uname.equals("foo") &&	pass.equals("bar") ){
			
			authenticated_p = true;
			username = "Foo";
			capacity = cap;	
			boo = true;
		}
		if( uname.equals("") &&	pass.equals("") ){
				
				authenticated_p = true;
				username = "Anonymous";
				capacity = cap;	
				boo = true;
		}
		
		return boo;
	}

	public void revokeAuthentication () {
		username = "UERROR";
		capacity = "CERROR";	
		authenticated_p = false;
	}

	public boolean getAuthentication () {
		return authenticated_p;
	}

	public String getUsername () {
		return username;
	}

	public String getCapacity () {
		return capacity;
	}


	public SimplePanel getBlocker () {
		return blockingPanel;
	}

	public void block () {
		blockingPanel.setVisible(true);
	}
	
	public void unblock () {
		blockingPanel.setVisible(false);
	}
	
}
