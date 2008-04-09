package org.bbop.client;

import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.SimplePanel;


public class WebSession {
	
	public static final int ERROR = -1;
	public static final int BIG = 0;
	public static final int LITTLE = 1;
	
	private String username;
	private int capacity;
	private boolean authenticated_p ;
	
	private SimplePanel blockingPanel;
	private HTML bpText;
	private String defaultBPText;
	
    // Create the object according to a session.
    public WebSession () {

    	//
    	username = "UERROR";
    	capacity = ERROR;
    	authenticated_p = false;

    	//
    	defaultBPText = "Please be patient...";
    	bpText = new HTML(defaultBPText);
    	blockingPanel = new SimplePanel();
    	blockingPanel.add(bpText);
    	blockingPanel.setVisible(false);
    	blockingPanel.setStyleName("blocking-panel");
    }
	
	public boolean tryAuthentication (String uname, String pass, int iface) {

		boolean boo = false;

		if( uname.equals("foo") &&	pass.equals("bar") ){
			
			authenticated_p = true;
			username = "Foo";
			capacity = iface;	
			boo = true;
		}
		if( uname.equals("") &&	pass.equals("") ){
				
				authenticated_p = true;
				username = "Anonymous";
				capacity = iface;	
				boo = true;
		}
		
		return boo;
	}

	public void revokeAuthentication () {
		username = "UERROR";
		capacity = ERROR;	
		authenticated_p = false;
	}

	public boolean getAuthentication () {
		return authenticated_p;
	}

	public String getCapacityLabel (int cap) {

		String str;
		
		if( cap == LITTLE ){
			str = "little cheese";
		}else if( cap == BIG ){
			str = "Big Cheese";
		}else{
			str = "Unknown Capacity";
		}

		return str;
	}
	
	public String getUsername () {
		return username;
	}

	public int getCapacity () {
		return capacity;
	}

	public SimplePanel getBlocker () {
		return blockingPanel;
	}

	public void block () {
		bpText.setText(defaultBPText);
		blockingPanel.setVisible(true);
	}
	
	public void block (String str) {
		bpText.setText(str);
		blockingPanel.setVisible(true);
	}
	
	public void unblock () {
		blockingPanel.setVisible(false);
	}
	
}
