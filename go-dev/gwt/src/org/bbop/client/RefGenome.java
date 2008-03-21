package org.bbop.client;


import org.bbop.client.Pages.RefGenome.General;
import org.bbop.client.Pages.RefGenome.Login;
import org.bbop.client.Pages.RefGenome.Logout;
import org.bbop.client.Pages.RefGenome.Summary;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.user.client.History;
import com.google.gwt.user.client.HistoryListener;
import com.google.gwt.user.client.ui.RootPanel;

/**
 * Entry point classes define <code>onModuleLoad()</code>.
 */
public class RefGenome implements EntryPoint, HistoryListener {
				
	private WebSession session = new WebSession();
	
	private Login login = new Login(session);
	private Logout logout = new Logout(session);
	private General general = new General(session);
	private Summary summary = new Summary(session);

	/**
	 * This is the entry point method.
	 */
	public void onModuleLoad() {

	    //String initToken = History.getToken();
	    //if (initToken.length() == 0)
	    String initToken = "login";
	    //onHistoryChanged(initToken);

	    History.addHistoryListener(this);

	    History.newItem(initToken);
	}

	
	// We are going to use this like an application loop.
	public void onHistoryChanged(String passToken) {

		//Window.alert("\"" + passToken + "\"");
		// TODO: Nuke root window.
		RootPanel root = RootPanel.get();
		root.clear();
		
		if( passToken.equalsIgnoreCase("login") ){
			
			root.add(login.getWidget());
			root.add(summary.getWidget());

		}else if( passToken.equalsIgnoreCase("logout") ){
			
			//root.add(general.getWidget());
		    root.add(logout.getWidget());

		}else if( passToken.equalsIgnoreCase("general") ){

		    root.add(general.getWidget());
		    root.add(summary.getWidget());

		}else{
			// Missed, so retry with login.
			History.newItem("login");
		}
	}
}
