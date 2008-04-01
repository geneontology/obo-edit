package org.bbop.client;


import org.bbop.client.Pages.RefGenome.General;
import org.bbop.client.Pages.RefGenome.Login;
import org.bbop.client.Pages.RefGenome.Logout;
import org.bbop.client.Pages.RefGenome.Salutation;
import org.bbop.client.Pages.RefGenome.Search;
import org.bbop.client.Pages.RefGenome.Summary;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.user.client.History;
import com.google.gwt.user.client.HistoryListener;
import com.google.gwt.user.client.ui.RootPanel;

/**
 * Entry point classes define <code>onModuleLoad()</code>.
 */
public class RefGenome implements EntryPoint, HistoryListener {
				
	private WebSession session;
	
	private Login login;
	private Logout logout;
	private General general;
	private Summary summary;
	private Search search;
	private Salutation salutation;
	
	/**
	 * This is the entry point method.
	 */
	public void onModuleLoad() {

		session = new WebSession();
		
		login = new Login(session);
		logout = new Logout(session);
		general = new General(session);
		summary = new Summary(session);
		search = new Search(session);
		salutation = new Salutation(session);
		
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
		root.add(session.getBlocker());
		
		if( passToken.equalsIgnoreCase("login") ){
			
			root.add(login.getWidget());
			root.add(summary.getWidget());

		}else if( passToken.equalsIgnoreCase("logout") &&
				session.getAuthentication() ){
			
			//root.add(general.getWidget());
		    root.add(logout.getWidget());

		}else if( passToken.equalsIgnoreCase("general") &&
				session.getAuthentication() ){

		    root.add(salutation.getWidget());
		    root.add(general.getWidget());
		    root.add(summary.getWidget());

		}else if( passToken.equalsIgnoreCase("search") &&
				session.getAuthentication() ){

			root.add(salutation.getWidget());
		    root.add(search.getWidget());

		}else{
			// Missed, so retry with login.
			History.newItem("login");
		}
	}
}
