package org.bbop.client;


import org.bbop.client.Pages.RefGenome.General;
import org.bbop.client.Pages.RefGenome.InternalFooter;
import org.bbop.client.Pages.RefGenome.Login;
import org.bbop.client.Pages.RefGenome.Logout;
import org.bbop.client.Pages.RefGenome.OrthoUp;
import org.bbop.client.Pages.RefGenome.Reports;
import org.bbop.client.Pages.RefGenome.Salutation;
import org.bbop.client.Pages.RefGenome.Sandbox;
import org.bbop.client.Pages.RefGenome.SearchTargets;
import org.bbop.client.Pages.RefGenome.Summary;
import org.bbop.client.Pages.RefGenome.TargetUp;

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
	private SearchTargets search_targets;
	private Salutation salutation;
	private TargetUp target_up;
	private OrthoUp ortho_up;
	private InternalFooter internal_footer;
	private Reports reports;

	private Sandbox sandbox;	
	
	/**
	 * This is the entry point method.
	 */
	public void onModuleLoad() {

		session = new WebSession();
		
		login = new Login(session);
		logout = new Logout(session);
		general = new General(session);
		summary = new Summary(session);
		search_targets = new SearchTargets(session);
		salutation = new Salutation(session);
		target_up = new TargetUp(session);
		ortho_up = new OrthoUp(session);
		internal_footer = new InternalFooter(session);
		reports = new Reports(session);
		
		sandbox = new Sandbox(session);
		
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
		    root.add(internal_footer.getWidget());

		}else if( passToken.equalsIgnoreCase("search_targets") &&
				session.getAuthentication() ){

			root.add(salutation.getWidget());
		    root.add(search_targets.getWidget());
		    root.add(internal_footer.getWidget());

		}else if( passToken.equalsIgnoreCase("target_up") &&
				session.getAuthentication() ){

			root.add(salutation.getWidget());
		    root.add(target_up.getWidget());
		    root.add(internal_footer.getWidget());

		}else if( passToken.equalsIgnoreCase("ortho_up") &&
				session.getAuthentication() ){

			root.add(salutation.getWidget());
		    root.add(ortho_up.getWidget());
		    root.add(internal_footer.getWidget());
		    
		}else if( passToken.equalsIgnoreCase("reports") &&
				session.getAuthentication() ){

			root.add(salutation.getWidget());
		    root.add(reports.getWidget());
		    root.add(internal_footer.getWidget());

		}else if( passToken.equalsIgnoreCase("sandbox") &&
				session.getAuthentication() ){

			root.add(salutation.getWidget());
		    root.add(sandbox.getWidget());
		    root.add(internal_footer.getWidget());
		    
		}else{
			// Missed, so retry with login.
			History.newItem("login");
		}
	}
}
