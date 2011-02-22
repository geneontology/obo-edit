package org.bbop.client.test.Pages;

import org.bbop.client.test.Components.WebSession;
import org.bbop.client.test.Components.WebUIInterface;
import org.bbop.client.test.Widgets.PageTitle;

import com.google.gwt.user.client.History;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.Hyperlink;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;

//Something to take care of login with session.
public class Logout implements WebUIInterface {

	private WebSession session = null;
	private Hyperlink loginLink = new Hyperlink("Login", "login");
	
	public Logout (WebSession webSession) {

		session = webSession;
		
	}


	//
	private boolean doLogout(){
		// Close out session.
		session.revokeAuthentication();
		return true;
	}
	
	
	// Assemble the widget.
	public Widget getWidget() {

		if( doLogout() ){
			//
		}else{
			Window.alert("Logout failed!");
		}
		
		VerticalPanel vp = new VerticalPanel();
		vp.add(new PageTitle("Logout"));
		vp.add(new HTML("You have successfully logged out."));
		vp.add(loginLink);
		
		return vp;
	}


	public void updateWidget() {
		// No real need to update.
	}
}
