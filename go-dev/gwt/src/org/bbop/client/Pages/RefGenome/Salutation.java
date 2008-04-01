package org.bbop.client.Pages.RefGenome;


import org.bbop.client.WebSession;
import org.bbop.client.WebUIInterface;
import org.bbop.client.Widgets.PageTitle;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.Command;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.google.gwt.user.client.ui.ClickListener;
import com.google.gwt.user.client.ui.FlexTable;
import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.KeyboardListener;
import com.google.gwt.user.client.ui.KeyboardListenerAdapter;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.MenuBar;
import com.google.gwt.user.client.ui.MenuItem;
import com.google.gwt.user.client.ui.PushButton;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.SourcesTabEvents;
import com.google.gwt.user.client.ui.SourcesTableEvents;
import com.google.gwt.user.client.ui.TabListener;
import com.google.gwt.user.client.ui.TabPanel;
import com.google.gwt.user.client.ui.TableListener;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;

public class Salutation implements WebUIInterface {
		
	private WebSession session = null;
	
	//	 Le Table.
	//private FlexTable table = new FlexTable();

    // Create the object according to a session.
    public Salutation (WebSession webSession) {

    	session = webSession;
    	
	}

	public Widget getWidget() {

		VerticalPanel vp = new VerticalPanel();
		//vp.add(new PageTitle("Logout"));
		vp.add(new HTML("Welcome, " + session.getUsername() +
				". You are logged in as a " + session.getCapacity() + "."));	
		return vp;
	}

	public void updateWidget() {
		//
	}
	
}
