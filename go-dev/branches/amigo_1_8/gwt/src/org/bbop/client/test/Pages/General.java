package org.bbop.client.test.Pages;


import org.bbop.client.test.Components.WebSession;
import org.bbop.client.test.Components.WebUIInterface;
import org.bbop.client.test.Widgets.PageTitle;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.Command;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.google.gwt.user.client.ui.ClickListener;
import com.google.gwt.user.client.ui.FlexTable;
import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.Hyperlink;
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

public class General implements WebUIInterface {

	private WebSession session = null;
	
	public General (WebSession webSession) {

		session = webSession;
		
	}

	public Widget getWidget() {

		VerticalPanel vp = new VerticalPanel();

		vp.add(new PageTitle("General"));

		if( session.getCapacity() == WebSession.BIG ){

			vp.add(new HTML("You may add targets here:"));
			vp.add(new Hyperlink("Upload New Targets", "target_up"));
			vp.add(new HTML("<br />"));
			
			vp.add(new HTML("You may also search current targets:"));
			vp.add(new Hyperlink("Search Targets", "search_targets"));
			vp.add(new HTML("<br />"));
			
			vp.add(new HTML("Go to the GO sandbox:"));
			vp.add(new Hyperlink("Use GO interface", "sandbox"));
			vp.add(new HTML("<br />"));
			
			vp.add(new HTML("View reports about various aspects:"));
			vp.add(new Hyperlink("View Reports", "reports"));
			vp.add(new HTML("<br />"));
			
		}else{
			vp.add(new HTML("You may upload orthologs here:"));
			vp.add(new Hyperlink("Upload Orthologs", "ortho_up"));
		}

		return vp;
	}


	//	
	public void updateWidget() {
		// Not really necessary yet...
	}
}
