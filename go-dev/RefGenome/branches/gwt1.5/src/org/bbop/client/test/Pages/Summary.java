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

public class Summary implements WebUIInterface {
		
	private WebSession session = null;
	
	//	 Le Table.
    private FlexTable table = new FlexTable();

    // Create the object according to a session.
    public Summary (WebSession webSession) {

    	session = webSession;
    	
    	drawTable();
	}

	public Widget getWidget() {

		VerticalPanel vp = new VerticalPanel();
		vp.add(new PageTitle("Summary"));
		vp.add(table);
		return vp;
	}

	public void updateWidget() {
		drawTable();
	}
	
	private void drawTable (){
		
		// Populate table with data from somewhere.
    	table.setText(0, 0, "[Species]");
    	table.setText(0, 1, "[Homologs]");
    	table.setText(0, 2, "[! Homologs]");
    	table.setText(0, 3, "[Comprehensive]");
    	
    	table.setText(1, 0, "worm");
    	table.setText(1, 1, "200");
    	table.setText(1, 2, "30");
    	table.setText(1, 3, "189");
    	
    	table.setText(2, 0, "fly");
    	table.setText(2, 1, "185");
    	table.setText(2, 2, "19");
    	table.setText(2, 3, "160");

    	table.setText(3, 0, "human");
    	table.setText(3, 1, "274");
    	table.setText(3, 2, "2");
    	table.setText(3, 3, "100");
	}
}
