package org.bbop.client.Pages.RefGenome;

import org.bbop.client.RefGenomeService;
import org.bbop.client.RefGenomeServiceAsync;
import org.bbop.client.WebSession;
import org.bbop.client.WebUIInterface;
import org.bbop.client.Widgets.FreeInputBox;
import org.bbop.client.Widgets.PageTitle;
import org.bbop.client.Widgets.QueryButtonFetchReferenceTargetIds;
import org.bbop.client.Widgets.QueryButtonFetchReferenceTaxonIds;
import org.bbop.client.Widgets.QuerySet;
import org.bbop.client.Widgets.QuerySetFetchIdsByName;
import org.bbop.client.Widgets.StringArrayDump;
import org.bbop.client.Widgets.QueryButton;

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

public class Search implements WebUIInterface {

	private WebSession session;
	private String defaultQuery;

    //
    final QueryButtonFetchReferenceTaxonIds b1;
    final QueryButtonFetchReferenceTargetIds b2;
    final QuerySetFetchIdsByName t1;
    
    //
    final StringArrayDump resultsTable;
	
	//	
	public Search (WebSession webSession) {

		session = webSession;

	    resultsTable = new StringArrayDump();
	    
		b1 = new QueryButtonFetchReferenceTaxonIds("referenceTaxonIds", resultsTable);
		b2 = new QueryButtonFetchReferenceTargetIds("referenceTargetIds", resultsTable);
		t1 = new QuerySetFetchIdsByName("idsByName", resultsTable);
	}

	
	// Construct the page.
	public Widget getWidget() {

		VerticalPanel vp = new VerticalPanel();


	    // A results table to catch searches.
	    resultsTable.reset("No data yet.");
	    
	    vp.add(new PageTitle("Search"));

		vp.add(b1);
		vp.add(b2);
		vp.add(t1);
		
		vp.add(resultsTable);
		vp.add(new Hyperlink("General", "general"));
		vp.add(new Hyperlink("Logout", "logout"));

		return vp;
	}
	
	//	
	public void updateWidget() {
		// Not really necessary yet...
	}
}
