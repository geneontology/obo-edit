package org.bbop.client.test.Pages;

import org.bbop.client.test.Components.WebSession;
import org.bbop.client.test.Components.WebUIInterface;
import org.bbop.client.test.Widgets.PageTitle;
import org.bbop.client.test.Widgets.QueryButtonFetchReferenceTargetIds;
import org.bbop.client.test.Widgets.QueryButtonFetchReferenceTaxonIds;
import org.bbop.client.test.Widgets.QueryButtonGetTaxonIdPrefix;
import org.bbop.client.test.Widgets.QueryDoubleSetFetchIdsByNameAndTaxon;
import org.bbop.client.test.Widgets.QuerySetFetchIdsByName;
import org.bbop.client.test.Widgets.QuerySetFetchLabelsById;
import org.bbop.client.test.Widgets.Results.Trivial;
import org.bbop.client.test.Widgets.Results.TrivialGeneProducts;
import org.bbop.client.test.Widgets.Results.TrivialStrings;

import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.Hyperlink;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;

public class TargetUp implements WebUIInterface {

	private WebSession session;
	
	//	
	public TargetUp (WebSession webSession) {

		session = webSession;

	}

	
	// Construct the page.
	public Widget getWidget() {

		VerticalPanel vp = new VerticalPanel();

	    vp.add(new PageTitle("Upload New Targets"));

	    vp.add(new HTML("Targets completion date: [.....]"));	
	    vp.add(new HTML("Targets TAB file: [.....] (Browse)"));
	    vp.add(new HTML("(Upload)"));

		return vp;
	}
	
	//	
	public void updateWidget() {
		// Not really necessary yet...
	}
}
