package org.bbop.client.Pages.RefGenome;

import org.bbop.client.WebSession;
import org.bbop.client.WebUIInterface;
import org.bbop.client.Widgets.PageTitle;
import org.bbop.client.Widgets.QueryButtonFetchReferenceTargetIds;
import org.bbop.client.Widgets.QueryButtonFetchReferenceTaxonIds;
import org.bbop.client.Widgets.QueryButtonGetTaxonIdPrefix;
import org.bbop.client.Widgets.QueryDoubleSetFetchIdsByNameAndTaxon;
import org.bbop.client.Widgets.QuerySetFetchIdsByName;
import org.bbop.client.Widgets.QuerySetFetchIdsByName;
import org.bbop.client.Widgets.QuerySetFetchLabelsById;
import org.bbop.client.Widgets.Results.TrivialNodeDTOs;
import org.bbop.client.Widgets.Results.Trivial;
import org.bbop.client.Widgets.Results.TrivialStrings;

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
