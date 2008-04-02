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

public class OrthoUp implements WebUIInterface {

	private WebSession session;
	
	//	
	public OrthoUp (WebSession webSession) {

		session = webSession;
		
	}

	
	// Construct the page.
	public Widget getWidget() {

		VerticalPanel vp = new VerticalPanel();

	    vp.add(new PageTitle("Upload Orthologs"));
	    vp.add(new HTML("TODO..."));	
	    
		return vp;
	}
	
	//	
	public void updateWidget() {
		// Not really necessary yet...
	}
}
