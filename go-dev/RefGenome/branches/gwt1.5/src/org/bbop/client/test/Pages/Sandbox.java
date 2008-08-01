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
import org.bbop.client.test.Widgets.QuerySetGetGPsBySearch;
import org.bbop.client.test.Widgets.Results.Trivial;
import org.bbop.client.test.Widgets.Results.TrivialGeneProducts;
import org.bbop.client.test.Widgets.Results.TrivialStrings;

import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.Hyperlink;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;

public class Sandbox implements WebUIInterface {

	private WebSession session;
	final QuerySetGetGPsBySearch s1;
    final TrivialGeneProducts resultsTableGP;
    
	//	
	public Sandbox (WebSession webSession) {

		session = webSession;
		resultsTableGP = new TrivialGeneProducts();	
		s1 = new QuerySetGetGPsBySearch("Get GPs By Search", resultsTableGP, session);
		
	}

	
	// Construct the page.
	public Widget getWidget() {

		VerticalPanel vp = new VerticalPanel();

	    vp.add(new PageTitle("Sandbox"));
	    vp.add(new HTML("Here is a playground..."));	

		vp.add(s1);
		vp.add(resultsTableGP);
	    
	    vp.add(new HTML("<br />"));		    
	    
		return vp;
	}
	
	//	
	public void updateWidget() {
		// Not really necessary yet...
	}
}
