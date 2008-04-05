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
import org.bbop.client.Widgets.QuerySetGetGPsBySearch;
import org.bbop.client.Widgets.Results.TrivialGeneProducts;
import org.bbop.client.Widgets.Results.Trivial;
import org.bbop.client.Widgets.Results.TrivialStrings;

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
