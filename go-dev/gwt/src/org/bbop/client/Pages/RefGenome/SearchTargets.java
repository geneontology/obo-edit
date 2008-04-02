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

import com.google.gwt.user.client.ui.Hyperlink;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;

public class SearchTargets implements WebUIInterface {

	private WebSession session;
	private String defaultQuery;

    //
    final QueryButtonFetchReferenceTaxonIds b1;
    final QueryButtonFetchReferenceTargetIds b2;
    final QueryButtonGetTaxonIdPrefix b3;
    final QuerySetFetchIdsByName s1;
    final QuerySetFetchLabelsById s2;
    final QueryDoubleSetFetchIdsByNameAndTaxon ds1;
    
    //
    final TrivialStrings resultsTableStrings;
    final TrivialNodeDTOs resultsTableNodeDTO;
	
	//	
	public SearchTargets (WebSession webSession) {

		session = webSession;

	    resultsTableStrings = new TrivialStrings();
	    resultsTableNodeDTO = new TrivialNodeDTOs();
	    
		b1 = new QueryButtonFetchReferenceTaxonIds("Fetch Reference Taxon Ids", resultsTableStrings, session);
		b2 = new QueryButtonFetchReferenceTargetIds("Fetch Reference Target Ids", resultsTableStrings, session);
		b3 = new QueryButtonGetTaxonIdPrefix("Get Taxon Id Prefix", resultsTableStrings, session);
		s1 = new QuerySetFetchIdsByName("Fetch Ids By Name", resultsTableNodeDTO, session);
		s2 = new QuerySetFetchLabelsById("Fetch Labels By Id", resultsTableStrings, session);
		ds1 = new QueryDoubleSetFetchIdsByNameAndTaxon("Fetch Ids By Name And Taxon", resultsTableStrings);
	}

	
	// Construct the page.
	public Widget getWidget() {

		VerticalPanel vp = new VerticalPanel();


	    // A results table to catch searches.
	    resultsTableStrings.reset("No data yet.");
	    
	    vp.add(new PageTitle("Search Targets"));

		vp.add(b1);
		vp.add(b2);
		vp.add(b3);
		vp.add(s1);
		vp.add(s2);
		vp.add(ds1);
		
		vp.add(resultsTableStrings);
		vp.add(resultsTableNodeDTO);

		return vp;
	}
	
	//	
	public void updateWidget() {
		// Not really necessary yet...
	}
}
