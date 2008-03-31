package org.bbop.client.Widgets;

import org.bbop.client.RefGenomeService;
import org.bbop.client.RefGenomeServiceAsync;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.ServiceDefTarget;

	
//
public class QueryDoubleSetFetchIdsByNameAndTaxon extends QueryDoubleSet{

	//
	public QueryDoubleSetFetchIdsByNameAndTaxon (String label, TrivialResultsTable resultsTable){

		super(label, resultsTable);

	}
	
	//
	public void callbackAction(String str1, String str2){
		RefGenomeServiceAsync rgService = (RefGenomeServiceAsync) GWT.create(RefGenomeService.class);
		((ServiceDefTarget) rgService).setServiceEntryPoint( GWT.getModuleBaseURL() + "/RefGenomeService");
		rgService.fetchIdsByNameAndTaxon(str1, str2, async);
	}

}
