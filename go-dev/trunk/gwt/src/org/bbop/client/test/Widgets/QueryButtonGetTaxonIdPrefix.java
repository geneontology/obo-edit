package org.bbop.client.test.Widgets;

import org.bbop.client.RefGenomeService;
import org.bbop.client.RefGenomeServiceAsync;
import org.bbop.client.test.Components.WebSession;
import org.bbop.client.test.Widgets.Results.Trivial;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.google.gwt.user.client.ui.ClickListener;
import com.google.gwt.user.client.ui.KeyboardListener;
import com.google.gwt.user.client.ui.KeyboardListenerAdapter;
import com.google.gwt.user.client.ui.PushButton;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;

	
//
public class QueryButtonGetTaxonIdPrefix extends QueryButton{


	public QueryButtonGetTaxonIdPrefix (String label, Trivial resultsTable, WebSession session){
		super(label, resultsTable, session);
	}
	
	//
	public void callbackAction(){

		RefGenomeServiceAsync rgService = (RefGenomeServiceAsync) GWT.create(RefGenomeService.class);
		((ServiceDefTarget) rgService).setServiceEntryPoint( GWT.getModuleBaseURL() + "/RefGenomeService");
		rgService.getTaxonIdPrefix(async);
	}
		
}
