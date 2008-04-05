package org.bbop.client.Widgets;

import org.bbop.client.GOService;
import org.bbop.client.GOServiceAsync;
import org.bbop.client.RefGenomeService;
import org.bbop.client.RefGenomeServiceAsync;
import org.bbop.client.WebSession;
import org.bbop.client.Widgets.Results.TrivialGeneProducts;
import org.bbop.client.Widgets.Results.Trivial;
import org.bbop.client.Widgets.Results.TrivialNodeDTOs;

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
public class QuerySetGetGPsBySearch extends QuerySet{

	//
	public QuerySetGetGPsBySearch (String label, TrivialGeneProducts resultsTableGeneProduct, WebSession sess){

		super(label, resultsTableGeneProduct, sess);
		tb.setText("SOX");
	}
	
	//
	public void callbackAction(String str){
		GOServiceAsync goService = (GOServiceAsync) GWT.create(GOService.class);
		((ServiceDefTarget) goService).setServiceEntryPoint( GWT.getModuleBaseURL() + "/GOService");
		goService.getGPsBySearch(str, async);
	}

}
