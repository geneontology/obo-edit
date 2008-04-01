package org.bbop.client.Widgets;

import org.bbop.client.RefGenomeService;
import org.bbop.client.RefGenomeServiceAsync;
import org.bbop.client.Widgets.Results.TrivialNodeDTOs;
import org.bbop.client.Widgets.Results.Trivial;

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
public class QuerySetFetchIdsByName extends QuerySet{

	//
	public QuerySetFetchIdsByName (String label, TrivialNodeDTOs resultsTableNodeDTO){

		super(label, resultsTableNodeDTO);

	}
	
	//
	public void callbackAction(String str){
		RefGenomeServiceAsync rgService = (RefGenomeServiceAsync) GWT.create(RefGenomeService.class);
		((ServiceDefTarget) rgService).setServiceEntryPoint( GWT.getModuleBaseURL() + "/RefGenomeService");
		rgService.fetchIdsByName(str, async);
	}

}
