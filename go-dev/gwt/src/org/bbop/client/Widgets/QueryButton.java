package org.bbop.client.Widgets;

import org.bbop.client.RefGenomeService;
import org.bbop.client.RefGenomeServiceAsync;

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
public class QueryButton extends VerticalPanel{

	final PushButton pb;
	final TrivialResultsTable rt;
	final AsyncCallback async;
	
	//
	public QueryButton (String label, TrivialResultsTable resultsTable){

		super();

		rt = resultsTable;
		
		pb = new PushButton(label, "Looking...");

	    // Setup the button to call the service
	    pb.addClickListener( new ClickListener() {
	    	public void onClick(Widget sender) {
	    		callbackAction();
	    	}
	    });

		this.add(pb);

		//
		async = new AsyncCallback(){

			public void onSuccess(Object result) {

				String[] res = (String[]) result;

				if( res.length > 0 ){					
					rt.clear();
					for (int i = 0; i < res.length; i++) {
						rt.add(res[i]);
					}
				}else{
					Window.alert("Unknown query.");
				}
			}

			public void onFailure(Throwable caught) {
				Window.alert("Server error (callbackAction): " + caught.toString());
			}
		};
	}
	
	//
	public void callbackAction(){

		Window.alert("Need to properly override callbackAction!");
	}

}
