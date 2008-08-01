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
public class QueryButton extends VerticalPanel{

	final PushButton pb;
	final Trivial rt;
	final AsyncCallback async;
	final WebSession session;
	
	//
	public QueryButton (String label, Trivial resultsTable, WebSession wsession){

		super();

		rt = resultsTable;
		
		session = wsession;
		
		pb = new PushButton(label, "Looking...");

	    // Setup the button to call the service
	    pb.addClickListener( new ClickListener() {
	    	public void onClick(Widget sender) {
	    		session.block();
	    		callbackAction();
	    	}
	    });

		this.add(pb);

		//
		async = new AsyncCallback(){

			public void onSuccess(Object result) {

						rt.add(result);
			    		session.unblock();
			}

			public void onFailure(Throwable caught) {
	    		session.unblock();
				Window.alert("Server error (callbackAction): " + caught.toString());
			}
		};
	}
	
	//
	public void callbackAction(){

		Window.alert("Need to properly override callbackAction!");
	}

}
