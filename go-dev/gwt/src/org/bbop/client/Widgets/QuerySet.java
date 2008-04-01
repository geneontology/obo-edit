package org.bbop.client.Widgets;

import org.bbop.client.RefGenomeService;
import org.bbop.client.RefGenomeServiceAsync;
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
public class QuerySet extends VerticalPanel{

	final TextBox tb;
	final PushButton pb;
	final Trivial rt;
	final AsyncCallback async;
	
	//
	public QuerySet (String label, Trivial resultsTable){

		super();

		rt = resultsTable;
		
		tb = new TextBox();		
		tb.setVisibleLength(30);
		tb.setText("???");
		
		pb = new PushButton(label, "Looking...");

	    // When the user hits enter in the textbox submit data.
	    tb.addKeyboardListener(new KeyboardListenerAdapter() {
	    	public void onKeyPress(Widget sender, char keyCode, int modifiers) {
	    		// Check for enter
	    		if ((keyCode == KeyboardListener.KEY_ENTER) && (modifiers == 0)) {
	    			callbackAction(tb.getText());
	    		}
	    	}
	    });

	    // Setup the button to call the service
	    pb.addClickListener( new ClickListener() {
	    	public void onClick(Widget sender) {
	    		callbackAction(tb.getText());
	    	}
	    });

		
		this.add(tb);
		this.add(pb);

		//
		async = new AsyncCallback(){

			public void onSuccess(Object result) {

				rt.add(result);
			}

			public void onFailure(Throwable caught) {
				Window.alert("Server error (callbackAction): " + caught.toString());
			}
		};
	}
	
	//
	public void callbackAction(String str){

		Window.alert("Need to properly override callbackAction! (" + str + ")");
	}

}
