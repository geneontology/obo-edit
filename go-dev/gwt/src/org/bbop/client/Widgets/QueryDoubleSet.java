package org.bbop.client.Widgets;

import org.bbop.client.Widgets.Results.Trivial;

import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.ui.ClickListener;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.KeyboardListener;
import com.google.gwt.user.client.ui.KeyboardListenerAdapter;
import com.google.gwt.user.client.ui.PushButton;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;

	
//
public class QueryDoubleSet extends VerticalPanel{

	final TextBox tb1;
	final TextBox tb2;
	final PushButton pb;
	final Trivial rt;
	final AsyncCallback async;
	
	//
	public QueryDoubleSet (String label, Trivial resultsTable){

		super();

		rt = resultsTable;
		
		tb1 = new TextBox();		
		tb1.setVisibleLength(30);
		tb1.setText("???");
		
		tb2 = new TextBox();		
		tb2.setVisibleLength(30);
		tb2.setText("???");
		
		pb = new PushButton(label, "Looking...");

	    // When the user hits enter in the textbox submit data.
	    tb1.addKeyboardListener(new KeyboardListenerAdapter() {
	    	public void onKeyPress(Widget sender, char keyCode, int modifiers) {
	    		// Check for enter
	    		if ((keyCode == KeyboardListener.KEY_ENTER) && (modifiers == 0)) {
	    			callbackAction(tb1.getText(), tb2.getText());
	    		}
	    	}
	    });

	    // When the user hits enter in the textbox submit data.
	    tb2.addKeyboardListener(new KeyboardListenerAdapter() {
	    	public void onKeyPress(Widget sender, char keyCode, int modifiers) {
	    		// Check for enter
	    		if ((keyCode == KeyboardListener.KEY_ENTER) && (modifiers == 0)) {
	    			callbackAction(tb1.getText(), tb2.getText());
	    		}
	    	}
	    });

	    // Setup the button to call the service
	    pb.addClickListener( new ClickListener() {
	    	public void onClick(Widget sender) {
	    		callbackAction(tb1.getText(), tb2.getText());
	    	}
	    });

		//
	    HorizontalPanel hp = new HorizontalPanel();
	    
		hp.add(tb1);
		hp.add(tb2);
		this.add(hp);
		this.add(pb);

		//
		async = new AsyncCallback(){

			public void onSuccess(Object result) {

				String[] res = (String[]) result;

				if( res.length > 0 ){					
					//Window.alert("Found query, \"" + res.length + "\".");
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
	public void callbackAction(String str1, String str2){

		Window.alert("Need to properly override callbackAction! (" + str1 + ", " + str2 + ")");
	}

}
