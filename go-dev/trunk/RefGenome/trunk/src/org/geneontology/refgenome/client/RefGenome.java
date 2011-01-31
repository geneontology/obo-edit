package org.geneontology.refgenome.client;



import org.geneontology.refgenome.client.model.GraphDTO;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.ClickListener;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.RadioButton;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;

/**
 * Entry point classes define <code>onModuleLoad()</code>.
 */
public class RefGenome implements EntryPoint {

	String moduleRelativeURL;
	RefGenomeServiceAsync srv = (RefGenomeServiceAsync) GWT.create(RefGenomeService.class);
	 private VerticalPanel vPanel = new VerticalPanel();

	/**
	 * This is the entry point method.
	 */
	public void onModuleLoad() {
		final Button button = new Button("Search ");
		final Label label = new Label();
		final TextBox tb = new TextBox();
		
		GraphDTO graph = new GraphDTO();

		ServiceDefTarget endpoint = (ServiceDefTarget) srv;
		moduleRelativeURL = GWT.getModuleBaseURL() + "email";
		endpoint.setServiceEntryPoint(moduleRelativeURL);

		// Use a module-relative URLs to ensure that this client code can find
		// its way home, even when the URL changes (as might happen when you
		// deploy this as a webapp under an external servlet container).
		moduleRelativeURL = GWT.getModuleBaseURL() + "refgenome";
		endpoint.setServiceEntryPoint(moduleRelativeURL );

		button.addClickListener(new ClickListener() {
			String msg = "Hello!!!";
			public void onClick(Widget sender) {
				System.err.println("clicked... "+srv);
				srv.fetchIdsByName(tb.getText(), new AsyncCallback() {
					public void onFailure(Throwable caught) {
						System.err.println("failed..."+caught);
						//
					}

					public void onSuccess(Object result) {
						System.err.println("success..."+result);

						String[] nids = (String[]) result;
						for (int i=0; i<nids.length; i++) {
							vPanel.add(new RadioButton("nid",nids[i]));
						}
						label.setText("x="+nids.toString());
					}

				});
				if (label.getText().equals(""))
					label.setText(msg);
				else
					label.setText("");
			}
		});

		// Assume that the host HTML has elements defined whose
		// IDs are "slot1", "slot2".  In a real app, you probably would not want
		// to hard-code IDs.  Instead, you could, for example, search for all 
		// elements with a particular CSS class and replace them with widgets.
		//
		RootPanel.get("slot1").add(tb);
		RootPanel.get("slot2").add(button);
		RootPanel.get("slot3").add(label);
		RootPanel.get("vp").add(vPanel);
	}
}
