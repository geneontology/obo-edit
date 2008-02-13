package org.bbop.client.Pages.RefGenome;


import org.bbop.client.WebSession;
import org.bbop.client.WebUIInterface;
import org.bbop.client.Widgets.PageTitle;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.Command;
import com.google.gwt.user.client.History;
import com.google.gwt.user.client.Window;
import com.google.gwt.user.client.rpc.AsyncCallback;
import com.google.gwt.user.client.rpc.ServiceDefTarget;
import com.google.gwt.user.client.ui.ClickListener;
import com.google.gwt.user.client.ui.FlexTable;
import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.KeyboardListener;
import com.google.gwt.user.client.ui.KeyboardListenerAdapter;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.MenuBar;
import com.google.gwt.user.client.ui.MenuItem;
import com.google.gwt.user.client.ui.PasswordTextBox;
import com.google.gwt.user.client.ui.PushButton;
import com.google.gwt.user.client.ui.RadioButton;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.SourcesTabEvents;
import com.google.gwt.user.client.ui.SourcesTableEvents;
import com.google.gwt.user.client.ui.TabListener;
import com.google.gwt.user.client.ui.TabPanel;
import com.google.gwt.user.client.ui.TableListener;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;

// Something to take care of login with session.
public class Login implements WebUIInterface {

	private WebSession session = null;
	
	// These can be as dumb as stone--don't need any listeners (we'll use the button for action).
	private TextBox usernameBox = new TextBox();
	private PasswordTextBox passwordBox = new PasswordTextBox();
	private RadioButton modRB = new RadioButton("interfaceRadioGroup", "MOD Curator");
	private RadioButton refRB = new RadioButton("interfaceRadioGroup", "Reference Genome");
	private PushButton loginButton = new PushButton("Login", "Login");    
	//	 Some HTML.
	// private HTML termTabHTML = new HTML("There is currently no data available.");

	public Login (WebSession webSession) {

		session = webSession;
		
		modRB.setChecked(true);

		// Setup the button to perform login duties.
		loginButton.addClickListener( new ClickListener() {
			public void onClick(Widget sender) {

				String iface = "";
				if( refRB.isChecked() ){
					iface = "ref";
				}else if( modRB.isChecked() ){
					iface = "mod";				
				}else{
					Window.alert("Something has gone very wrong with the interface selection. Logging in as ref anyways.");				
					iface = "ref";
				}

				doLogin(usernameBox.getText(),
						passwordBox.getText(),
						iface);
			}
		});
	}


	//
	private void doLogin(String uname, String pass, String iface){

		// TODO: Add some kind of check against the database.
		
		//Window.alert("Login not yet implemented.");
		String passToken = History.getToken();
		passToken = "general";
		History.newItem(passToken);
	}
	
	
	// Assemble the widget.
	public Widget getWidget() {

		VerticalPanel vp = new VerticalPanel();
		vp.add(new PageTitle("Login"));
		vp.add(usernameBox);
		vp.add(passwordBox);
		vp.add(refRB);
		vp.add(modRB);
		vp.add(loginButton);

		return vp;
	}


	public void updateWidget() {
		// No real need to update.
	}
}
