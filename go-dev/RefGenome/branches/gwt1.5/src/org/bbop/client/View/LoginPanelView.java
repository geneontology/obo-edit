package org.bbop.client.View;

import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.Manager.LoginPanelManagerI;

import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.PasswordTextBox;
import com.google.gwt.user.client.ui.TextBox;


import com.extjs.gxt.ui.client.event.BaseEvent;
import com.extjs.gxt.ui.client.event.ButtonEvent;
import com.extjs.gxt.ui.client.event.Listener;
import com.extjs.gxt.ui.client.event.SelectionListener;
import com.extjs.gxt.ui.client.widget.button.Button;
import com.extjs.gxt.ui.client.widget.MessageBox;
import com.extjs.gxt.ui.client.widget.toolbar.ToolBar;
import com.extjs.gxt.ui.client.widget.toolbar.AdapterToolItem;

public class LoginPanelView implements LoginPanelManagerI {
	private ToolBar tbar;
	private Label userlabel;
	private Label passlabel;
	private TextBox user;
	private PasswordTextBox pass;
	private Button loginbtn;
	private Button logoutbtn;

	private AdapterToolItem useritem;
	private AdapterToolItem userlabelitem;
	private AdapterToolItem passitem;
	private AdapterToolItem passlabelitem;
	private AdapterToolItem loginitem;
	private AdapterToolItem logoutitem;

	private RefGenomeViewListenerI refglistener;
	private RefGenomeView mainview;

	public LoginPanelView(RefGenomeViewListenerI listener, RefGenomeView parent) {
		refglistener = listener;
		mainview = parent;
		tbar = new ToolBar();
		userlabel = new Label("User");
		passlabel = new Label("Password");
		user = new TextBox();
		pass = new PasswordTextBox();
		loginbtn = new Button("Login");
		logoutbtn = new Button("Logout");

		// pass it through toolbar item adapter
		userlabelitem = new AdapterToolItem(userlabel);
		passlabelitem = new AdapterToolItem(passlabel);
		useritem = new AdapterToolItem(user);
		passitem = new AdapterToolItem(pass);
		loginitem = new AdapterToolItem(loginbtn);
		logoutitem = new AdapterToolItem(logoutbtn);

		setAttr();
		addObservers();

	}

	public void createView() {

		// now assemble them on toolbar
		tbar.add(userlabelitem);
		tbar.add(useritem);
		tbar.add(passlabelitem);
		tbar.add(passitem);
		tbar.add(loginitem);

	}

	private void setAttr() {

		userlabelitem.setStyleAttribute("paddingTop", "4px");
		userlabelitem.setStyleAttribute("paddingLeft", "5px");

		passlabelitem.setStyleAttribute("paddingTop", "4px");
		passlabelitem.setStyleAttribute("paddingLeft", "10px");

		useritem.setStyleAttribute("paddingTop", "4px");
		passitem.setStyleAttribute("paddingTop", "6px");

		loginitem.setStyleAttribute("paddingTop", "4px");
		loginitem.setStyleAttribute("paddingLeft", "5px");

	}

	private void addObservers() {
		loginbtn.addSelectionListener(new LoginListener());
		logoutbtn.addSelectionListener(new LogoutListener());
	}

	public ToolBar getView() {
		return tbar;
	}

	public void denyLogin() {
		// TODO Auto-generated method stub
		MessageBox alert = new MessageBox();
		alert.setButtons(MessageBox.OK);
		alert.setIcon(MessageBox.WARNING);
		alert.setTitle("Login failed");
		alert.addCallback(new Listener<BaseEvent>() {
			public void handleEvent(BaseEvent be) {
				// TODO Auto-generated method stub
				
			}
		});
		alert.setMessage("Try again");
		alert.show();
	}

	public void doLogout() {
		// TODO Auto-generated method stub
		tbar.removeAll();
		createView();
		pass.setText("");
		mainview.getNavPanel().removeCurationBar();
		mainview.layout();

	}

	public void enableLogin() {
		// TODO Auto-generated method stub
		tbar.removeAll();
		Label loggeduser = new Label("Logged in as: " + user.getText());
		AdapterToolItem loggeduseritem = new AdapterToolItem(loggeduser);

		loggeduseritem.setStyleAttribute("paddingTop", "4px");
		loggeduseritem.setStyleAttribute("paddingLeft", "5px");
		loggeduseritem.setStyleAttribute("paddingRight", "5px");
		logoutitem.setStyleAttribute("paddingTop", "4px");
		logoutitem.setStyleAttribute("paddingLeft", "5px");

		tbar.add(loggeduseritem);
		tbar.add(logoutitem);
		mainview.layout();

	}

	private class LoginListener extends SelectionListener<ButtonEvent> {
		public void componentSelected(ButtonEvent be) {
			String username = user.getText();
			String passwd = pass.getText();
			if ((username == null || username.length() == 0)
					|| (passwd == null || passwd.length() == 0)) {
				denyLogin();
			} else {
				refglistener.doLogin(username, passwd);
			}

		}
	}

	private class LogoutListener extends SelectionListener<ButtonEvent> {
		@Override
		public void componentSelected(ButtonEvent ce) {
			// TODO Auto-generated method stub
			doLogout();
		}
	}

}
