package org.bbop.client.View;

import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.Manager.LoginPanelManagerI;


import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.PasswordTextBox;
import com.google.gwt.user.client.ui.TextBox;

import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.event.BaseEvent;
import net.mygwt.ui.client.event.SelectionListener;
import net.mygwt.ui.client.widget.Button;
import net.mygwt.ui.client.widget.MessageBox;
import net.mygwt.ui.client.widget.ToolBar;
import net.mygwt.ui.client.widget.ToolItemAdapter;
import net.mygwt.ui.client.widget.WidgetContainer;

public class LoginPanelView implements LoginPanelManagerI {
	private ToolBar tbar;
	private Label userlabel;
	private Label passlabel ;
	private TextBox user;
	private PasswordTextBox pass;
	private Button loginbtn;
	private Button logoutbtn;
	
	private ToolItemAdapter useritem;
	private ToolItemAdapter userlabelitem;
	private ToolItemAdapter passitem;
	private ToolItemAdapter passlabelitem;
	private ToolItemAdapter loginitem;
	private ToolItemAdapter logoutitem;
	
	private RefGenomeViewListenerI refglistener;
	private WidgetContainer mainview;
	
	
	public LoginPanelView(RefGenomeViewListenerI listener, WidgetContainer parent) {
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
		userlabelitem = new ToolItemAdapter(userlabel);
		passlabelitem = new ToolItemAdapter(passlabel);
		useritem = new ToolItemAdapter(user);
		passitem = new ToolItemAdapter(pass);
		loginitem = new ToolItemAdapter(loginbtn);
		logoutitem = new ToolItemAdapter(logoutbtn);
		
		setAttr();
		addObservers();
	
	}

	public void createView(){
		
		
		//now assemble them on toolbar
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
		
		loginitem.setStyleAttribute("paddingTop","4px");
		loginitem.setStyleAttribute("paddingLeft","5px");					
		
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
		final MessageBox alert = new MessageBox(Style.ICON_ERROR, Style.OK);  
	    alert.setText("Login failed");  
	    alert.setMessage("Try again"); 
		
	}
	
	

	public void doLogout() {
		// TODO Auto-generated method stub
		tbar.removeAll();
		createView();
		mainview.layout();
		
	}

	public void enableLogin() {
		// TODO Auto-generated method stub
		tbar.removeAll();
		Label loggeduser = new Label(user.getText());
		ToolItemAdapter loggeduseritem = new ToolItemAdapter(loggeduser);
		
		loggeduseritem.setStyleAttribute("paddingTop", "4px");
		loggeduseritem.setStyleAttribute("paddingLeft", "5px");
		loggeduseritem.setStyleAttribute("paddingRight", "5px");
		logoutitem.setStyleAttribute("paddingTop","4px");
		logoutitem.setStyleAttribute("paddingLeft","5px");	
		
		tbar.add(loggeduseritem);
		tbar.add(logoutitem);
		mainview.layout();
		
		
	}
	
	private class LoginListener implements SelectionListener {
		public void widgetSelected(BaseEvent be) {
			refglistener.doLogin(user.getText(), pass.getText());
			
		}
	}
	
	private class LogoutListener implements SelectionListener {
		public void widgetSelected(BaseEvent be) {
			doLogout();
		}
	}
	
}
