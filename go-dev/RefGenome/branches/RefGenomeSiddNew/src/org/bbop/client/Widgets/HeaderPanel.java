/**
 * 
 */
package org.bbop.client.Widgets;

import java.util.HashMap;

import org.bbop.client.Manager.LoginPanelManagerI;



import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.PasswordTextBox;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.Widget;

import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.event.BaseEvent;
import net.mygwt.ui.client.event.SelectionListener;
import net.mygwt.ui.client.widget.Button;
import net.mygwt.ui.client.widget.ToolBar;
import net.mygwt.ui.client.widget.ToolItemAdapter;
import net.mygwt.ui.client.widget.WidgetContainer;
import net.mygwt.ui.client.widget.layout.FillLayout;

/**
 * @author sid
 *
 */
public class HeaderPanel extends RefGWidgetTemplate  implements LoginPanelManagerI {

	private WidgetContainer north ;
	private boolean flagsubcomp = true;
	private boolean flagitem = true;
	private HashMap gwthash = new HashMap();
	private HashMap toolitems = new HashMap();
	private String[] itemnames = {"userlabel","user","passlabel","pass","loginbtn"};
	private HorizontalPanel hpanel;
	private ToolBar tbar;
	
	
	/**
	 * 
	 */
	public HeaderPanel() {
		north = new WidgetContainer();
		FillLayout northfill = new FillLayout(4);
		northfill.setType(Style.VERTICAL);
		north.setLayout(northfill);
		
		// TODO Auto-generated constructor stub
		
	}

	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetTemplate#configureComponents()
	 */
	
	public void configureComponents() {
		// TODO Auto-generated method stub
		hpanel.setStyleName("header");
		
	}

	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetTemplate#configureItems()
	 */
	
	public void configureItems() {
		// TODO Auto-generated method stub
		
		((ToolItemAdapter) toolitems.get("useritem")).setStyleAttribute("paddingTop", "4px");
		((ToolItemAdapter) toolitems.get("userlabelitem")).setStyleAttribute("paddingTop", "4px");
		((ToolItemAdapter) toolitems.get("passitem")).setStyleAttribute("paddingTop", "6px");
		((ToolItemAdapter) toolitems.get("passlabelitem")).setStyleAttribute("paddingTop", "4px");
		((ToolItemAdapter) toolitems.get("loginbtnitem")).setStyleAttribute("paddingTop","4px");
		
		
		((ToolItemAdapter) toolitems.get("passlabelitem")).setStyleAttribute("paddingLeft", "10px");
		((ToolItemAdapter) toolitems.get("userlabelitem")).setStyleAttribute("paddingLeft", "5px");
		((ToolItemAdapter) toolitems.get("loginbtnitem")).setStyleAttribute("paddingLeft","5px");
		
	}

	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetTemplate#setComponents()
	 */
	
	public void setComponents() {
		// TODO Auto-generated method stub
		hpanel = new HorizontalPanel();
	    tbar = new ToolBar();
	}

	
	
	
	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetInterface#setSubComponents()
	 */
	public void setSubComponents() {
		// TODO Auto-generated method stub
		gwthash.put("refgheader",new Label("RefGenome tracker interface"));
		gwthash.put("userlabel", new Label("User:"));
		gwthash.put("user",new TextBox());
		gwthash.put("passlabel", new Label("Pass:"));
		gwthash.put("pass",new PasswordTextBox());	
		gwthash.put("loginbtn", new Button("Login"));
		gwthash.put("logoutbtn", new Button("Logout"));
	}

	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetTemplate#setItems()
	 */
	
	public void setItems() {
		// TODO Auto-generated method stub
		for(int i = 0; i < itemnames.length; i++) {
			String name = itemnames[i];
			Widget gwtwidget = (Widget) gwthash.get(name);
			toolitems.put(name+"item",new ToolItemAdapter(gwtwidget));
		}
		
	}

	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetTemplate#configureSubComponents()
	 */
	public void configureSubComponents() {
		// TODO Auto-generated method stub
		Widget gwtwidget = (Widget) gwthash.get("refgheader");
		gwtwidget.setStyleName("title");
		
		Button loginbtn = (Button) gwthash.get("loginbtn");
		loginbtn.addSelectionListener(new SelectionListener() {  
			public void widgetSelected(BaseEvent be) {  
			 	   String user = ((TextBox) gwthash.get("user")).getText();
			 	 //  String pass = ((PasswordTextBox) gwthash.get("pass")).getText();
			 	  //final MessageBox alert = new MessageBox(Style.ICON_ERROR, Style.OK);  
			 	    //alert.setText("Alert");  
			 	    //alert.setMessage("Your hair is on fire!!!");
			 	   enableLogin();
			 	   //Client based rpc code should go here
			 	   //Login and logout buttons should be differentiated
			  }
		});
		
	}

	/* (non-Javadoc)
	 * @see org.bbop.client.Widgets.RefGWidgetTemplate#makeWidget()
	 */
	
	public void makeWidget() {
		// TODO Auto-generated method stub
		hpanel.add((Widget) gwthash.get("refgheader"));
		for (int i = 0; i<itemnames.length; i++) {
			tbar.add((ToolItemAdapter) toolitems.get(itemnames[i]+"item"));
		}
		
		//Finishing header panel
		north.add(hpanel);
		north.add(tbar);
		
		
	}
	
	public boolean isSubComponents() {
		return flagsubcomp;
	}
	
	public boolean isItems() {
		return flagitem;
	}

	public WidgetContainer getWidget() {
		return north;
	}
	


	public void denyLogin() {
		// TODO Auto-generated method stub
		
	}

	public void enableLogin() {
		// TODO Auto-generated method stub
		tbar.removeAll();
		
		String user = ((TextBox) gwthash.get("user")).getText();
		ToolItemAdapter useritem = new ToolItemAdapter(new Label(user));
		Widget gwtwidget = (Widget) gwthash.get("logoutbtn");
		ToolItemAdapter logoutitem = new ToolItemAdapter(gwtwidget);
		logoutitem.setStyleAttribute("paddingTop","4px");
		logoutitem.setStyleAttribute("paddingLeft","5px");
		
		tbar.add(useritem);
		tbar.add(logoutitem);
		north.layout();
		
	
	}

}
