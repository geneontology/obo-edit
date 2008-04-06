package org.bbop.client.View;


import net.mygwt.ui.client.widget.Button;


import org.bbop.client.Listener.RefGenomeViewListenerI;

import com.google.gwt.user.client.ui.FocusListener;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.ListBox;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;

public class SearchPanelView {
	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	
	private VerticalPanel searchBar;
	private VerticalPanel txnSearchBar;
	private HorizontalPanel idSearchBar;
	private HorizontalPanel nameSearchBar;
	private HorizontalPanel txnPanel;
	
	private Button idSearchBtn;
	private Button nameSearchBtn;
	private Button txnSearchBtn;
	private TextBox idSearchTerm;
	private TextBox nameSearchTerm;
	private TextBox txnSearchTerm;
	private ListBox txnList;
	
	
	public SearchPanelView (RefGenomeViewListenerI listener,RefGenomeView parent){
		refgListener = listener;
		mainView = parent;
		
		searchBar = new VerticalPanel();
		txnSearchBar = new VerticalPanel();
		idSearchBar = new HorizontalPanel();
		nameSearchBar = new HorizontalPanel();
		txnPanel = new HorizontalPanel();
		
		idSearchBtn = new Button("Search by Id");
		idSearchTerm = new TextBox();
		
		nameSearchBtn = new Button("Search by Name");
		nameSearchTerm = new TextBox();
		
		txnSearchBtn =  new Button("Search with Taxon id");
		txnSearchTerm = new TextBox();
		txnList = new ListBox();
		
		
		
		setAttr();		
		addObservers();
	}
	
	public void createView () {
		idSearchBar.add(idSearchTerm);
		idSearchBar.add(idSearchBtn);
		
		nameSearchBar.add(nameSearchTerm);
		nameSearchBar.add(nameSearchBtn);
		
		txnList.addItem("9606");
		txnList.addItem("10090");
		
		txnPanel.add(txnSearchTerm);
		txnPanel.add(txnList);
		txnSearchBar.add(txnPanel);
		txnSearchBar.add(txnSearchBtn);
		
		searchBar.add(idSearchBar);
		searchBar.add(nameSearchBar);
		searchBar.add(txnSearchBar);
		
	}
	
	public VerticalPanel getView () { return searchBar; }
	
	private void addObservers(){
		idSearchTerm.addFocusListener(new UserFocusListener());
		nameSearchTerm.addFocusListener(new UserFocusListener());
		txnSearchTerm.addFocusListener(new UserFocusListener());
	}
	
	private void setAttr () {
		searchBar.setSpacing(8);
		txnSearchBar.setSpacing(0);
		
		idSearchBar.setSpacing(10);
		nameSearchBar.setSpacing(10);
		txnPanel.setSpacing(10);
		
		
		idSearchTerm.setText("??? ...");
		nameSearchTerm.setText("????...");
		txnSearchTerm.setText("??....");
		
		txnList.setVisibleItemCount(1);
		txnSearchBtn.setStyleAttribute("paddingLeft", "8px");
		
			
	}
	
	private class UserFocusListener implements FocusListener {

		public void onFocus(Widget sender) {
			// TODO Auto-generated method stub
			TextBox textFocus = (TextBox) sender;
			textFocus.setText("");
			
			
		}

		public void onLostFocus(Widget sender) {
			// TODO Auto-generated method stub
			
		}
		
	}
	
}
