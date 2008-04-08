package org.bbop.client.View;


import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.event.BaseEvent;
import net.mygwt.ui.client.event.SelectionListener;
import net.mygwt.ui.client.widget.Button;
import net.mygwt.ui.client.widget.MessageBox;


import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.Manager.SearchPanelManagerI;
import org.bbop.client.model.NodeDTO;

import com.google.gwt.user.client.ui.FocusListener;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.ListBox;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;

public class SearchPanelView implements SearchPanelManagerI {
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
		nameSearchBtn.addSelectionListener(new NameSearchListener());
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
	
	private boolean validateInput(String input) {
		if (input == null || input.length() == 0) {
			return false;
		}
		return true;
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
	
	private class NameSearchListener implements SelectionListener {

		public void widgetSelected(BaseEvent be) {
			// TODO Auto-generated method stub
			final String userInput = nameSearchTerm.getText();
			if (!validateInput(userInput)) {
				final MessageBox alert = new MessageBox(Style.ICON_ERROR, Style.OK);
				alert.setText("Invalid input");
				alert.setMessage("Please try again");
				alert.open();
			}
			else {
				refgListener.fetchByName(userInput);
			}
		}
		
	}

	public void displayNameSearchResult(Object obj) {
		
		ResultPanelView resultView = mainView.getResultPanel();
		NameSearchTableView tableView = new NameSearchTableView(refgListener, mainView);
		
		//Get the DTO object
		NodeDTO[] result = (NodeDTO[]) obj;
		tableView.createView(result);
		
		// Remove the initial table view
		resultView.removeChildViews();
		//Add the new one
		resultView.addTableView(tableView.getView());
		mainView.layout();
		
	}
	
}
