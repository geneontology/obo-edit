package org.bbop.client.View;


import com.extjs.gxt.ui.client.Style;
import com.extjs.gxt.ui.client.event.BaseEvent;
import com.extjs.gxt.ui.client.event.ButtonEvent;
import com.extjs.gxt.ui.client.event.Listener;
import com.extjs.gxt.ui.client.event.SelectionListener;
import com.extjs.gxt.ui.client.event.WindowEvent;
import com.extjs.gxt.ui.client.widget.button.Button;
import com.extjs.gxt.ui.client.widget.MessageBox;
import com.extjs.gxt.ui.client.widget.toolbar.ToolBar;


import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.Manager.SearchPanelManagerI;
import org.bbop.client.model.NodeDTO;

import com.google.gwt.user.client.ui.ClickListener;
import com.google.gwt.user.client.ui.FocusListener;
import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.ListBox;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;

public class SearchPanelView implements SearchPanelManagerI {
	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	private ResultPanelView resultView;
	
	private VerticalPanel searchBar;
	private VerticalPanel txnSearchBar;
	private HorizontalPanel idSearchBar;
	private HorizontalPanel nameSearchBar;
	private HorizontalPanel targetSearchBar;
	private HorizontalPanel txnPanel;
	
	private Button idSearchBtn;
	private Button nameSearchBtn;
	private Button targetSearchBtn;
	private Button txnSearchBtn;
	private TextBox idSearchTerm;
	private TextBox nameSearchTerm;
	private TextBox targetSearchTerm;
	private TextBox txnSearchTerm;
	private ListBox txnList;
	
	private MessageBox info;
	private MessageBox listInfo;
	
	
	public SearchPanelView (RefGenomeViewListenerI listener,RefGenomeView parent){
		refgListener = listener;
		mainView = parent;
		searchBar = new VerticalPanel();
		txnSearchBar = new VerticalPanel();
		idSearchBar = new HorizontalPanel();
		nameSearchBar = new HorizontalPanel();
		targetSearchBar = new HorizontalPanel();
		txnPanel = new HorizontalPanel();
		
		idSearchBtn = new Button("Search by Id");
		idSearchTerm = new TextBox();
		
		nameSearchBtn = new Button("Search by Name");
		nameSearchTerm = new TextBox();
		
		targetSearchBtn = new Button("Search for Target");
		targetSearchTerm = new TextBox();
		
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

		targetSearchBar.add(targetSearchTerm);
		targetSearchBar.add(targetSearchBtn);
		
		txnList.addItem("Click for list....");
		
		
		txnPanel.add(txnSearchTerm);
		txnPanel.add(txnList);
		txnSearchBar.add(txnPanel);
		txnSearchBar.add(txnSearchBtn);
		
		searchBar.add(idSearchBar);
		searchBar.add(nameSearchBar);
		searchBar.add(targetSearchBar);
		searchBar.add(txnSearchBar);
		
	}
	
	public VerticalPanel getView () { return searchBar; }
	
	private void addObservers(){
		idSearchTerm.addFocusListener(new UserFocusListener());
		nameSearchTerm.addFocusListener(new UserFocusListener());
		txnSearchTerm.addFocusListener(new UserFocusListener());
		nameSearchBtn.addSelectionListener(new NameSearchListener());
		targetSearchBtn.addSelectionListener(new TargetSearchListener());
		txnSearchBtn.addSelectionListener(new NameSearchByTaxonListener());
		txnList.addClickListener(new ListBoxListener());
		//txnList.addFocusListener(new ListFocusListener());
	}
	
	private void setAttr () {
		searchBar.setSpacing(8);
		txnSearchBar.setSpacing(0);
		
		idSearchBar.setSpacing(10);
		nameSearchBar.setSpacing(10);
		txnPanel.setSpacing(10);
		
		idSearchTerm.setWidth("100");
		nameSearchTerm.setWidth("110");
		txnSearchTerm.setWidth("100");
		
		
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
	
	private class ListFocusListener implements FocusListener {

		public void onFocus(Widget sender) {
			// TODO Auto-generated method stub
			if (txnList.getItemCount() <= 2 ) {
				txnList.insertItem("Wait.....",0);
				refgListener.fetchTaxonNodes();
				MessageBox info = new MessageBox();
	    		info.setButtons(MessageBox.OK);
	    		info.setIcon(MessageBox.INFO);
	    		info.setTitle("Fetching taxon ids");
	    		info.addCallback(new Listener<BaseEvent>() {
	    			public void handleEvent(BaseEvent be) {
	    				// TODO Auto-generated method stub
	    			}
	    		});
	    		info.setMessage("Please wait");
	    		info.show();
			
			}
			
		}

		public void onLostFocus(Widget sender) {
			// TODO Auto-generated method stub
			
		}
		
	}
	
	private class ListBoxListener implements ClickListener {

		public void onClick(Widget sender) {
			// TODO Auto-generated method stub
			if (txnList.getItemCount() <= 2 ) {
				txnList.clear();
				txnList.addItem("Wait.....");
				MessageBox info = new MessageBox();
	    		info.setButtons(MessageBox.OK);
	    		info.setIcon(MessageBox.INFO);
	    		info.setTitle("Search in progress");
	    		info.addCallback(new Listener<BaseEvent>() {
	    			public void handleEvent(BaseEvent be) {
	    				// TODO Auto-generated method stub
	    			}
	    		});
	    		info.setMessage("Please wait");
	    		info.show();
			}
		}
		
	}
	
	private class NameSearchListener extends SelectionListener<ButtonEvent> {

		public void componentSelected(ButtonEvent ce)  {
			// TODO Auto-generated method stub
			final String userInput = nameSearchTerm.getText();
			if (!validateInput(userInput)) {
				MessageBox.alert("Alert", "No input given", 
						new Listener<WindowEvent>() {
					public void handleEvent(WindowEvent be) {
						// TODO Auto-generated method stub
						
					}
				  }		
			  );
			}
			else {
				
	    		refgListener.fetchByName(userInput);
	    		MessageBox info = new MessageBox();
	    		info.setButtons(MessageBox.OK);
	    		info.setIcon(MessageBox.INFO);
	    		info.setTitle("Search in progress");
	    		info.addCallback(new Listener<BaseEvent>() {
	    			public void handleEvent(BaseEvent be) {
	    				// TODO Auto-generated method stub
	    			}
	    		});
	    		info.setMessage("Please wait");
	    		info.show();
			}
		}

		
		
		
	}
	
	private class NameSearchByTaxonListener extends SelectionListener<ButtonEvent> {

		public void componentSelected(ButtonEvent be) {
			// TODO Auto-generated method stub
			final String userInput = txnSearchTerm.getText();
			if (!validateInput(userInput)) {
				MessageBox.alert("Invalid input", "Please try again",				
						new Listener<WindowEvent>() {
					public void handleEvent(WindowEvent be) {
						// TODO Auto-generated method stub	
					}
				  }	
				);
				
			}
			else {
				int selectedIdx = txnList.getSelectedIndex();
				String txnLabel = txnList.getItemText(selectedIdx);
				String[] tokens = txnLabel.split(" ",2); // TODO - is there a less hacky way?
	    		refgListener.fetchByNameAndTaxon(userInput, tokens[0]);
	    		MessageBox info = new MessageBox();
	    		info.setButtons(MessageBox.OK);
	    		info.setIcon(MessageBox.INFO);
	    		info.setTitle("Search in progress");
	    		info.addCallback(new Listener<BaseEvent>() {
	    			public void handleEvent(BaseEvent be) {
	    				// TODO Auto-generated method stub
	    			}
	    		});
	    		info.setMessage("Please wait");
	    		info.show();
				
			}
		}
		
	}
	
	private class TargetSearchListener extends SelectionListener<ButtonEvent> {

		public void componentSelected(ButtonEvent be) {
			// TODO Auto-generated method stub
			final String userInput = targetSearchTerm.getText();
			if (!validateInput(userInput)) {
				MessageBox.alert("Invalid input", "Please try again",				
						new Listener<WindowEvent>() {
					public void handleEvent(WindowEvent be) {
						// TODO Auto-generated method stub	
					}
				  }	
				);
			}
			else {
				
	    		refgListener.fetchTargetNodesByName(userInput);  
				MessageBox info = new MessageBox();
	    		info.setButtons(MessageBox.OK);
	    		info.setIcon(MessageBox.INFO);
	    		info.setTitle("Search in progress");
	    		info.addCallback(new Listener<BaseEvent>() {
	    			public void handleEvent(BaseEvent be) {
	    				// TODO Auto-generated method stub
	    			}
	    		});
	    		info.setMessage("Please wait");
	    		info.show();
				
			}
		}
		
	}
	
	

	public void displayNameSearchResult(Object obj) {
		
		
		NameSearchTableView tableView = new NameSearchTableView(refgListener, mainView);

		//Get the DTO object
		NodeDTO[] result = (NodeDTO[]) obj;
		info.hide();

		if (result.length < 1) {
			MessageBox.alert("Search result", "No result",				
					new Listener<WindowEvent>() {
				public void handleEvent(WindowEvent be) {
					// TODO Auto-generated method stub	
				}
			  }	
			);
		}
		else {
			tableView.createView(result);
			resultView = mainView.getResultPanel();
			// Remove the initial table view
			resultView.removeChildViews();
			//Add the new one
			resultView.addTableView(tableView.getView(), "Search result");
			resultView.resetView();
		}
		
	}
	

	// CUT AND PASTED FROM BROWSE PANEL
	// sorry to be so hacky - CJM.
	// need to sort out what goes where...
	public void displaySearchTargets(NodeDTO[] targetNodes) {
		info.hide();
		System.err.println("making table view");
		GenericNodeListTableView tableView = new GenericNodeListTableView(refgListener, mainView);
		tableView.addColumnHeading("OBO_REL:in_organism","species");
		tableView.addColumnHeading("oboInOwl:hasDbXref","xref");
		tableView.addColumnHeading("OBO_REL:homologous_to","orth");
		TargetToolBarView toolBarView = new TargetToolBarView(refgListener, mainView);
		TargetListToolBarView toolBarListView = new TargetListToolBarView(refgListener, mainView);
		// the following method should pass server side data
		toolBarView.createView();
		toolBarListView.createView();

		//Get the two toolbarviews
		ToolBar[] tbars = new ToolBar[2];
		tbars[0] = toolBarView.getView();
		tbars[1] = toolBarListView.getView();

		tableView.createView(targetNodes);
		System.err.println("created generic view");
		resultView = mainView.getResultPanel();
		// Remove the initial table view
		resultView.removeChildViews();
		//Add the new one
		System.err.println("adding table view");
		resultView.addTableView(tableView.getView(), "Target List");
		resultView.resetView();
	}


	public void fillTaxonNodes(Object obj) {
		// TODO Auto-generated method stub
		listInfo.hide();
		txnList.clear();
		System.err.println("got taxon list: "+obj);
		NodeDTO[] list = (NodeDTO[]) obj;
		for(int i = 0; i < list.length; i++) {
			if (list[i].getLabel() != null)
				txnList.addItem(list[i].getId()+" "+list[i].getLabel());
		}
		
	}
	
}
