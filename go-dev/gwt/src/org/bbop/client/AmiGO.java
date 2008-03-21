package org.bbop.client;


import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.Command;
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
import com.google.gwt.user.client.ui.PushButton;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.client.ui.SourcesTabEvents;
import com.google.gwt.user.client.ui.SourcesTableEvents;
import com.google.gwt.user.client.ui.TabListener;
import com.google.gwt.user.client.ui.TabPanel;
import com.google.gwt.user.client.ui.TableListener;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;

/**
 * Entry point classes define <code>onModuleLoad()</code>.
 */
public class AmiGO implements EntryPoint {
		
	private String currentDB = "go_latest_lite";
	private String currentTerm = "GO:xxxxxxx";
	private int dbLimit = 10;
	private int dbStep = dbLimit;
	private int dbOffset = 0;
	private int dbTotal = -1;
	
	//	 Le Table.
    private FlexTable table = new FlexTable();
    private HTML termTabHTML = new HTML("There is currently no data available.");
    private HTML gpTabHTML = new HTML("There is currently no information available.");
    private HTML resultsSummary = new HTML("<b>Currently no results...</b>");
    //table.setText(0, 0, "No information currently availabl\.");

	// Let's make me a little tab bar.
	private TabPanel mainNav = new TabPanel();
    
	//
	private void doTermLookup(String identifier){
		
		currentTerm = identifier;
		
		GOLookupServiceAsync lookupService = (GOLookupServiceAsync) GWT.create(GOLookupService.class);
		((ServiceDefTarget) lookupService).setServiceEntryPoint( GWT.getModuleBaseURL() + "/GOLookupService");
		
		lookupService.isTerm(currentTerm, new AsyncCallback(){

			public void onSuccess(Object result) {

				Boolean res = (Boolean) result;
				boolean r = res.booleanValue();
				
				if( r ){					
					// If it is indeed a term, go back out and get number of GPs and GPs.
					dbOffset = 0;
					dbTotal = -1;
					doListUpdate(currentTerm);
					doTermUpdate(currentTerm);
				}else{
					Window.alert("Unknown term.");
				}
			}

			public void onFailure(Throwable caught) {
				Window.alert("Server error (isTerm): " + caught.toString());
			}
		});
	}
	
	//
	private void doCountRefresh(){
		
		if( dbTotal > 0){
			//
			resultsSummary.setHTML("<b>" + dbOffset + " - " + (dbOffset + dbLimit) + " of " + dbTotal + " results...</b>");
			//termTabHTML.setHTML(currentTerm);
			mainNav.selectTab(0);
		}else{
			Window.alert("Sorry, this term has no gene products.");
			resultsSummary.setHTML("<b>" + dbOffset + " - " + (dbOffset + dbLimit) + " of " + dbTotal + " results...</b>");
			
		}
	}
	
	//
	private void doListUpdate(String acc){
		
		GOLookupServiceAsync lookupService = (GOLookupServiceAsync) GWT.create(GOLookupService.class);
		((ServiceDefTarget) lookupService).setServiceEntryPoint( GWT.getModuleBaseURL() + "/GOLookupService");
				
		lookupService.getGPs(acc, dbLimit, dbOffset, new AsyncCallback(){

			public void onSuccess(Object result) {

				String[] res = (String[]) result;
				//Window.alert("GPs: " + res.length);

				mainNav.selectTab(0);
				gpTabHTML.setHTML("");
				
				// Wipe table.
				for( int i = table.getRowCount(); i > 0; i-- ){
					table.removeRow(0);
				}
				
				if( res.length > 1 ){
					// Regenerate table.
					for( int i = 0; i < res.length -1; i++ ){
						table.setText(i, 0, res[i]);
					}
					 // Regenerate count.
				}	
				dbTotal = Integer.parseInt( res[res.length -1] );
				doCountRefresh();
			}

			public void onFailure(Throwable caught) {
				Window.alert("Server error (getGPs): " + caught.toString());
			}
		});
	}

	//
	private void doTermUpdate(String acc){
		
		GOLookupServiceAsync lookupService = (GOLookupServiceAsync) GWT.create(GOLookupService.class);
		((ServiceDefTarget) lookupService).setServiceEntryPoint( GWT.getModuleBaseURL() + "/GOLookupService");
				
		lookupService.getTermInfo(acc, new AsyncCallback(){

			public void onSuccess(Object result) {

				String[] res = (String[]) result;
				//Window.alert("GPs: " + res.length);
				termTabHTML.setHTML(
						"id: " + res[0] + "<br />" +
						"name: " + res[1] + "<br />" +
						"term_type: " + res[2] + "<br />" +
						"acc: " + res[3] + "<br />" +
						"is_obsolete: " + res[4] + "<br />" +
						"is_root: " + res[5] + "<br />"
						);
				mainNav.selectTab(0);
			}

			public void onFailure(Throwable caught) {
				Window.alert("Server error (getTermInfo): " + caught.toString());
			}
		});
	}
	
	//
	private void doGPUpdate(String id){
		
		GOLookupServiceAsync lookupService = (GOLookupServiceAsync) GWT.create(GOLookupService.class);
		((ServiceDefTarget) lookupService).setServiceEntryPoint( GWT.getModuleBaseURL() + "/GOLookupService");
				
		lookupService.getGPInfo(id, new AsyncCallback(){

			public void onSuccess(Object result) {

				String[] res = (String[]) result;
				//Window.alert("GPs: " + res.length);
				gpTabHTML.setHTML(
						"id: " + res[0] + "<br />" +
						"symbol: " + res[1] + "<br />" +
						"dbxref_id: " + res[2] + "<br />" +
						"species_id: " + res[3] + "<br />" +
						"type_id: " + res[4] + "<br />" +
						"full_name: " + res[5] + "<br />"
						);
				mainNav.selectTab(1);
			}

			public void onFailure(Throwable caught) {
				Window.alert("Server error (getGPInfo): " + caught.toString());
			}
		});
	}
	
	//
	private void doSniff(){
		
		GOLookupServiceAsync lookupService = (GOLookupServiceAsync) GWT.create(GOLookupService.class);
		((ServiceDefTarget) lookupService).setServiceEntryPoint( GWT.getModuleBaseURL() + "/GOLookupService");
				
		lookupService.getSniff(new AsyncCallback(){

			public void onSuccess(Object result) {

				String s = (String) result;
				Window.alert("SNIFF:  " + s);
			}

			public void onFailure(Throwable caught) {
				Window.alert("Server error (SNIFF): " + caught.toString());
			}
		});
	}
	
	/**
	 * This is the entry point method.
	 */
	public void onModuleLoad() {

		mainNav.add(termTabHTML, "Term Info");
		mainNav.add(gpTabHTML, "GP Info");
		mainNav.add(new HTML("Still on beta."), "Beta");
		mainNav.selectTab(0);
		    
		// 
		table.setText(0, 0, "No information currently available.");
		table.addTableListener(new TableListener() {
	
				public void onCellClicked(SourcesTableEvents sender, int row, int cell) {


					// Clear all rows.
					
					// Add color to row.
					
					//Window.alert("What you want is:" + row);
					//termTabHTML.setHTML(table.getHTML(row, 0));
					//gpTabHTML.setHTML(table.getHTML(row, 0));
					doGPUpdate(table.getHTML(row, 0));
					mainNav.selectTab(1);
				}
			
		});
	    
	    mainNav.addTabListener(new TabListener() {

			public void onTabSelected(SourcesTabEvents sender, int tabIndex) {
				//Window.alert("What you want is in Beta.");
			}
	    
			public boolean onBeforeTabSelected(SourcesTabEvents sender, int tabIndex) {
				// Just for fun, let's disallow selection of 'bar'.
				if (tabIndex == 2){
					Window.alert("What you want is in Beta.");
					return false;
				}else{
					return true;
				}
			}
		});

	   // private static class SearchPopup extends PopupPanel {
	   // 	// Pop-up
	   // 	public SearchPopup(){
	   // 		super(true);
	   // 		setWidget(new Label("Click outside of this popup to close it"));
	   // 	}
	    //}
	    
	    // When the user hits enter in the textbox submit data.
		final TextBox textBox = new TextBox();
		textBox.setVisibleLength(30);
		textBox.setText(currentTerm);	
	    textBox.addKeyboardListener(new KeyboardListenerAdapter() {
	    	public void onKeyPress(Widget sender, char keyCode, int modifiers) {
	    		// Check for enter
	    		if ((keyCode == KeyboardListener.KEY_ENTER) && (modifiers == 0)) {
	    			doTermLookup(textBox.getText());
	    		}
	    	}
	    });

	    // Setup the button to call the service
	    final PushButton submit = new PushButton("Lookup", "Looking...");
	    submit.addClickListener( new ClickListener() {
	    	public void onClick(Widget sender) {
	    		doTermLookup(textBox.getText());
	    	}
	    });
	    
	    // Setup the button to increment.
	    final PushButton next = new PushButton("Next", "Looking...");
	    next.addClickListener( new ClickListener() {
	    	public void onClick(Widget sender) {
	    		if( (dbOffset + dbStep) <= dbTotal ){
	    			dbOffset += dbStep;
	    			doListUpdate(currentTerm);
	    		}else{
	    		}
	    	}	
	    });
	    
	    // Setup the button to increment.
	    final PushButton prev = new PushButton("Previous", "Looking...");
	    prev.addClickListener( new ClickListener() {
	    	public void onClick(Widget sender) {
	    		if( (dbOffset - dbStep) >= 0 ){
	    			dbOffset -= dbStep;
	    			doListUpdate(currentTerm);
	    		}else{
	    			
	    		}
	    	}
	    });
	    	    
		//
		MenuBar fileMenu = new MenuBar(true);
		fileMenu.addItem("Sniff", true, new Command(){
			public void execute() {
				doSniff();
				//Window.alert("Save not yet implemented.");
			}
		});
		fileMenu.addItem("Load", true, new Command(){
			public void execute() {
				Window.alert("Load not yet implemented.");
			}
		});
		fileMenu.addItem("Save", true, new Command(){
			public void execute() {
				Window.alert("Save not yet implemented.");
			}
		});
		MenuBar dbMenu = new MenuBar(true);
		dbMenu.addItem("GO DB alpha", true, new Command(){
			public void execute() {
				Window.alert("Alpha not yet implemented.");
			}
		});
		dbMenu.addItem("GO DB beta", true, new Command(){
			public void execute() {
				Window.alert("Beta not yet implemented.");
			}
		});
		
		MenuBar menu = new MenuBar();
		menu.setAutoOpen(true);	
	    menu.addItem(new MenuItem("File", fileMenu));
	    menu.addItem(new MenuItem("Database", dbMenu));
	    menu.setWidth("100%");
	    
	    // Final layout
	    //DockPanel dock = new DockPanel();
	    //dock.setHorizontalAlignment(DockPanel.ALIGN_LEFT);
	    //dock.add(menu, DockPanel.NORTH);
	    //dock.add(table, DockPanel.EAST);
	    //dock.add(mainNav, DockPanel.CENTER);

	    VerticalPanel vpRight = new VerticalPanel();
	    //vpRight.setHorizontalAlignment(HasHorizontalAlignment.ALIGN_RIGHT);
	    vpRight.add(new HTML("<br />"));
	    vpRight.add(new HTML("<b>Term Lookup:</b>"));
	    vpRight.add(textBox);
	    vpRight.add(submit);
	    vpRight.add(new HTML("<br />"));
	    vpRight.add(resultsSummary);
	    vpRight.add(next);
	    vpRight.add(prev);
	    vpRight.add(table);
	    
	    HorizontalPanel hp = new HorizontalPanel();
	    hp.add(vpRight);
	    hp.add(new HTML("<table width=\"40px\"><tr><td></td></tr></table>"));
	    hp.add(mainNav);	
	    // BUG: Hackity hack!
	    
	    VerticalPanel vp = new VerticalPanel();
	    vp.add(new HTML("<h2>AmiGO</h2>"));
	    vp.add(menu);
	    vp.add(hp);
	    
	    RootPanel.get().add(vp);
		//RootPanel.get("slot2").add(hp);
		//RootPanel.get("slot3").add(mainNav);
		//RootPanel.get("slot4").add(textBox);
		//RootPanel.get("slot5").add(submit);
		//RootPanel.get("slot6").add(menu);
	}
}
