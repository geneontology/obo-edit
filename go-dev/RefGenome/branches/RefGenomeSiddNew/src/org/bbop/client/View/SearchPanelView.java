package org.bbop.client.View;


import net.mygwt.ui.client.widget.Button;


import org.bbop.client.Listener.RefGenomeViewListenerI;

import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.TextBox;

public class SearchPanelView {
	private RefGenomeViewListenerI refglistener;
	private RefGenomeView mainview;
	
	private HorizontalPanel searchbar;
	private Button searchbtn;
	private TextBox searchterm;
	
	public SearchPanelView (RefGenomeViewListenerI listener,RefGenomeView parent){
		refglistener = listener;
		mainview = parent;
		
		searchbar = new HorizontalPanel();
		searchbtn = new Button("Search");
		searchterm = new TextBox();
		
		
		setAttr();		
		addObservers();
	}
	
	public void createView () {
		searchbar.add(searchterm);
		searchbar.add(searchbtn);
		
		
	}
	
	public HorizontalPanel getView () { return searchbar; }
	
	private void addObservers(){
		
	}
	
	private void setAttr () {
		searchbar.setSpacing(10);
		searchterm.setWidth("100px");
		
			
	}
	
}
