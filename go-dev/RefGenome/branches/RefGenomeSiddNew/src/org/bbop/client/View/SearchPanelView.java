package org.bbop.client.View;

import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.widget.Button;
import net.mygwt.ui.client.widget.WidgetContainer;
import net.mygwt.ui.client.widget.layout.FillLayout;
import net.mygwt.ui.client.widget.layout.RowData;
import net.mygwt.ui.client.widget.layout.RowLayout;

import org.bbop.client.Listener.RefGenomeViewListenerI;

import com.google.gwt.user.client.ui.TextBox;

public class SearchPanelView {
	private RefGenomeViewListenerI refglistener;
	private RefGenomeView mainview;
	
	private WidgetContainer searchbar;
	private Button searchbtn;
	private TextBox searchterm;
	
	public SearchPanelView (RefGenomeViewListenerI listener,RefGenomeView parent){
		refglistener = listener;
		mainview = parent;
		
		searchbar = new WidgetContainer();
		searchbtn = new Button("Search");
		searchterm = new TextBox();
		
				
		addObservers();
	}
	
	public void createView () {
		searchbar.add(searchterm);
		searchbar.add(searchbtn);
		
		
	}
	
	public WidgetContainer getView () { return searchbar; }
	
	private void addObservers(){
		
	}
	
}
