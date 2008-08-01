package org.bbop.client.View;

import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.Manager.NavPanelManagerI;
import com.extjs.gxt.ui.client.widget.ContentPanel;
import com.extjs.gxt.ui.client.widget.layout.AccordionLayout;


public  class NavPanelView  implements NavPanelManagerI {
	private ContentPanel navPanel; 
	private RefGenomeViewListenerI refglistener;
	private RefGenomeView mainview; //parent view
	
	//child view attached to it
	private BrowsePanelView browseView;
	private SearchPanelView searchView;
	private ReportPanelView reportView;
	private CurationPanelView curationView;
	
	public NavPanelView(RefGenomeViewListenerI listener,RefGenomeView parent){
		refglistener = listener;
		mainview = parent;
		navPanel = new ContentPanel();
		navPanel.setLayout(new AccordionLayout());
		navPanel.setIconStyle("icon-accordion");
		
		browseView = new BrowsePanelView(refglistener,mainview);
		browseView.creatView();
		
		searchView = new SearchPanelView(refglistener,mainview);
		searchView.createView();
		
		reportView = new ReportPanelView(refglistener, mainview);
		reportView.createView();
		
		curationView = new CurationPanelView(refglistener,mainview);
		curationView.createView();
		
		
	}
	
	public void createView(){
		navPanel.add(browseView.getView());
		navPanel.add(searchView.getView());
		navPanel.add(reportView.getView());
		
	}
	
	public ContentPanel getView() {
		return navPanel;
	}
	
	public BrowsePanelView getBrowseView() { return browseView; }
	
	public void addCurationBar(){
		navPanel.add(curationView.getView());
		mainview.layout();
		
	}
	
	public void removeCurationBar() {
		navPanel.remove(curationView.getView());
		
	}
	
	public SearchPanelView getSearchPanelView() {
		return searchView;
	}

}
