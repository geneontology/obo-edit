package org.bbop.client.View;

import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.Manager.NavPanelManagerI;



import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.widget.ExpandBar;
import net.mygwt.ui.client.widget.ExpandItem;


public  class NavPanelView  implements NavPanelManagerI {
	private ExpandBar exbar;
	private ExpandItem browseitem;
	private ExpandItem searchitem;
	private ExpandItem curationitem;
	private RefGenomeViewListenerI refglistener;
	
	private RefGenomeView mainview;
	private BrowsePanelView browseview;
	
	public NavPanelView(RefGenomeViewListenerI listener,RefGenomeView parent){
		refglistener = listener;
		mainview = parent;
		exbar = new ExpandBar(Style.MULTI);
		browseitem = new ExpandItem();
		searchitem = new ExpandItem();
		curationitem = new ExpandItem();
		browseview = new BrowsePanelView(refglistener,mainview);
		browseview.creatView();
		
		
	}
	
	public void createView(){
		browseitem.setText("Browse");
		browseitem.getContainer().add(browseview.getView());
		searchitem.setText("Search");
		searchitem.getContainer().addText("Search genes");
		
		curationitem.setText("Curation");
		curationitem.getContainer().addText("Curate genes");
		
		exbar.add(browseitem);
		exbar.add(searchitem);
		
	}
	
	public ExpandBar getView() {
		return exbar;
	}
	
	public BrowsePanelView getBrowseView() { return browseview; }
	
	public void addCurationBar(){
		exbar.add(curationitem);
		mainview.layout();
		
	}
	
	public void removeCurationBar() {
		exbar.remove(curationitem);
		
	}

}
