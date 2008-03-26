package org.bbop.client.View;

import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.Manager.NavPanelManagerI;



import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.widget.ExpandBar;
import net.mygwt.ui.client.widget.ExpandItem;
import net.mygwt.ui.client.widget.WidgetContainer;

public  class NavPanelView  implements NavPanelManagerI {
	private ExpandBar exbar;
	private ExpandItem browseitem;
	private ExpandItem searchitem;
	private ExpandItem curationitem;
	private RefGenomeViewListenerI refglistener;
	private WidgetContainer mainview;
	
	public NavPanelView(RefGenomeViewListenerI listener,WidgetContainer parent){
		refglistener = listener;
		mainview = parent;
		exbar = new ExpandBar(Style.MULTI);
		browseitem = new ExpandItem();
		searchitem = new ExpandItem();
		curationitem = new ExpandItem();
		
		
	}
	
	public void createView(){
		browseitem.setText("Browse");
		browseitem.getContainer().addText("List of genes");
		
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
	
	public void addCurationBar(){
		exbar.add(curationitem);
		mainview.layout();
		
	}

}
