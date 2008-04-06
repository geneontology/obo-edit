package org.bbop.client.View;

import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.Manager.NavPanelManagerI;

import com.google.gwt.user.client.ui.TextBox;



import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.widget.Button;
import net.mygwt.ui.client.widget.ExpandBar;
import net.mygwt.ui.client.widget.ExpandItem;
import net.mygwt.ui.client.widget.layout.FillLayout;


public  class NavPanelView  implements NavPanelManagerI {
	private ExpandBar exbar;
	private ExpandItem browseitem;
	private ExpandItem searchitem;
	private ExpandItem curationitem;
	private ExpandItem reportItem; 
	private RefGenomeViewListenerI refglistener;
	
	private RefGenomeView mainview; //parent view
	
	//child view attached to it
	private BrowsePanelView browseview;
	private SearchPanelView searchview;
	private ReportPanelView reportView;
	private CurationPanelView curationView;
	
	public NavPanelView(RefGenomeViewListenerI listener,RefGenomeView parent){
		refglistener = listener;
		mainview = parent;
		exbar = new ExpandBar(Style.MULTI);
		browseitem = new ExpandItem();
		searchitem = new ExpandItem();
		reportItem = new ExpandItem();
		curationitem = new ExpandItem();
		
		browseview = new BrowsePanelView(refglistener,mainview);
		browseview.creatView();
		
		searchview = new SearchPanelView(refglistener,mainview);
		searchview.createView();
		
		reportView = new ReportPanelView(refglistener, mainview);
		reportView.createView();
		
		curationView = new CurationPanelView(refglistener,mainview);
		curationView.createView();
		
		
	}
	
	public void createView(){
		browseitem.setText("Browse");
		browseitem.getContainer().add(browseview.getView());
		
		searchitem.setText("Search");
		searchitem.getContainer().add(searchview.getView());
		
		reportItem.setText("Report");
		reportItem.getContainer().add(reportView.getView());
		
		
		curationitem.setText("Curation");
		curationitem.getContainer().add(curationView.getView());
		
		exbar.add(browseitem);
		exbar.add(searchitem);
		exbar.add(reportItem);
		
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
