package org.bbop.client.View;


import net.mygwt.ui.client.event.BaseEvent;
import net.mygwt.ui.client.event.SelectionListener;
import net.mygwt.ui.client.widget.Button;
import net.mygwt.ui.client.widget.ToolBar;

import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.Manager.BrowsePanelManagerI;

import com.google.gwt.user.client.ui.VerticalPanel;

public class BrowsePanelView implements BrowsePanelManagerI {
	private RefGenomeViewListenerI refglistener;
	private RefGenomeView mainview;
	private VerticalPanel browseBar;
	private Button targetBtn;
	private Button orthologBtn;	
	private ResultPanelView resultView;
	
	public  BrowsePanelView (RefGenomeViewListenerI listener, RefGenomeView parent){
		refglistener = listener;
		mainview = parent;
		
		browseBar = new VerticalPanel();
		targetBtn = new Button("List Target");
		orthologBtn = new Button("List Ortholog");
		
		
		setAttr();
		addObservers();
	}
	
	public void creatView() {
		browseBar.add(targetBtn);
		browseBar.add(orthologBtn);
		
		
	}
	
	public void setAttr() {
		browseBar.setSpacing(8);
		targetBtn.setIconStyle("icon-list");
		orthologBtn.setIconStyle("icon-list");
		
		
	}
	
	public VerticalPanel getView() { return browseBar; }
	
	private void addObservers() {
		targetBtn.addSelectionListener(new TargetListListener());
		
	}
	
	public void addTargetIds(String[] ids) {
		// TODO Auto-generated method stub
		
	}
	
	
	private class TargetListListener implements SelectionListener {
		public void widgetSelected(BaseEvent be) {
			//refglistener.fetchTargetIds();
			TargetTableView tableView = new TargetTableView(refglistener, mainview);
			TargetToolBarView toolBarView = new TargetToolBarView(refglistener, mainview);
			TargetListToolBarView toolBarListView = new TargetListToolBarView(refglistener, mainview);
			// the following method should pass server side data
			tableView.createView();
			toolBarView.createView();
			toolBarListView.createView();
			
			//Get the two toolbarviews
			ToolBar[] tbars = new ToolBar[2];
			tbars[0] = toolBarView.getView();
			tbars[1] = toolBarListView.getView();
			
			resultView = mainview.getResultPanel();
			resultView.removeChildViews();
			resultView.addTargetListView(tableView.getView(), tbars, "List of target");
			resultView.resetView();
		
		}
	}

	

}
