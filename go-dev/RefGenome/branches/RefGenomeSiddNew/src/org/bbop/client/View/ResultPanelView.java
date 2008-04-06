package org.bbop.client.View;

import org.bbop.client.Listener.RefGenomeViewListenerI;

import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.widget.ContentPanel;


public class ResultPanelView {
	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	
	
	private ContentPanel centerPanel;
	private boolean bootStrap = true;
	
	
	private SummaryTableView summView;
	
	public ResultPanelView(RefGenomeViewListenerI listener,RefGenomeView parent){
		refgListener = listener;
		mainView = parent;
		centerPanel = new ContentPanel(Style.HEADER);
	}

	public void createView() {
		if(bootStrap) {
			centerPanel.setText("Summary Table");
			summView = new SummaryTableView(refgListener,mainView);
			summView.createView();
			centerPanel.add(summView.getView());
		}
		else {
			centerPanel.setText("Empty display");
		}
			
		
}
	
	public void setBootStrap(boolean flag) {
		bootStrap = flag;
	}
	
	public ContentPanel getView(){
		return centerPanel;
	}
	
	
}
