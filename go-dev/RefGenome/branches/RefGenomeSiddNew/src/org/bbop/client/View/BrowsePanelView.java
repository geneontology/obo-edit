package org.bbop.client.View;


import net.mygwt.ui.client.event.BaseEvent;
import net.mygwt.ui.client.event.SelectionListener;
import net.mygwt.ui.client.widget.Button;


import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.Manager.BrowsePanelManagerI;

import com.google.gwt.user.client.ui.VerticalPanel;

public class BrowsePanelView implements BrowsePanelManagerI {
	private RefGenomeViewListenerI refglistener;
	private RefGenomeView mainview;
	private VerticalPanel browseBar;
	private Button targetBtn;
	private Button orthologBtn;
	
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
			refglistener.fetchTargetIds();
		}
	}

	

}
