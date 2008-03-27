package org.bbop.client.View;

import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.event.BaseEvent;
import net.mygwt.ui.client.event.SelectionListener;
import net.mygwt.ui.client.widget.Button;
import net.mygwt.ui.client.widget.ButtonBar;

import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.Manager.BrowsePanelManagerI;

public class BrowsePanelView implements BrowsePanelManagerI {
	private RefGenomeViewListenerI refglistener;
	private RefGenomeView mainview;
	private ButtonBar browsebar;
	private Button targetbtn;
	
	public  BrowsePanelView (RefGenomeViewListenerI listener, RefGenomeView parent){
		refglistener = listener;
		mainview = parent;
		
		browsebar = new ButtonBar(Style.VERTICAL);
		targetbtn = new Button("List target");
		targetbtn.setIconStyle("icon-list");
		
		addObservers();
	}
	
	public void creatView() {
		browsebar.add(targetbtn);
		
		
	}
	
	public ButtonBar getView() { return browsebar; }
	
	private void addObservers() {
		targetbtn.addSelectionListener(new TargetListListener());
		
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
