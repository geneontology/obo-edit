package org.bbop.client.View;

import net.mygwt.ui.client.event.BaseEvent;
import net.mygwt.ui.client.event.SelectionListener;
import net.mygwt.ui.client.widget.Button;
import net.mygwt.ui.client.widget.ToolBar;

import org.bbop.client.Listener.RefGenomeViewListenerI;

import com.google.gwt.user.client.ui.VerticalPanel;

public class CurationPanelView {
	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	
	private VerticalPanel curationBar;
	
	private Button targetBtn;
	private Button importBtn;
	
	public CurationPanelView (RefGenomeViewListenerI listener, RefGenomeView parent) {
		refgListener = listener;
		mainView = parent;
		
		curationBar = new VerticalPanel();
		
		targetBtn = new Button("Add Target");
		importBtn = new Button("Import Spreadsheet");
		
		setAttr();
		addObservers();
	}
	
	public void createView () {
		curationBar.add(targetBtn);
		curationBar.add(importBtn);
		
	}
	
	public void setAttr() {
		targetBtn.setIconStyle("icon-database-add");
		importBtn.setIconStyle("icon-database-add");
		curationBar.setSpacing(8);
		
	}
	
	public void addObservers() {
		importBtn.addSelectionListener(new ImportButtonListener());
	}
	
	public VerticalPanel getView() {
		return curationBar;
	}
	
	private class ImportButtonListener implements SelectionListener {
		public void widgetSelected(BaseEvent be) {
			//refglistener.fetchTargetIds();
			refgListener.uploadFile("test",null,null); // TODO - set userId
		}
	}

}
