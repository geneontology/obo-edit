package org.bbop.client.View;

import com.extjs.gxt.ui.client.event.BaseEvent;
import com.extjs.gxt.ui.client.event.ButtonEvent;
import com.extjs.gxt.ui.client.event.ComponentEvent;
import com.extjs.gxt.ui.client.event.SelectionListener;
import com.extjs.gxt.ui.client.widget.ContentPanel;
import com.extjs.gxt.ui.client.widget.button.Button;
import com.extjs.gxt.ui.client.widget.toolbar.ToolBar;

import org.bbop.client.Listener.RefGenomeViewListenerI;
import org.bbop.client.model.DateDTO;

import com.google.gwt.user.client.ui.VerticalPanel;

public class CurationPanelView {
	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	
	private ContentPanel curationBar;
	
	private Button targetBtn;
	private Button importBtn;
	
	public CurationPanelView (RefGenomeViewListenerI listener, RefGenomeView parent) {
		refgListener = listener;
		mainView = parent;
		
		curationBar = new ContentPanel();
		curationBar.setHeading("Curation");
		curationBar.setIconStyle("icon-curation");
		
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
		//curationBar.setSpacing(8);
		
	}
	
	public void addObservers() {
		importBtn.addSelectionListener(new ImportButtonListener());
	}
	
	public ContentPanel getView() {
		return curationBar;
	}
	
	private class ImportButtonListener extends SelectionListener<ButtonEvent> {
		@Override
		public void componentSelected(ButtonEvent ce) {
			// TODO Auto-generated method stub
			refgListener.uploadFile("test",null,null); // TODO - set userId
		}

	
	}

}
