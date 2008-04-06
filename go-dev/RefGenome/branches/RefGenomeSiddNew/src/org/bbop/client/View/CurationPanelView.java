package org.bbop.client.View;

import net.mygwt.ui.client.widget.Button;

import org.bbop.client.Listener.RefGenomeViewListenerI;

import com.google.gwt.user.client.ui.VerticalPanel;

public class CurationPanelView {
	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	
	private VerticalPanel curationBar;
	
	private Button targetBtn;
	
	public CurationPanelView (RefGenomeViewListenerI listener, RefGenomeView parent) {
		refgListener = listener;
		mainView = parent;
		
		curationBar = new VerticalPanel();
		
		targetBtn = new Button("Add Target");
		
		setAttr();
		addObservers();
	}
	
	public void createView () {
		curationBar.add(targetBtn);
		
	}
	
	public void setAttr() {
		targetBtn.setIconStyle("icon-add");
		curationBar.setSpacing(8);
		
	}
	
	public void addObservers() {
		
	}
	
	public VerticalPanel getView() {
		return curationBar;
	}
	
}
