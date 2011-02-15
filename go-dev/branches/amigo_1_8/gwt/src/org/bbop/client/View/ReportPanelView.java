package org.bbop.client.View;

import net.mygwt.ui.client.widget.Button;

import org.bbop.client.Listener.RefGenomeViewListenerI;

import com.google.gwt.user.client.ui.VerticalPanel;

public class ReportPanelView {

	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	private VerticalPanel reportPanel;
	
	private Button reportBtnOne;
	private Button reportBtnTwo;
	private Button reportBtnThree;
	
	
	public ReportPanelView(RefGenomeViewListenerI listner, RefGenomeView parent) {
		refgListener = listner;
		mainView = parent;
		reportPanel = new VerticalPanel();
		
		reportBtnOne = new Button("First report");
		reportBtnTwo = new Button("Second report");
		reportBtnThree = new Button("Third report");
		
		setAttr();
		addObservers();
	}
	
	public void addObservers(){
		
	}
	
	public void setAttr () {
		reportPanel.setSpacing(8);
		
		reportBtnOne.setIconStyle("icon-list");
		reportBtnTwo.setIconStyle("icon-list");
		reportBtnThree.setIconStyle("icon-list");
	}
	
	public VerticalPanel getView() { return reportPanel; }
	
	public void createView () {
		reportPanel.add(reportBtnOne);
		reportPanel.add(reportBtnTwo);
		reportPanel.add(reportBtnThree);
		
	}
}
