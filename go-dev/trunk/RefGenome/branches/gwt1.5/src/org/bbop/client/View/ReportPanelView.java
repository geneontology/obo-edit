package org.bbop.client.View;

import com.extjs.gxt.ui.client.widget.ContentPanel;
import com.extjs.gxt.ui.client.widget.button.Button;

import org.bbop.client.Listener.RefGenomeViewListenerI;

import com.google.gwt.user.client.ui.VerticalPanel;

public class ReportPanelView {

	private RefGenomeViewListenerI refgListener;
	private RefGenomeView mainView;
	private ContentPanel reportPanel;

	private Button reportBtnOne;
	private Button reportBtnTwo;
	private Button reportBtnThree;

	public ReportPanelView(RefGenomeViewListenerI listner, RefGenomeView parent) {
		refgListener = listner;
		mainView = parent;
		reportPanel = new ContentPanel();
		reportPanel.setHeading("Report");
		reportPanel.setIconStyle("icon-report");

		reportBtnOne = new Button("First report");
		reportBtnTwo = new Button("Second report");
		reportBtnThree = new Button("Third report");

		setAttr();
		addObservers();
	}

	public void addObservers() {

	}

	public void setAttr() {
		//reportPanel.setSpacing(8);

		reportBtnOne.setIconStyle("icon-list");
		reportBtnTwo.setIconStyle("icon-list");
		reportBtnThree.setIconStyle("icon-list");
	}

	public ContentPanel getView() {
		return reportPanel;
	}

	public void createView() {
		reportPanel.add(reportBtnOne);
		reportPanel.add(reportBtnTwo);
		reportPanel.add(reportBtnThree);

	}
}
