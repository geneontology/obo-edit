package org.bbop.client.View;

import com.google.gwt.user.client.ui.HorizontalPanel;
import com.google.gwt.user.client.ui.Label;

public class TitlePanelView {
	private HorizontalPanel hpanel;
	private Label refglabel;
	
	public TitlePanelView() {
		hpanel = new HorizontalPanel();
		refglabel = new Label("RefGenome tracker interface");
		
	}
	
	public void createView() {
		hpanel.setStyleName("header");
		refglabel.setStyleName("title");
		hpanel.add(refglabel);
		
	}
	
	public HorizontalPanel getView() {
		return hpanel;
	}

}
