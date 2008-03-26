package org.bbop.client.View;

import net.mygwt.ui.client.Style;
import net.mygwt.ui.client.widget.ContentPanel;

public class ResultPanelView {
	private ContentPanel centerpanel;
	public ResultPanelView(){
		centerpanel = new ContentPanel(Style.HEADER);
	}

	public void createView() {
		centerpanel.setText("Result");
	}
	
	public ContentPanel getView(){
		return centerpanel;
	}
}
