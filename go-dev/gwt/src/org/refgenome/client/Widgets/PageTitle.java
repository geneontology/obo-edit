package org.bbop.client.Widgets;

import java.util.Iterator;

import com.google.gwt.user.client.ui.HTML;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.VerticalPanel;
import com.google.gwt.user.client.ui.Widget;

public class PageTitle extends HTML{

	public PageTitle (String title) {

		super("<h2>" + title + "</h2>");
		//VerticalPanel p = new VerticalPanel();
		//p.add(new HTML("<h2>" + title + "</h2>"));
		//return p;
	}
}
