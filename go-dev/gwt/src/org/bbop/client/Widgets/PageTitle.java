package org.bbop.client.Widgets;

import com.google.gwt.user.client.ui.HTML;


//
public class PageTitle extends HTML{

	public PageTitle (String title) {

		super("<h2>" + title + "</h2>");
		//VerticalPanel p = new VerticalPanel();
		//p.add(new HTML("<h2>" + title + "</h2>"));
		//return p;
	}
}
