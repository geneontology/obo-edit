package org.bbop.client;

import com.extjs.gxt.ui.client.GXT;
import com.extjs.gxt.ui.client.util.Theme;
import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.ui.RootPanel;


/**
 * Entry point classes define <code>onModuleLoad()</code>.
 */
public class RefGenome implements EntryPoint {
		
	/**
	 * This is the entry point method.
	 */
	public void onModuleLoad() {
		 GXT.setDefaultTheme(Theme.BLUE, true);
		 GXT.hideLoadingPanel("loading");
		 RefGenomeServiceClientImpl refgservice = new RefGenomeServiceClientImpl(GWT.getModuleBaseURL() + "/RefGenomeService"); 
		RootPanel.get().add(refgservice.getView().getViewPort());
	}	
}
