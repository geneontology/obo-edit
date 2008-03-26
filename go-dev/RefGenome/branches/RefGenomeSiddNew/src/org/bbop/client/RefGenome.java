package org.bbop.client;








import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.client.GWT;


/**
 * Entry point classes define <code>onModuleLoad()</code>.
 */
public class RefGenome implements EntryPoint {
		
	/**
	 * This is the entry point method.
	 */
	public void onModuleLoad() {
		RefGenomeServiceClientImpl refgservice = new RefGenomeServiceClientImpl(GWT.getModuleBaseURL() + "refgenome"); 
       refgservice.getView().getViewPort().hideLoadingPanel("loading");
       
	   
	}	
}
