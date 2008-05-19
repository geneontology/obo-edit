package org.obo.filters;

import java.util.EventListener;

import org.apache.log4j.*;

public class FilterEditUpdateListener implements EventListener {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FilterEditUpdateListener.class);
	/**
	 * Called when a filter has been updated in some way, and editors need to
	 * refresh to reflect the new state. This method is called a lot, so to increase
	 * performance it has no associated event 
	 *
	 */
	public void update() {
		
	}
}
