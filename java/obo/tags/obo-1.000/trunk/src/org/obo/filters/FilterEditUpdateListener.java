package org.obo.filters;

import java.util.EventListener;

public class FilterEditUpdateListener implements EventListener {
	/**
	 * Called when a filter has been updated in some way, and editors need to
	 * refresh to reflect the new state. This method is called a lot, so to increase
	 * performance it has no associated event 
	 *
	 */
	public void update() {
		
	}
}
