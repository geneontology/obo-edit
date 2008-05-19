package org.obo.filters;

/*
 * A filter factory that builds {@link CompoundFilter}s.
 *
 */

import org.apache.log4j.*;

public class CompoundFilterFactory implements FilterFactory {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CompoundFilterFactory.class);

	/*
	 * Creates a new {@link CompoundFilterImpl}
	 * 
	 * @return a new compound filter
	 */
	public Filter createNewFilter() {
		return new CompoundFilterImpl();
	}
}
