package org.obo.filters;

import org.apache.log4j.*;

public class ObjectFilterFactory implements FilterFactory {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ObjectFilterFactory.class);

	public Filter createNewFilter() {
		return new ObjectFilterImpl();
	}
}
