package org.obo.filters;

import org.apache.log4j.*;

public class LinkFilterFactory implements FilterFactory {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(LinkFilterFactory.class);

	public Filter createNewFilter() {
		return new LinkFilterImpl();
	}
}
