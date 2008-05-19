package org.obo.filters;

import org.apache.log4j.*;

public class RegexpComparison extends WildcardComparison {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(RegexpComparison.class);

	@Override
	public String getID() {
		return "~=";
	}

	@Override
	public String getRegexp(String val) {
		return val;
	}

	@Override
	public String toString() {
		return "matches regexp";
	}
}
