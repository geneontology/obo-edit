package org.obo.filters;

/*
 * An implementation of {@link SearchComparison} to check whether a
 * some value in a collection of string values contains a given
 * comparison string. The match is case-insensitive.
 */

import java.util.*;

import org.bbop.util.StringUtil;

import org.apache.log4j.*;

public class ContainsComparison extends AbstractComparison {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ContainsComparison.class);

	protected Class[] types = { String.class };

	public Class[] getAcceptedTypes() {
		return types;
	}

	public String getID() {
		return "=~";
	}

	public boolean matches(Collection testVals, String value) {
		if (value == null || value.length() == 0)
			return false;

		Iterator it = testVals.iterator();
		while (it.hasNext()) {
			String s = (String) it.next();
			boolean returnVal = StringUtil.unicodeIndexOf(s, value) >= 0;
			if (returnVal)
				return true;
		}
		return false;
	}

	@Override
	public String toString() {
		return "contains";
	}
}
