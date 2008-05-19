package org.obo.filters;

import java.util.*;

import org.apache.log4j.*;

public class GreaterThanComparison extends AbstractComparison {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GreaterThanComparison.class);

	protected Class[] types = { Number.class };

	public Class[] getAcceptedTypes() {
		return types;
	}

	public String getID() {
		return ">";
	}

	public boolean matches(Collection testVals, String value) {
		if (value == null)
			value = "";
		Number val = null;
		try {
			val = new Double(value);
		} catch (NumberFormatException ex) {
		}

		if (val == null)
			return false;

		double dvalue = (val).doubleValue();

		Iterator it = testVals.iterator();
		while (it.hasNext()) {
			Object o = it.next();
			if (o instanceof Number && o != null) {
				double d = ((Number) o).doubleValue();
				if (d > dvalue)
					return true;
			}
		}
		return false;
	}
}
