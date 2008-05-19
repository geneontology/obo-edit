package org.obo.filters;

import java.util.*;

/**
 * Tests either numeric or string equality, whichever is appropriate
 * 
 */
import org.apache.log4j.*;

public class EqualsComparison extends AbstractComparison {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(EqualsComparison.class);

	protected Class[] types = { String.class, Number.class };

	public Class[] getAcceptedTypes() {
		return types;
	}

	public String getID() {
		return "=";
	}

	public boolean matches(Collection testVals, String value) {
		if (value == null)
			value = "";
		Number intVal = null;
		// this is the recommended way to convert strings to numbers
		try {
			intVal = new Double(value);
		} catch (NumberFormatException ex) {
		}

		Iterator it = testVals.iterator();
		while (it.hasNext()) {
			Object o = it.next();
			boolean returnVal = false;
			if (o instanceof Number && intVal != null)
				returnVal = o.equals(intVal);
			else if (o instanceof String) {
				returnVal = ((String) o).equalsIgnoreCase(value);
			}
			if (returnVal)
				return true;
		}
		return false;
	}

	@Override
	public String toString() {
		return "equals";
	}
}
