package org.obo.filters;

import java.util.*;

public class EqualsComparison extends AbstractComparison {

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
