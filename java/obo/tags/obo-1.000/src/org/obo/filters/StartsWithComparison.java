package org.obo.filters;

import java.util.*;

public class StartsWithComparison extends AbstractComparison {

	protected Class[] types = { String.class };

	public Class[] getAcceptedTypes() {
		return types;
	}

	public String getID() {
		return "starts_with";
	}

	public boolean matches(Collection testVals, String value) {
		if (value == null)
			value = "";

		Iterator it = testVals.iterator();
		while (it.hasNext()) {
			String s = (String) it.next();
			if (s.toLowerCase().startsWith(value.toLowerCase()))
				return true;
		}
		return false;
	}

	@Override
	public String toString() {
		return "starts with";
	}
}
