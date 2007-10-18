package org.obo.filters;

import java.util.Collection;

public interface SearchComparison {

	public String getID();

	public Class[] getAcceptedTypes();

	// public void init();
	// public void cleanup();
	public boolean matches(Collection testVals, String value);

}
