package org.obo.filters;

public class ObjectFilterFactory implements FilterFactory {

	public Filter createNewFilter() {
		return new ObjectFilterImpl();
	}
}
