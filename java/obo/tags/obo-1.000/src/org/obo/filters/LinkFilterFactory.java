package org.obo.filters;

public class LinkFilterFactory implements FilterFactory {

	public Filter createNewFilter() {
		return new LinkFilterImpl();
	}
}
