package org.obo.filters;

public class FilterPairImpl implements FilterPair {

	protected Filter objectFilter;
	protected ObjectRenderSpec objectSpec;
	protected Filter linkFilter;
	protected LinkRenderSpec linkSpec;

	public FilterPairImpl() {
	}
	
	public FilterPairImpl(Filter objectFilter, Filter linkFilter) {
		this.objectFilter = objectFilter;
		this.linkFilter = linkFilter;
	}

	@Override
	public String toString() {
		String linkString = null;
		String termString = null;
		if (linkFilter != null) {
			linkString = "";
			if (linkSpec != null)
				linkString = linkSpec.toString() + " where ";
			linkString += linkFilter.toString();
		}
		if (objectFilter != null) {
			termString = "";
			if (objectSpec != null)
				termString = objectSpec.toString() + " where ";
			termString += objectFilter.toString();
		}
		if (linkString == null)
			return termString;
		else if (termString == null)
			return linkString;
		else
			return "link: " + linkString + ", term: " + termString;
	}

	@Override
	public Object clone() {
		FilterPairImpl out = new FilterPairImpl();

		if (objectFilter != null)
			out.objectFilter = (Filter) objectFilter.clone();

		if (objectSpec != null)
			out.objectSpec = (ObjectRenderSpec) objectSpec.clone();

		if (linkFilter != null)
			out.linkFilter = (Filter) linkFilter.clone();

		if (linkSpec != null)
			out.linkSpec = (LinkRenderSpec) linkSpec.clone();

		return out;
	}

	public void setObjectFilter(Filter objectFilter) {
		this.objectFilter = objectFilter;
	}

	public void setObjectRenderSpec(ObjectRenderSpec objectSpec) {
		this.objectSpec = objectSpec;
	}

	public void setLinkFilter(Filter linkFilter) {
		this.linkFilter = linkFilter;
	}

	public void setLinkRenderSpec(LinkRenderSpec linkSpec) {
		this.linkSpec = linkSpec;
	}

	public Filter getObjectFilter() {
		return objectFilter;
	}

	public ObjectRenderSpec getObjectRenderSpec() {
		return objectSpec;
	}

	public Filter getLinkFilter() {
		return linkFilter;
	}

	public LinkRenderSpec getLinkRenderSpec() {
		return linkSpec;
	}

	public RenderedFilter getLinkRenderer() {
		if (linkSpec != null && linkFilter != null)
			return new RenderedFilter(linkFilter, linkSpec);
		else
			return null;
	}

	public RenderedFilter getObjectRenderer() {
		if (objectSpec != null && objectFilter != null)
			return new RenderedFilter(objectFilter, objectSpec);
		else
			return null;
	}
}
