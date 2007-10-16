package org.obo.filters;

public interface FilterPair extends Cloneable {

	public static final FilterPair ALWAYS_TRUE = new FilterPairImpl(Filter.ALWAYS_TRUE, Filter.ALWAYS_TRUE);
	
	public Filter getObjectFilter();

	public ObjectRenderSpec getObjectRenderSpec();

	public Filter getLinkFilter();

	public LinkRenderSpec getLinkRenderSpec();

	public Object clone();

	public void setObjectFilter(Filter objectFilter);

	public void setObjectRenderSpec(ObjectRenderSpec objectSpec);

	public void setLinkFilter(Filter linkFilter);

	public void setLinkRenderSpec(LinkRenderSpec linkSpec);
	
	public RenderedFilter getLinkRenderer();
	public RenderedFilter getObjectRenderer();
}
