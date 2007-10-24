package org.obo.filters;

public interface ObjectFilter extends PathCapableFilter {

	public boolean getNegate();

	public void setNegate(boolean negate);

	public SearchCriterion getCriterion();

	public void setCriterion(SearchCriterion c);

	public SearchAspect getAspect();

	public void setAspect(SearchAspect c);

	public SearchComparison getComparison();

	public void setComparison(SearchComparison c);

	public void setValue(String value);

	public String getValue();

	public LinkFilter getTraversalFilter();

	public void setTraversalFilter(LinkFilter traversalFilter);
}
