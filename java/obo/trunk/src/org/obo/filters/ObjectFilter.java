package org.obo.filters;

import org.obo.datamodel.LinkedObject;

/*
 * Here is an example of how this interface can be used along with a LinkFilter object.
 * 
 * //Call the method:
 *
 *		LinkFilter lfilter1 = getLinkFilter("CARO:0000003", LinkFilter.CHILD);
 *		
 * //Method definition:
 *
 *	private LinkFilter getLinkFilter(String id, int aspect) {
 *
 *	//Initialize lfilter = Link type has Any text field contains ""
 *	LinkFilter lfilter = (LinkFilter)lff.createNewFilter();
 *	
 *	//Initialize ofilter = Any text field contains ""
 *	ObjectFilter ofilter = (ObjectFilter)off.createNewFilter();
 *		
 *	//Initialize EqualsComparison c = equals.
 *	EqualsComparison c = new EqualsComparison();
 *
 *	//now ofilter = Any text field equals ""
 *	ofilter.setComparison(c);
 *
 *	//Now ofilter = Any text field equals "CARO:0000003"
 *	ofilter.setValue(id);
 *
 *	//Now lfilter = Link type has Any text field equals "CARO:0000003"
 *	lfilter.setFilter(ofilter);
 *
 *	//Now lfilter = Link child has Any text field equals "CARO:0000003"
 *	lfilter.setAspect(aspect);
 *
 *	return lfilter;
 *	}
 *
 */

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
