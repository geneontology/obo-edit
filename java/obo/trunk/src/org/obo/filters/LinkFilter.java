package org.obo.filters;

import org.obo.datamodel.Link;

/*
 * Here is an example of how this interface can be used along with an ObjectFilter object.
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

public interface LinkFilter extends PathCapableFilter<Link> {

	public static final int CHILD = 1;
	public static final int TYPE = 2;
	public static final int PARENT = 3;
	public static final int SELF = 4;

	public int getAspect();

	public void setAspect(int aspect);

	public ObjectFilter getFilter();

	public void setFilter(ObjectFilter filter);
}
