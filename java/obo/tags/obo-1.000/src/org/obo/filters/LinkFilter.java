package org.obo.filters;

import org.obo.datamodel.Link;

public interface LinkFilter extends Filter<Link> {

	public static final int CHILD = 1;
	public static final int TYPE = 2;
	public static final int PARENT = 3;
	public static final int SELF = 4;

	public int getAspect();

	public void setAspect(int aspect);

	public ObjectFilter getFilter();

	public void setFilter(ObjectFilter filter);
}
