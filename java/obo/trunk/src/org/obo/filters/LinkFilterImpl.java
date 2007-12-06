package org.obo.filters;

import org.bbop.expression.JexlContext;
import org.obo.datamodel.*;
import org.obo.reasoner.ReasonedLinkDatabase;

public class LinkFilterImpl implements LinkFilter {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected int aspect = TYPE;
	protected ObjectFilter filter = new ObjectFilterImpl();
	protected JexlContext context;

	public LinkFilterImpl() {
	}

	/**
	 * Create a link filter that only matches links with a particular property
	 */
	public LinkFilterImpl(OBOProperty p) {
		ObjectFilter typeFilter = new ObjectFilterImpl();
		typeFilter.setCriterion(new IDSearchCriterion());
		typeFilter.setAspect(new SelfSearchAspect());
		typeFilter.setComparison(new EqualsComparison());
		typeFilter.setValue(p.getID());
		setAspect(LinkFilter.TYPE);
		setFilter(typeFilter);
	}

	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (Exception ex) {
			return null;
		}
	}

	public void setContext(JexlContext context) {
		this.context = context;
	}

	// public void lock() {
	// filter.lock();
	// }

	@Override
	public String toString() {
		StringBuffer out = new StringBuffer();
		if (aspect == CHILD)
			out.append("Link child");
		else if (aspect == PARENT)
			out.append("Link parent");
		else if (aspect == TYPE)
			out.append("Link type");
		else
			out.append("Link");
		out.append(" has ");
		out.append(filter.toString());
		return out.toString();
	}

	public int getAspect() {
		return aspect;
	}

	public void setAspect(int aspect) {
		this.aspect = aspect;
	}

	public ObjectFilter getFilter() {
		return filter;
	}

	public void setFilter(ObjectFilter filter) {
		this.filter = filter;
	}

	public boolean satisfies(Link link) {
		if (aspect == CHILD && link.getChild() != null)
			return filter.satisfies(link.getChild());
		else if (aspect == PARENT && link.getParent() != null)
			return filter.satisfies(link.getParent());
		else if (aspect == TYPE && link.getType() != null)
			return filter.satisfies(link.getType());
		else if (aspect == SELF)
			return filter.satisfies(link);
		else
			return false;
	}

	public void setReasoner(ReasonedLinkDatabase reasoner) {
		filter.setReasoner(reasoner);
	}
}
