package org.obo.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import org.obo.datamodel.OBOProperty;
import org.obo.filters.BooleanCriterion;
import org.obo.filters.CompoundFilter;
import org.obo.filters.CompoundFilterImpl;
import org.obo.filters.EqualsComparison;
import org.obo.filters.Filter;
import org.obo.filters.IDSearchCriterion;
import org.obo.filters.LinkFilter;
import org.obo.filters.LinkFilterImpl;
import org.obo.filters.ObjectFilter;
import org.obo.filters.ObjectFilterImpl;
import org.obo.filters.SearchComparison;
import org.obo.filters.SearchCriterion;
import org.obo.filters.SelfSearchAspect;

public class FilterUtil {

	private FilterUtil() {
	}
	
	public static boolean isTypeOnlyLinkFilter(LinkFilter filter) {
		if (filter.getAspect() != LinkFilter.TYPE)
			return false;
		ObjectFilter of = filter.getFilter();
		return of.getCriterion() instanceof IDSearchCriterion &&
				of.getAspect() instanceof SelfSearchAspect &&
				of.getComparison() instanceof EqualsComparison;
	}
	
	public static String getTypeOnlyPropertyID(LinkFilter filter) {
		if (isTypeOnlyLinkFilter(filter)) {
			ObjectFilter of = filter.getFilter();
			return of.getValue();
		} else
			return null;
	}

	public static LinkFilter getTypeFilter(OBOProperty type) {
		LinkFilter basicLinkFilter = new LinkFilterImpl();
		basicLinkFilter
				.setAspect(org.obo.filters.LinkFilter.TYPE);

		ObjectFilter idfilter = new ObjectFilterImpl();
		idfilter.setCriterion(new IDSearchCriterion());
		idfilter.setComparison(new EqualsComparison());
		idfilter.setValue(type.getID());
		basicLinkFilter.setFilter(idfilter);
		return basicLinkFilter;
	}

	public static Collection filterCriteriaByInput(Collection allCriteria,
			Class c) {
		Collection out = new ArrayList();
		Iterator it = allCriteria.iterator();
		while (it.hasNext()) {
			SearchCriterion sc = (SearchCriterion) it.next();
			if (c.isAssignableFrom(sc.getInputType()))
				out.add(sc);
		}
		return out;
	}
	
	public static <T> Filter<T> mergeFilters(Filter<T> a, Filter<T> b) {
		if (a == null)
			return b;
		if (b == null)
			return a;
		CompoundFilter out = new CompoundFilterImpl(CompoundFilter.AND);
		out.addFilter(a);
		out.addFilter(b);
		return out;
	}

	public static Collection filterComparisonByInput(Collection allComparisons,
			Class c) {
		Collection out = new ArrayList();
		Iterator it = allComparisons.iterator();
		while (it.hasNext()) {
			SearchComparison sc = (SearchComparison) it.next();
			boolean matches = false;
			for (int i = 0; i < sc.getAcceptedTypes().length; i++) {
				Class compClass = sc.getAcceptedTypes()[i];
				if (compClass.isAssignableFrom(c)) {
					matches = true;
					break;
				}
			}
			if (matches)
				out.add(sc);
		}
		return out;
	}

	public static String getOBOFilterExpression(Filter filter) {
		if (filter instanceof CompoundFilter)
			return getOBOFilterExpression((CompoundFilter) filter);
		else if (filter instanceof ObjectFilter)
			return getOBOFilterExpression((ObjectFilter) filter);
		else if (filter instanceof LinkFilter)
			return getOBOFilterExpression((LinkFilter) filter);
		else
			throw new IllegalArgumentException(
					"Cannot create obo filter from filter of type "
							+ filter.getClass());
	}
	
	public static String getOBOFilterExpression(ObjectFilter filter) {
		StringBuffer out = new StringBuffer();
		if (!(filter.getAspect() instanceof SelfSearchAspect)) {
			out.append(filter.getAspect().getID());
			if (filter.getTraversalFilter() != null) {
				out.append("(");
				if (isTypeOnlyLinkFilter(filter.getTraversalFilter())) {
					out.append(getTypeOnlyPropertyID(filter.getTraversalFilter()));
				} else
					out.append(getOBOFilterExpression(filter.getTraversalFilter()));
				out.append(")");
			}
			out.append(" ");
		}
		out.append(filter.getCriterion().getID());
		if (!(filter.getCriterion() instanceof BooleanCriterion)) {
			out.append(" ");
			out.append(filter.getComparison().toString());
			out.append(" ");
			out.append("\"");
			out.append(filter.getValue());
			out.append("\"");
		}
		return out.toString();
	}
	
	public static String getOBOFilterExpression(LinkFilter filter) {
		StringBuffer out = new StringBuffer();
		out.append("link");
		if (filter.getAspect() == LinkFilter.CHILD) {
			out.append(".child");
		} else if (filter.getAspect() == LinkFilter.PARENT) {
			out.append(".parent");
		} else if (filter.getAspect() == LinkFilter.TYPE) {
			out.append(".type");
		}
		out.append("(");
		out.append(getOBOFilterExpression(filter.getFilter()));
		out.append(")");
		return out.toString();
	}
	
	public static String getOBOFilterExpression(CompoundFilter filter) {
		StringBuffer out = new StringBuffer();
		boolean first = true;
		for(Filter<?> f : filter.getFilters()) {
			if (!first) {
				out.append(" ");
				if (filter.getBooleanOperation() == CompoundFilter.AND)
					out.append("and");
				else if (filter.getBooleanOperation() == CompoundFilter.OR)
					out.append("or");
				else
					out.append("???");
				out.append(" ");
			} else
				first = false;
			if (f instanceof CompoundFilter)
				out.append("(");
			out.append(getOBOFilterExpression(f));
			if (f instanceof CompoundFilter)
				out.append(")");
		}
		return out.toString();
	}
}
