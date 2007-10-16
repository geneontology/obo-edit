package org.obo.filters;

/*
 * An implementation of {@link CompoundFilter}. Constituent filters are
 * evaluated in the order they were added. The {@link #satisfies(Object) }
 * method terminates as soon as the truth of the filter can be determined,
 * so longer running filters should be added last.
 */

import java.util.*;

import org.bbop.expression.JexlContext;

public class CompoundFilterImpl implements CompoundFilter {

	/**
	 * 
	 */
	private static final long serialVersionUID = -1115133463692494529L;
	protected List filters = new ArrayList();
	protected int booleanOperation = AND;
	protected JexlContext context;

	public CompoundFilterImpl() {
	}

	public CompoundFilterImpl(int booleanOperation) {
		this();
		setBooleanOperation(booleanOperation);
	}

	public boolean satisfies(Object o) {
		if (filters.size() == 0) {
			// return booleanOperation == OR;
			return true;
		}
		Iterator it = filters.iterator();
		while (it.hasNext()) {
			Filter f = (Filter) it.next();
			boolean result = f.satisfies(o);
			if (booleanOperation == AND && !result)
				return false;
			else if (booleanOperation == OR && result)
				return true;
		}
		if (booleanOperation == AND)
			return true;
		else
			return false;
	}

	public void setContext(JexlContext context) {
		this.context = context;
		Iterator it = filters.iterator();
		while (it.hasNext()) {
			Filter f = (Filter) it.next();
			f.setContext(context);
		}
	}

//	public void lock() {
//		Iterator it = filters.iterator();
//		while (it.hasNext()) {
//			Filter f = (Filter) it.next();
//			f.lock();
//		}
//	}

	public void clear() {
		filters.clear();
	}

	@Override
	public Object clone() {
		CompoundFilterImpl out = new CompoundFilterImpl();
		out.booleanOperation = booleanOperation;
		Iterator it = filters.iterator();
		while (it.hasNext()) {
			Filter filter = (Filter) ((Filter) it.next()).clone();
			filter.setContext(context);
			out.filters.add(filter);
		}
		return out;
	}

	public void addFilter(Filter f) {
		filters.add(f);
		f.setContext(context);
	}

	public void removeFilter(Filter f) {
		filters.remove(f);
	}

	public List getFilters() {
		return filters;
	}

	public void setFilters(List filters) {
		this.filters = filters;
	}

	public void setBooleanOperation(int booleanOperation) {
		if (booleanOperation != AND && booleanOperation != OR)
			throw new IllegalArgumentException("Only CompoundFilter.AND or "
					+ "CompoundFilter.OR are " + "allowed as boolean "
					+ "operations");
		this.booleanOperation = booleanOperation;
	}

	public int getBooleanOperation() {
		return booleanOperation;
	}

	/*
	 * Returns the name of the boolean operation in use.
	 * 
	 */
	@Override
	public String toString() {
		String op;
		if (booleanOperation == AND)
			op = "AND";
		else if (booleanOperation == OR)
			op = "OR";
		else
			op = "?";

		if (filters.size() == 0)
			return op;
		else if (filters.size() == 1)
			return filters.get(0).toString();
		else {
			StringBuffer out = new StringBuffer();
			out.append(filters.get(0));
			for (int i = 1; i < filters.size(); i++) {
				out.append(" ");
				out.append(op);
				out.append(" ");
				Filter f = (Filter) filters.get(i);
				if (f instanceof CompoundFilter)
					out.append("(");
				out.append(filters.get(i).toString());
				if (f instanceof CompoundFilter)
					out.append(")");
			}
			return out.toString();
		}
	}
}
