package org.obo.filters;

import org.bbop.expression.ExpressionException;
import org.bbop.expression.ExpressionUtil;
import org.bbop.expression.JexlContext;
import org.obo.reasoner.ReasonedLinkDatabase;

import java.util.*;

import org.apache.log4j.*;

public class ObjectFilterImpl implements ObjectFilter {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ObjectFilterImpl.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected JexlContext context;

	protected boolean negate = false;

	protected LinkFilter traversalFilter;

	protected SearchAspect aspect = new SelfSearchAspect();

	protected SearchCriterion<?, ?> criterion = new AllTextFieldsCriterion();

	protected SearchCriterion wrappedCriterion = new StringCriterionWrapper(
			criterion, StringConverter.DEFAULT);

	protected SearchComparison comparison = new ContainsComparison();

	protected String value = "";

	public ObjectFilterImpl() {
	}

	@Override
	public int hashCode() {
		return aspect.hashCode() + criterion.hashCode() + comparison.hashCode()
				+ value.hashCode() + (negate ? 1 : 0);
	}

	public void setContext(JexlContext context) {
		this.context = context;
	}

	public void lock() {
		try {
			value = ExpressionUtil.resolveBacktickExpression(value, context);
		} catch (ExpressionException ex) {
			ex.printStackTrace();
		}
	}

	@Override
	public boolean equals(Object o) {
		if (o instanceof ObjectFilter) {
			ObjectFilter filter = (ObjectFilter) o;
			return filter.getAspect().getClass().equals(aspect.getClass())
					&& filter.getCriterion().getClass().equals(
							criterion.getClass())
					&& filter.getComparison().getClass().equals(
							comparison.getClass())
					&& filter.getValue().equals(value);
		} else
			return false;
	}

	@Override
	public Object clone() {
		try {
			return super.clone();
		} catch (Exception ex) {
			return null;
		}
	}

	public static long allocTime = 0;

	protected ReasonedLinkDatabase reasoner;

	public ReasonedLinkDatabase getReasoner() {
		return reasoner;
	}

	public void setReasoner(ReasonedLinkDatabase reasoner) {
		this.reasoner = reasoner;
	}

	public boolean satisfies(Object object) {
		if (!criterion.getInputType().isAssignableFrom(object.getClass()))
			return false;
		else {
			String matchVal = value;
			try {
				matchVal = ExpressionUtil.resolveBacktickExpression(value,
						context);
			} catch (ExpressionException e) {
			}
			List os = new LinkedList();
			aspect.getObjects(os, getReasoner(), traversalFilter, object);
			criterion.setReasoner(getReasoner());
			boolean matches = negate;

			Iterator it = os.iterator();
			while (it.hasNext()) {
				Object o = it.next();

				if (criterion instanceof BooleanCriterion) {
					if (((BooleanCriterion) criterion).matches(o)) {
						matches = !negate;
						break;
					} else
						continue;
				}

				List<?> c = new LinkedList();
				boolean b = false;
				Collection values;
				if (criterion instanceof AbstractNumberCriterion)
					values = ((AbstractNumberCriterion) criterion)
							.getValues(c, o);
				else
					values = wrappedCriterion.getValues(c, o);

				b = comparison.matches(values, matchVal);

				if (b) {
					matches = !negate;
					break;
				}
			}

			return matches;
		}
	}

	@Override
	public String toString() {
		if (criterion instanceof BooleanCriterion)
			return (negate ? "NOT " : "")
					+ (aspect instanceof SelfSearchAspect ? "" : aspect
							.toString()
							+ " ") + criterion;
		else
			return (negate ? "NOT " : "")
					+ (aspect instanceof SelfSearchAspect ? "" : aspect
							.toString()
							+ " ") + criterion + " " + comparison + " \""
					+ value + "\"";
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public SearchComparison getComparison() {
		return comparison;
	}

	public void setComparison(SearchComparison comparison) {
		this.comparison = comparison;
	}

	public SearchAspect getAspect() {
		return aspect;
	}

	public void setAspect(SearchAspect aspect) {
		this.aspect = aspect;
	}

	public SearchCriterion getCriterion() {
		return criterion;
	}

	public void setCriterion(SearchCriterion criterion) {
		this.criterion = criterion;
		this.wrappedCriterion = new StringCriterionWrapper(criterion,
				StringConverter.DEFAULT);
	}

	public boolean getNegate() {
		return negate;
	}

	public void setNegate(boolean negate) {
		this.negate = negate;
	}

	public LinkFilter getTraversalFilter() {
		return traversalFilter;
	}

	public void setTraversalFilter(LinkFilter traversalFilter) {
		this.traversalFilter = traversalFilter;
	}
}
