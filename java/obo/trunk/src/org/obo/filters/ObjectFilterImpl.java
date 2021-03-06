package org.obo.filters;

import org.bbop.expression.ExpressionException;
import org.bbop.expression.ExpressionUtil;
import org.bbop.expression.JexlContext;
import org.obo.datamodel.OBOProperty;
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

        protected boolean excludeObsoletes = false;

        protected SearchCriterion<?, ?> criterion = new AllTextFieldsCriterion();

	protected SearchCriterion wrappedCriterion = new StringCriterionWrapper(
			criterion, StringConverter.DEFAULT);

	protected SearchComparison comparison = new ContainsComparison();

	protected String value = "";

	public ObjectFilterImpl() {
          this(false);
	}

	public ObjectFilterImpl(boolean excludeObsoletes) {
          this.excludeObsoletes = excludeObsoletes;
          criterion = new AllTextFieldsCriterion(excludeObsoletes);
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

	// checking object's compliance to filter conditions
	public boolean satisfies(Object object) {
		if (criterion == null) {
//			logger.debug("satisfies(" +  object+ "): criterion is null"); 
			return false;
		}
                //                logger.debug("ObjectFilterImpl.satisfies -- input type = " + criterion.getInputType() + "; evaluating object: " + object + ", isAssignableFrom(" + object.getClass() + ") =" +  criterion.getInputType().isAssignableFrom(object.getClass())); // DEL
		if (!criterion.getInputType().isAssignableFrom(object.getClass()))
			return false;
		else {
			String matchVal = value;
			try {
				matchVal = ExpressionUtil.resolveBacktickExpression(value, context);
			} 
			catch (ExpressionException e) {
				logger.debug("ObjectFilterImpl -- matchVal error");
			}

			List os = new LinkedList();
			aspect.getObjects(os, getReasoner(), traversalFilter, object);
			criterion.setReasoner(getReasoner());
			boolean matches = negate;

			for(Object o : os){
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
					values = ((AbstractNumberCriterion) criterion).getValues(c, o);
				else
					values = wrappedCriterion.getValues(c, o);

				b = comparison.matches(values, matchVal);

				/*//if (b || object.getClass().getName().contains("OBOProperty")){
				 * Removing condition for OBOProperty objects getting a green flag through a set filter.
				 * This causes all relations to show up in the search results irrespective of their relevance to the set filter.
				 * If relations need to pass for certain conditions to hold..evaluate situation and set this at the higher level, not so late in the filtering process.
				 * */

				// this is giving all relations a free pass through search even when they dont match any search criterion.
				//if(b || object instanceof OBOProperty){
				if(b){
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
