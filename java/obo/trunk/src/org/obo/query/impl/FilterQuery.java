package org.obo.query.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;

import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.filters.Filter;
import org.obo.filters.ParentSearchCriterion;
import org.obo.query.Query;
import org.obo.reasoner.ReasonedLinkDatabase;

public class FilterQuery<T> implements Query<T, SearchHit<T>> {

	protected Filter<T> filter;
	protected Class<T> inputType;
	protected static Collection<FieldPathSpec> inputPaths;
	protected ReasonedLinkDatabase reasoner;

	static {
		inputPaths = new ArrayList<FieldPathSpec>();
		inputPaths.add(new FieldPathSpec());
		inputPaths.add(new FieldPathSpec(new ParentSearchCriterion()));
	}

	public FilterQuery(Filter<T> filter, Class<T> inputType,
			ReasonedLinkDatabase reasoner) {
		this.filter = filter;
		this.inputType = inputType;
		setReasoner(reasoner);
	}

	public T convertToInputType(SearchHit<T> original) {
		return original.getHit();
	}

	public SearchHit<T> convertToOutputType(T original) {
		return new BasicSearchHit<T>(original);
	}

	public Collection<SearchHit<T>> createResultHolder() {
		return null;
	}

	public Comparator<? super SearchHit<T>> getComparator() {
		return null;
	}

	public Class<T> getInputType() {
		return inputType;
	}

	public SearchHit<T> matches(T a) {
		filter.setReasoner(reasoner);
		if (filter.satisfies(a))
			return new BasicSearchHit<T>(a);
		else
			return null;
	}

	public Collection<FieldPathSpec> getInputPaths() {
		return inputPaths;
	}

	public void setFieldPath(FieldPath path) {
	}

	public ReasonedLinkDatabase getReasoner() {
		return reasoner;
	}

	public void setReasoner(ReasonedLinkDatabase reasoner) {
		this.reasoner = reasoner;
	}
}
