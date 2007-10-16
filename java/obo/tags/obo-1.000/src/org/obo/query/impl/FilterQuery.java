package org.obo.query.impl;

import java.util.Collection;
import java.util.Comparator;

import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.filters.Filter;
import org.obo.query.Query;

public class FilterQuery<T> implements Query<T, SearchHit<T>> {

	protected Filter<T> filter;
	protected Class<T> inputType;
	
	public FilterQuery(Filter<T> filter, Class<T> inputType) {
		this.filter = filter;
		this.inputType = inputType;
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

	public Class<? super T> getInputType() {
		return inputType;
	}

	public SearchHit<T> matches(T a) {
		if (filter.satisfies(a))
			return new BasicSearchHit<T>(a);
		else
			return null;
	}

	public Collection<FieldPathSpec> getInputPaths() {
		return null;
	}

	public void setFieldPath(FieldPath path) {}
}
