package org.obo.filters;

import java.util.Collection;
import java.util.LinkedList;

import org.apache.log4j.*;

public class StringCriterionWrapper<T, V> extends AbstractCriterion<T, String> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(StringCriterionWrapper.class);
	protected SearchCriterion<T, V> criterion;

	protected StringConverter<? super V> converter;

	protected LinkedList<V> myScratch = new LinkedList<V>();

	public StringCriterionWrapper(SearchCriterion<T, V> criterion,
			StringConverter<? super V> converter) {
		this.criterion = criterion;
		this.converter = converter;
	}

	public String getID() {
		return "string:" + criterion.getID();
	}

	public Class<T> getInputType() {
		return criterion.getInputType();
	}

	public Class<String> getReturnType() {
		return String.class;
	}

	public Collection<String> getValues(Collection<String> scratch, T obj) {
		LinkedList<V> myScratch = new LinkedList<V>();
		myScratch.clear();
		Collection<V> vals = criterion.getValues(myScratch, obj);
		for (V val : vals) {
			scratch.add(converter.convert(val));
		}
		return scratch;
	}

}
