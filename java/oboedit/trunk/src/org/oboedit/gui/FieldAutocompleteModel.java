package org.oboedit.gui;

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;

import org.bbop.swing.autocomplete.AbstractAutocompleteModel;
import org.bbop.util.FastSuperset;
import org.bbop.util.Superset;
import org.obo.datamodel.FieldPath;
import org.obo.datamodel.FieldPathSpec;
import org.obo.datamodel.LinkDatabase;
import org.obo.filters.SearchCriterion;
import org.obo.query.Query;
import org.obo.query.QueryEngine;
import org.obo.util.QueryUtil;
import org.oboedit.controller.SessionManager;

import org.apache.log4j.*;

public class FieldAutocompleteModel<T> extends AbstractAutocompleteModel<T, T> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FieldAutocompleteModel.class);

	protected Collection<FieldPathSpec> specs;
	protected LinkDatabase linkDatabase;
	protected Query<FieldPath, FieldPath> specQuery;
	protected Class<T> type;
	protected QueryEngine queryEngine;

	public FieldAutocompleteModel(Class<T> type) {
		specs = new LinkedList<FieldPathSpec>();
		this.type = type;
	}
	
	public void addPathSpec(FieldPathSpec spec) {
		specs.add(spec);
	}
	
	public void removePathSpec(FieldPathSpec spec) {
		specs.remove(spec);
	}
	
	public void addCriterion(SearchCriterion criterion) {
		addPathSpec(new FieldPathSpec(criterion));
	}

	public void removeCriterion(SearchCriterion criterion) {
		removePathSpec(new FieldPathSpec(criterion));
	}
	
	public void clear() {
		specs.clear();
	}
	
	public void setLinkDatabase(LinkDatabase linkDatabase) {
		this.linkDatabase = linkDatabase;
	}
	
	public LinkDatabase getLinkDatabase() {
		if (linkDatabase == null)
			return SessionManager.getManager().getSession().getLinkDatabase();
		else
			return linkDatabase;
	
	}
	
	public void setQuery(Query<FieldPath, FieldPath> specQuery) {
		this.specQuery = specQuery;
	}
	
	public T createValue(String val) {
		if (String.class.isAssignableFrom(type))
			return (T) val;
		else
			throw new UnsupportedOperationException();
	}

	public Collection<T> getAllValues() {
		Collection<FieldPath> paths = new FastSuperset<FieldPath>();
		for (FieldPathSpec spec : specs) {
			Collection<FieldPath> fpaths = FieldPath.resolve(spec,
					getLinkDatabase());
			((Superset<FieldPath>) paths).addSubset(fpaths);
		}
		if (specQuery != null)
			paths = QueryUtil.query(getQueryEngine(), paths, specQuery);
		Collection<T> out = new LinkedHashSet<T>();
		for(FieldPath p : paths) {
			T val = (T) p.getLastValue();
			out.add(val);
		}
		return out;
	}
	
	public void setQueryEngine(QueryEngine queryEngine) {
		this.queryEngine = queryEngine;
	}
	
	public QueryEngine getQueryEngine() {
		if (queryEngine == null)
			return SessionManager.getManager().getQueryEngine();
		else
			return queryEngine;
	}

	public Class<T> getDisplayType() {
		return type;
	}

	public List<T> getDisplayValues(T val) {
		return Collections.singletonList(val);
	}

	public Class<T> getOutputType() {
		return type;
	}

	public T getOutputValue(T val) {
		return val;
	}

	public boolean isLegal(T val) {
		for(T t : getAllValues()) {
			if (equals(t, val))
				return true;
		}
		return false;
	}
	
	protected boolean equals(T a, T b) {
		return a.equals(b);
	}

	public String toString(T val) {
	    if (val == null)
		return "";
	    return val.toString();
	}


}
