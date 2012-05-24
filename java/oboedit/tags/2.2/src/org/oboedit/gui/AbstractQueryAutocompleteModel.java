package org.oboedit.gui;

import java.util.Collection;
import java.util.Iterator;

import org.bbop.swing.autocomplete.AbstractAutocompleteModel;
import org.obo.query.Query;
import org.obo.query.QueryEngine;
import org.obo.util.QueryUtil;
import org.oboedit.controller.SessionManager;

public abstract class AbstractQueryAutocompleteModel<DISPLAY_TYPE, OUTPUT_TYPE>
		extends AbstractAutocompleteModel<DISPLAY_TYPE, OUTPUT_TYPE> implements
		QueryAutocompleteModel<DISPLAY_TYPE, OUTPUT_TYPE> {

	protected QueryEngine engine;
	protected Query<DISPLAY_TYPE, DISPLAY_TYPE> displayQuery;
	protected Query<OUTPUT_TYPE, OUTPUT_TYPE> outputQuery;

	public void setQueryEngine(QueryEngine engine) {
		this.engine = engine;
	}

	public QueryEngine getQueryEngine() {
		if (engine == null)
			return SessionManager.getManager().getQueryEngine();
		else
			return engine;
	}

	public Collection<DISPLAY_TYPE> getAllValues() {
		Collection<DISPLAY_TYPE> queryInput = getQueryInput();

		if (outputQuery != null) {
			Collection<OUTPUT_TYPE> outputVals = getOutputValues(queryInput);
			outputVals = QueryUtil.query(getQueryEngine(), outputVals, outputQuery);
			Collection<DISPLAY_TYPE> displayVals = getDisplayValues(outputVals);
			Iterator<DISPLAY_TYPE> it = displayVals.iterator();
			while (it.hasNext()) {
				DISPLAY_TYPE d = it.next();
				if (!queryInput.contains(d))
					it.remove();
			}
			queryInput = displayVals;
		}
		
		if (displayQuery != null) {
			queryInput = QueryUtil.query(getQueryEngine(), queryInput, displayQuery);
		}
		
		return queryInput;
	}

	public abstract Collection<DISPLAY_TYPE> getQueryInput();

	public void setDisplayQuery(Query<DISPLAY_TYPE, DISPLAY_TYPE> query) {
		this.displayQuery = query;
	}

	public void setOutputQuery(Query<OUTPUT_TYPE, OUTPUT_TYPE> query) {
		outputQuery = query;
	}

}
