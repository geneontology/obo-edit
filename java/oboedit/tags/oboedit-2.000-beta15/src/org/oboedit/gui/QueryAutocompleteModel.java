package org.oboedit.gui;

import java.util.Collection;

import org.bbop.swing.autocomplete.AutocompleteModel;
import org.obo.query.Query;

public interface QueryAutocompleteModel<DISPLAY_TYPE, OUTPUT_TYPE> extends
		AutocompleteModel<DISPLAY_TYPE, OUTPUT_TYPE> {

	public void setDisplayQuery(Query<DISPLAY_TYPE, DISPLAY_TYPE> query);
	public void setOutputQuery(Query<OUTPUT_TYPE, OUTPUT_TYPE> query);
	public Collection<DISPLAY_TYPE> getQueryInput();
}
