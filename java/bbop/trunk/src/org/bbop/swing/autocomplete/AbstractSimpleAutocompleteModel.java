package org.bbop.swing.autocomplete;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

public abstract class AbstractSimpleAutocompleteModel<TYPE> extends
		AbstractAutocompleteModel<TYPE, TYPE> {

	public List<TYPE> getDisplayValues(TYPE val) {
		return Collections.singletonList(val);
	}

	public TYPE getOutputValue(TYPE val) {
		return val;
	}

	public boolean isLegal(TYPE val) {
		return getAllValues().contains(val);
	}

}
