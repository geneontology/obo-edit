package org.bbop.swing.autocomplete;

import java.util.Collection;

public abstract class AbstractListAutocompleteModel<T> extends
		AbstractSimpleAutocompleteModel<T> {

	protected Collection<T> values;

	public Collection<T> getAllValues() {
		return values;
	}
	
	public void setValues(Collection<T> values) {
		this.values = values;
	}
}
