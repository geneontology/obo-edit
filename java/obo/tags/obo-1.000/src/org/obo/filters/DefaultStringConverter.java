package org.obo.filters;

import org.obo.datamodel.Synonym;

public class DefaultStringConverter implements StringConverter<Object> {

	public String convert(Object o) {
		if (o == null) {
			return "";
		} else if (o instanceof Synonym) {
			return ((Synonym) o).getText();
		} else
			return o.toString();
	}

}
