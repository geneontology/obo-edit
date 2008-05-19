package org.obo.filters;

import org.obo.datamodel.Synonym;

import org.apache.log4j.*;

public class DefaultStringConverter implements StringConverter<Object> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefaultStringConverter.class);

	public String convert(Object o) {
		if (o == null) {
			return "";
		} else if (o instanceof Synonym) {
			return ((Synonym) o).getText();
		} else
			return o.toString();
	}

}
