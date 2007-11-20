package org.obo.datamodel;

import java.io.Serializable;
import java.util.*;

public interface PropertyValue extends Cloneable, Serializable {

	public static Comparator COMPARATOR = new Comparator() {
		public int compare(Object a, Object b) {
			String namea = null;
			String nameb = null;

			if (a instanceof PropertyValue)
				namea = ((PropertyValue) a).getProperty();

			if (b instanceof PropertyValue)
				nameb = ((PropertyValue) b).getProperty();

			int compVal = namea.compareToIgnoreCase(nameb);
			if (compVal != 0)
				return compVal;
			else {
				String aval = ((PropertyValue) a).getValue();
				String bval = ((PropertyValue) b).getValue();
				return aval.compareToIgnoreCase(bval);
			}
		}
	};

	public String getValue();

	public String getProperty();

	public Object clone();

	public int getLineNumber();

	public String getFilename();

	public String getLine();
}
