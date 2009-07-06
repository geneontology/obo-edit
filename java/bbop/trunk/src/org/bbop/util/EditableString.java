package org.bbop.util;

import javax.swing.undo.*;
import java.util.*;

/**
 * A simple mutable String implementation that implements StateEditable
 */
import org.apache.log4j.*;

public class EditableString implements StateEditable {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(EditableString.class);

    String value;

    public EditableString(String in) {
	value = in;
    }

    public void setValue(String in) {
	value = in;
    }

    @Override
	public String toString() {
	return value;
    }

    public void storeState(Hashtable in) {
	in.put("strvalue", value);
    }

    public void restoreState(Hashtable in) {
	if (in.get("strvalue") != null)
	    value = (String) in.get("strvalue");
    }

    @Override
	public boolean equals(Object o) {
	if (o == null && value == null)
	    return true;
	if (o == null || value == null)
	    return false;
	return value.equals(o.toString());
    }
}
