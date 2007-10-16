package org.bbop.util;

import javax.swing.undo.*;
import java.util.*;

/**
 * A simple mutable String implementation that implements StateEditable
 */
public class EditableString implements StateEditable {

    String value;

    public EditableString(String in) {
	value = in;
    }

    public void setValue(String in) {
	value = in;
    }

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

    public boolean equals(Object o) {
	if (o == null && value == null)
	    return true;
	if (o == null || value == null)
	    return false;
	return value.equals(o.toString());
    }
}
