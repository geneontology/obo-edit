package org.oboedit.util;

import java.util.Collection;

import org.obo.datamodel.FieldPath;
import org.oboedit.gui.OBOTextEditComponent;

public class TextEditUtil {

	private TextEditUtil() {}
	
	public static void addDirtyPaths(OBOTextEditComponent c, Collection<FieldPath> dirty) {
		if (c.getRoot() != null) {
			for(FieldPath p : dirty) {
				c.getRoot().addDirtyPath(p);
			}
		}
	}
}
