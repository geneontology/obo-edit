package org.oboedit.util;

import java.util.Collection;

import org.obo.datamodel.FieldPath;
import org.oboedit.gui.OBOTextEditComponent;
import org.oboedit.gui.RootTextEditComponent;

import org.apache.log4j.*;

public class TextEditUtil {

	//initialize logger
//	protected final static Logger logger = Logger.getLogger(TextEditUtil.class);

	private TextEditUtil() {}
	
	public static void addDirtyPaths(OBOTextEditComponent c, Collection<FieldPath> dirty) {
		RootTextEditComponent root = c.getRoot();
		if (root != null) {
			for(FieldPath p : dirty) {
				root.addDirtyPath(p);
			}
		}
	}
}
