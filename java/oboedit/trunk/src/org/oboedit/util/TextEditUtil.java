package org.oboedit.util;

import java.util.Collection;

import org.obo.datamodel.FieldPath;
import org.obo.datamodel.IdentifiedObject;
import org.obo.history.SingleTermOperationModel;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.OBOTextEditComponent;
import org.oboedit.verify.HistoryQuickFix;
import org.oboedit.verify.ImmediateQuickFix;
import org.oboedit.verify.QuickFix;
import org.oboedit.verify.TextReplaceQuickFix;

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
