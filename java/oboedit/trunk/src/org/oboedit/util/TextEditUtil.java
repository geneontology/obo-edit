package org.oboedit.util;

import java.util.Collection;

import org.obo.datamodel.FieldPath;
import org.obo.datamodel.IdentifiedObject;
import org.obo.history.SingleTermOperationModel;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.OBOTextEditComponent;
import org.oboedit.gui.RootTextEditComponent;
import org.oboedit.verify.HistoryQuickFix;
import org.oboedit.verify.ImmediateQuickFix;
import org.oboedit.verify.QuickFix;
import org.oboedit.verify.TextReplaceQuickFix;

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
