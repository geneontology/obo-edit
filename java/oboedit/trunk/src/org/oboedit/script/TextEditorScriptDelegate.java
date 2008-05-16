package org.oboedit.script;

import org.obo.datamodel.IdentifiedObject;
import org.oboedit.gui.OBOTextEditComponent;

import org.apache.log4j.*;

public class TextEditorScriptDelegate {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TextEditorScriptDelegate.class);
	
	protected OBOTextEditComponent c;

	public TextEditorScriptDelegate(OBOTextEditComponent c) {
		this.c = c;
	}
	
	public IdentifiedObject getObject() {
		return c.getObject();
	}
}
