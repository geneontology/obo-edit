package org.oboedit.script;

import org.obo.datamodel.IdentifiedObject;
import org.oboedit.gui.OBOTextEditComponent;

public class TextEditorScriptDelegate {
	
	protected OBOTextEditComponent c;

	public TextEditorScriptDelegate(OBOTextEditComponent c) {
		this.c = c;
	}
	
	public IdentifiedObject getObject() {
		return c.getObject();
	}
}
