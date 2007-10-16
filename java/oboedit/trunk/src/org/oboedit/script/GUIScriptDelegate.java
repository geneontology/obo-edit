package org.oboedit.script;

import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOSession;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;

public class GUIScriptDelegate {
	public LinkedObject getSubSelection() {
		return SelectionManager.getGlobalSelection().getTermSubSelection();
	}
	
	public OBOSession getSession() {
		return SessionManager.getManager().getSession();
	}
	
	public boolean autoCommitTextEdits() {
		return Preferences.getPreferences().getAutoCommitTextEdits();
	}
}
