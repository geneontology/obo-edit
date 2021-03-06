package org.oboedit.gui.tasks;

import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.IdentifiedObject;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;
import org.apache.log4j.*;

/** Load icons (used in Ontology Tree Editor, Graph Editor, etc.) for relationships that were in the input */

public class LoadRelationIconsTask extends AbstractReloadTask {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(LoadRelationIconsTask.class);

	@Override
	public void reload() {
		loadRelationIcons();
	}

	protected void loadRelationIcons() {
		for (IdentifiedObject obj : SessionManager.getManager().getSession().getLinkDatabase().getObjects()) {
			if (obj instanceof OBOProperty) {  // that's how relationships show up
				// We don't care about the icon now--we just want it to be cached so that
				// later when the user opens something in the Ontology Tree Editor, the
				// icon's ready.
				Preferences.getPreferences().getIconForRelationshipType((OBOProperty)obj);
			}
		}
	}
}
