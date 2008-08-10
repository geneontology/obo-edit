package org.oboedit.gui.tasks;

import org.bbop.framework.GUIManager;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.IdentifiedObject;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;
import javax.swing.Icon;

import org.apache.log4j.*;

/** Load icons (used in Ontology Tree Editor, Graph Editor, etc.) for relationships that were in the input */

public class LoadRelationIconsTask extends AbstractReloadTask {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FrameNameUpdateTask.class);

	@Override
	public void reload() {
		long time = System.currentTimeMillis(); // DEL
		loadRelationIcons();
		logger.debug("Loaded relation icons in " + (System.currentTimeMillis() - time) + " ms"); // DEL
	}

	protected void loadRelationIcons() {
		for (IdentifiedObject obj : SessionManager.getManager().getSession().getLinkDatabase().getObjects()) {
			if (obj instanceof OBOProperty) {  // that's how relationships show up
//				logger.debug("loadRelationIcons: getIconForRelationshipType " + ((OBOProperty) obj).getName()); // DEL
				// We don't care about the icon now--we just want it to be cached so that
				// later when the user opens something in the Ontology Tree Editor, the
				// icon's ready.
				Preferences.getPreferences().getIconForRelationshipType((OBOProperty)obj);
			}
		}
	}
}
