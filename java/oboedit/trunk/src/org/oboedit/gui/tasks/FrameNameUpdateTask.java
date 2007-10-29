package org.oboedit.gui.tasks;

import org.bbop.framework.GUIManager;
import org.obo.datamodel.OBOSession;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;

public class FrameNameUpdateTask extends AbstractReloadTask {

	@Override
	public void reload() {
		GUIManager.getManager().getFrame().setTitle(getFrameTitle());
	}

	protected String getFrameTitle() {
		String out = "OBO-Edit version " + Preferences.getVersion();
		OBOSession session = SessionManager.getManager().getSession();
		if (session != null && session.getLoadRemark() != null
				&& session.getLoadRemark().length() > 0)
			return out + ": " + session.getLoadRemark();
		else
			return out;
	}
}
