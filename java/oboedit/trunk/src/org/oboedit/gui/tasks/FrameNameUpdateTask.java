package org.oboedit.gui.tasks;

import org.bbop.framework.GUIManager;
import org.obo.datamodel.OBOSession;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;

import org.apache.log4j.*;

public class FrameNameUpdateTask extends AbstractReloadTask {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FrameNameUpdateTask.class);

	public FrameNameUpdateTask() {
		// Set initial title
		reload();
	}

	@Override
	public void reload() {
		//logger.debug("FrameNameUpdateTask: setting title to " + getFrameTitle());
	        GUIManager.getManager().getFrame().setTitle(getFrameTitle());
	}

	protected String getFrameTitle() {
		String out = "OBO-Edit version " + Preferences.getVersion();
		OBOSession session = SessionManager.getManager().getSession();
		    String loadRemark = session.getLoadRemark();
		if (session != null && loadRemark != null
		    && loadRemark.length() > 0)
		    out +=  ": " + loadRemark;
		if (SessionManager.getManager().getUseReasoner())
		    out +=  " (REASONER ON)";
		return out;
	}
}
