package org.oboedit.gui.tasks;

import org.bbop.framework.GUIManager;
import org.obo.datamodel.OBOSession;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.event.ReconfigEvent;
import org.oboedit.gui.event.ReconfigListener;
import org.oboedit.gui.Preferences;

import org.apache.log4j.*;

public class FrameNameUpdateTask extends AbstractReloadTask {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FrameNameUpdateTask.class);


	protected ReconfigListener listener = new ReconfigListener() {
                // This is triggered when the user saves to a file. When this happens, we
		// want to update the frame title.
		public void configReloaded(ReconfigEvent e) {
		    //		    logger.debug("configReloaded " + e); // DEL
		    reload();
		}
	};

	public FrameNameUpdateTask() {
		Preferences.getPreferences().addReconfigListener(listener);
		// Set initial title
		reload();
	}

	@Override
	public void reload() {
	    //	        logger.debug("FrameNameUpdateTask: setting title to " + getFrameTitle());
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
		if(SessionManager.getManager().getIncReasoningStatus())
			out += "^";
		if(SessionManager.getManager().getStepIncReasoningStatus())
			out += "_^";
		
//		logger.debug("FrameNameUpdateTask - " + out);
		return out;
	}
}
