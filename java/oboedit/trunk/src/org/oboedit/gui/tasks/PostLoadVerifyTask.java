package org.oboedit.gui.tasks;

import java.awt.Dimension;
import java.util.Collection;

import org.bbop.framework.GUIManager;
import org.obo.datamodel.IdentifiedObject;
import org.oboedit.controller.SessionManager;
import org.oboedit.controller.VerificationManager;
import org.oboedit.gui.widget.CheckWarningComponent;
import org.oboedit.verify.CheckTask;
import org.oboedit.verify.CheckWarning;

import org.apache.log4j.*;

public class PostLoadVerifyTask extends AbstractReloadTask {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(PostLoadVerifyTask.class);

	public PostLoadVerifyTask() {
	}
	
	public void reload() {
		
		byte condition = (byte) (VerificationManager.LOAD |
				VerificationManager.REASONER_ACTIVATED);

		final CheckTask task = VerificationManager.getManager().getCheckTask(
				SessionManager.getManager().getSession(),
				(IdentifiedObject) null, condition);
		Runnable updateGUIRunnable = new Runnable() {
			public void run() {
				Collection<CheckWarning> c = task.getResults();
				if (c.size() > 0) {
					CheckWarningComponent warningComponent = new CheckWarningComponent();

					warningComponent.setWarnings(c, null, null, true, true,
							true);
					warningComponent.setPreferredSize(new Dimension(640,
							640));
					int dialogType = CheckWarningComponent.OK_OPTION;
					warningComponent.showDialog(GUIManager.getManager()
							.getFrame(), "Warnings found", dialogType);
				}
			}
		};
		task.addPostExecuteRunnable(updateGUIRunnable);
		GUIManager.getManager().scheduleTask(task, true);
	}
}
