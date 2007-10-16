package org.oboedit.gui.tasks;

import java.awt.Dimension;
import java.util.Collection;

import javax.swing.SwingUtilities;

import org.bbop.framework.GUIManager;
import org.bbop.framework.GUITask;
import org.bbop.swing.BackgroundUtil;
import org.bbop.util.AbstractTaskDelegate;
import org.bbop.util.TaskDelegate;
import org.obo.datamodel.IdentifiedObject;
import org.oboedit.controller.SessionManager;
import org.oboedit.controller.VerificationManager;
import org.oboedit.gui.event.ReloadEvent;
import org.oboedit.gui.event.ReloadListener;
import org.oboedit.gui.event.RootChangeEvent;
import org.oboedit.gui.event.RootChangeListener;
import org.oboedit.gui.widget.CheckWarningComponent;
import org.oboedit.util.GUIUtil;
import org.oboedit.verify.CheckTask;
import org.oboedit.verify.CheckWarning;

public class PostLoadVerifyTask implements GUITask {

	protected ReloadListener listener = new ReloadListener() {
		public void reload(ReloadEvent e) {
			if (e.isRoot() || e.isReasoner())
				doVerify();
		}
	};
	/*
	 * protected static class CheckRunnable implements Runnable { Collection c;
	 * 
	 * Runnable updateGUIRunnable = new Runnable() { public void run() { if
	 * (c.size() > 0) { CheckWarningComponent warningComponent = new
	 * CheckWarningComponent();
	 * 
	 * warningComponent.setWarnings(c, null, null, true, true, true);
	 * warningComponent.setPreferredSize(new Dimension(640, 640)); int
	 * dialogType = CheckWarningComponent.OK_OPTION;
	 * warningComponent.showDialog(GUIManager.getManager() .getFrame(),
	 * "Warnings found", dialogType); } } };
	 * 
	 * public void run() { byte condition = (byte) (VerificationManager.LOAD |
	 * VerificationManager.REASONER_ACTIVATED); c =
	 * VerificationManager.getManager().runChecks(
	 * SessionManager.getManager().getSession(), (IdentifiedObject) null,
	 * condition); }
	 * 
	 * public Runnable getUpdateGUIRunnable() { return updateGUIRunnable; } }
	 * 
	 * protected void doVerify() { CheckRunnable r = new CheckRunnable();
	 * BackgroundUtil.runInBackground(r, VerificationManager.getManager(), r
	 * .getUpdateGUIRunnable(), GUIManager.getManager().getFrame(), "Verifying",
	 * "Starting...", 500, false); }
	 */

	/*
	protected static TaskDelegate<Collection> verificationTask = new AbstractTaskDelegate<Collection>() {
		@Override
		public void execute() {
			byte condition = (byte) (VerificationManager.LOAD | VerificationManager.REASONER_ACTIVATED);
			final Collection c = VerificationManager.getManager().runChecks(
					SessionManager.getManager().getSession(),
					(IdentifiedObject) null, condition);
			setResults(c);
			Runnable updateGUIRunnable = new Runnable() {
				public void run() {
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
			SwingUtilities.invokeLater(updateGUIRunnable);
		}
	};
	*/

	public PostLoadVerifyTask() {
	}

	public void doVerify() {
		
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

	public void install() {
		GUIUtil.addReloadListener(listener);
	}

	public void shutdown() {
		GUIUtil.removeReloadListener(listener);
	}

}
