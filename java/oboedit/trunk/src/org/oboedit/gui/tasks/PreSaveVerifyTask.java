package org.oboedit.gui.tasks;

import java.awt.Dimension;
import java.util.Collection;

import javax.swing.JOptionPane;

import org.bbop.framework.GUIManager;
import org.bbop.framework.GUITask;
import org.bbop.framework.IOEvent;
import org.bbop.framework.IOListener;
import org.bbop.framework.IOManager;
import org.bbop.swing.BackgroundUtil;
import org.obo.dataadapter.OBOAdapter;
import org.obo.datamodel.IdentifiedObject;
import org.oboedit.controller.SessionManager;
import org.oboedit.controller.VerificationManager;
import org.oboedit.gui.widget.CheckWarningComponent;
import org.oboedit.verify.CheckTask;
import org.oboedit.verify.CheckWarning;

import org.apache.log4j.*;

public class PreSaveVerifyTask implements GUITask {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(PreSaveVerifyTask.class);

	protected IOListener listener = new IOListener() {

		protected boolean fatal;

		public void operationExecuted(IOEvent<?> e) {
		}

		public boolean willExecuteOperation(IOEvent<?> e) {
			if (!e.getOperation().equals(OBOAdapter.WRITE_ONTOLOGY))
				return true;
			fatal = false;
			byte condition = (byte) VerificationManager.SAVE;

			final CheckTask task = VerificationManager.getManager()
					.getCheckTask(SessionManager.getManager().getSession(),
							(IdentifiedObject) null, condition);
			Runnable updateGUIRunnable = new Runnable() {
				public void run() {
					Collection<CheckWarning> c = task.getResults();
					if (c.size() > 0) {
						byte warningConditions = VerificationManager
								.getManager().getWarningConditions();

						int fatalCount = VerificationManager.countFatal(c);
						int warningCount = c.size() - fatalCount;
						boolean showWarnings = VerificationManager
								.getConditionAtField(warningConditions,
										VerificationManager.SAVE);
						if (fatalCount > 0
								|| (warningCount > 0 && showWarnings)) {
							CheckWarningComponent warningComponent = new CheckWarningComponent();

							String header = null;
							String footer = null;
							if (fatalCount > 0) {
								header = "<p>Cannot save because of fatal errors. "
										+ "These errors must be addressed before the save can "
										+ "proceed. A copy of this list of errors - with hyperlinks "
										+ "to the associated terms - can be found "
										+ "on the \"Verification Results\" tab of the "
										+ "Verification Plugin.</p>";
							} else {
								header = "<p>Non-fatal warnings were found. These warnings "
										+ "may be ignored and the save will continue normally. A "
										+ "copy of this list of warnings - with hyperlinks "
										+ " to the associated terms - can be found "
										+ "on the \"Verification Results\" tab of the "
										+ "Verification Plugin.</p>";
								footer = "<p><b><font size=5>Proceed with save?</font></b>";
							}
							warningComponent.setWarnings(c, header, footer,
									true, showWarnings, true);
							warningComponent.setPreferredSize(new Dimension(
									640, 640));
							int dialogType = CheckWarningComponent.OK_OPTION;
							if (fatalCount == 0)
								dialogType = CheckWarningComponent.YES_NO_OPTION;
							int dialogVal = warningComponent.showDialog(
									GUIManager.getManager().getFrame(),
									(fatalCount > 0 ? "Fatal errors found"
											: "Warnings found"), dialogType);
							if (fatalCount > 0
									|| dialogVal != CheckWarningComponent.YES_VALUE)
								fatal = true;
							return;
						}
					}
				}
			};
			task.addPostExecuteRunnable(updateGUIRunnable);
			task.addCancelledRunnable(new Runnable() {

				public void run() {
					fatal = true;
					JOptionPane.showMessageDialog(GUIManager.getManager()
								      .getFrame(), "Save cancelled");
					// We also need to cancel the little "working" modal dialog window that was
					// created by BackgroundUtil.scheduleTask
//					sleep(100); // didn't help
					task.cancel();  // didn't help
				}

			});
//			BackgroundUtil.scheduleTask(GUIManager.getManager()
//						    .getScreenLockQueue(), task, true, "OBO-Edit: running verifier");
			// The line below was commented out, but it works better than the one above (BackgroundUtil.scheduleTask),
			// which tended to leave those vestigial "working" windows if the user hit Cancel.
			GUIManager.getManager().runTaskNow(task, true);
			return !fatal && !task.isCancelled() && !task.isFailed();
		}
	};

	public PreSaveVerifyTask() {
	}

	public void install() {
		IOManager.getManager().addIOListener(listener);
	}

	public void shutdown() {
		IOManager.getManager().removeIOListener(listener);
	}
}
