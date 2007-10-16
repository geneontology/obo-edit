package org.oboedit.controller;

import java.awt.Color;
import java.awt.Dimension;
import java.util.Collection;
import java.util.LinkedList;

import javax.swing.JOptionPane;

import org.bbop.dataadapter.DataAdapter;
import org.bbop.dataadapter.DataAdapterRegistry;
import org.bbop.dataadapter.DefaultAdapterRegistry;
import org.bbop.dataadapter.GraphicalAdapterChooser;
import org.bbop.framework.GUIManager;
import org.bbop.swing.BackgroundUtil;
import org.obo.dataadapter.OBOEditAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.OBOSession;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.event.IOEvent;
import org.oboedit.gui.event.IOListener;
import org.oboedit.gui.widget.CheckWarningComponent;
import org.oboedit.verify.CheckTask;
import org.oboedit.verify.CheckWarning;

public class IOManager {

	protected static IOManager manager;

	protected Collection<IOListener> listeners = new LinkedList<IOListener>();

	protected DataAdapterRegistry adapterRegistry;

	public DataAdapterRegistry getAdapterRegistry() {
		return adapterRegistry;
	}

	public DataAdapter getDefaultReadAdapter() {
		if (adapterRegistry == null)
			return new OBOFileAdapter();
		else
			return adapterRegistry.getAdapter("OBO_EDIT:OBO_Adapter");
	}

	public static IOManager getManager() {
		if (manager == null)
			manager = new IOManager();
		return manager;
	}

	protected boolean fireWillChangeRoot(IOEvent e) {
		for (IOListener listener : new LinkedList<IOListener>(listeners)) {
			if (!listener.willChangeRoot(e))
				return false;
		}
		return true;
	}

	protected boolean fireWillSaveRoot(IOEvent e) {
		for (IOListener listener : new LinkedList<IOListener>(listeners)) {
			if (!listener.willSaveRoot(e))
				return false;
		}
		return true;
	}

	public void addIOListener(IOListener listener) {
		listeners.add(listener);
	}

	public void removeIOListener(IOListener listener) {
		listeners.remove(listener);
	}

	protected IOManager() {
		adapterRegistry = new DefaultAdapterRegistry();
	}

	public void installDataAdapter(DataAdapter adapter) {
		adapterRegistry.addAdapter(adapter);
	}

	public void removeDataAdapter(DataAdapter adapter) {
		adapterRegistry.removeAdapter(adapter);
	}

	public void load() {
		if (!fireWillChangeRoot(new IOEvent(this)))
			return;
		if (SessionManager.getManager().getSession().getCurrentHistory().size() > 0) {
			int proceed = JOptionPane.showConfirmDialog(GUIManager.getManager()
					.getFrame(), "There are unsaved changes to this "
					+ "ontology. If you load this file, your "
					+ "changes will be discarded. Are you sure "
					+ "you want to proceed?", "Unsaved changes",
					JOptionPane.YES_NO_OPTION);
			if (proceed != JOptionPane.YES_OPTION)
				return;
		}

		OBOSession session = showLoadDialog();
		session.setCurrentUser(Preferences.getPreferences().getUserName());
		if (session != null) {
			SessionManager.getManager().setSession(session);
		}
	}

	public void saveAs() {
		if (!fireWillSaveRoot(new IOEvent(this)))
			return;

		byte condition = (byte) VerificationManager.SAVE;

		final CheckTask task = VerificationManager.getManager().getCheckTask(
				SessionManager.getManager().getSession(),
				(IdentifiedObject) null, condition);
		Runnable updateGUIRunnable = new Runnable() {
			public void run() {
				Collection<CheckWarning> c = task.getResults();
				if (c.size() > 0) {
					byte warningConditions = VerificationManager.getManager()
							.getWarningConditions();

					int fatalCount = VerificationManager.countFatal(c);
					int warningCount = c.size() - fatalCount;
					boolean showWarnings = VerificationManager
							.getConditionAtField(warningConditions,
									VerificationManager.SAVE);
					if (fatalCount > 0 || (warningCount > 0 && showWarnings)) {
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
							footer = "Proceed with save?";
						}
						warningComponent.setWarnings(c, header, footer, true,
								showWarnings, true);
						warningComponent.setPreferredSize(new Dimension(640,
								640));
						int dialogType = CheckWarningComponent.OK_OPTION;
						if (fatalCount == 0)
							dialogType = CheckWarningComponent.YES_NO_OPTION;
						int dialogVal = warningComponent.showDialog(GUIManager
								.getManager().getFrame(),
								(fatalCount > 0 ? "Fatal errors found"
										: "Warnings found"), dialogType);
						if (fatalCount > 0
								|| dialogVal != CheckWarningComponent.YES_VALUE)
							return;
					}
				}
				showSaveDialog();
			}
		};
		task.addPostExecuteRunnable(updateGUIRunnable);
		task.addCancelledRunnable(new Runnable() {

			public void run() {
				JOptionPane.showMessageDialog(GUIManager.getManager()
						.getFrame(), "Save cancelled");
			}

		});
		GUIManager.getManager().scheduleTask(task, true);
		/*
		 * Collection c = VerificationManager.getManager().runChecks(
		 * SessionManager.getManager().getSession(), (IdentifiedObject) null,
		 * VerificationManager.SAVE); byte warningConditions =
		 * VerificationManager.getManager() .getWarningConditions();
		 * 
		 * int fatalCount = VerificationManager.countFatal(c); int warningCount =
		 * c.size() - fatalCount; boolean showWarnings =
		 * VerificationManager.getConditionAtField( warningConditions,
		 * VerificationManager.SAVE); if (fatalCount > 0 || (warningCount > 0 &&
		 * showWarnings)) { CheckWarningComponent warningComponent = new
		 * CheckWarningComponent();
		 * 
		 * String header = null; String footer = null; if (fatalCount > 0) {
		 * header = "<p>Cannot save because of fatal errors. " + "These errors
		 * must be addressed before the save can " + "proceed. A copy of this
		 * list of errors - with hyperlinks " + "to the associated terms - can
		 * be found " + "on the \"Verification Results\" tab of the " +
		 * "Verification Plugin.</p>"; } else { header = "<p>Non-fatal
		 * warnings were found. These warnings " + "may be ignored and the save
		 * will continue normally. A " + "copy of this list of warnings - with
		 * hyperlinks " + " to the associated terms - can be found " + "on the
		 * \"Verification Results\" tab of the " + "Verification Plugin.</p>";
		 * footer = "Proceed with save?"; } warningComponent.setWarnings(c,
		 * header, footer, true, showWarnings, true);
		 * warningComponent.setPreferredSize(new Dimension(640, 640)); int
		 * dialogType = CheckWarningComponent.OK_OPTION; if (fatalCount == 0)
		 * dialogType = CheckWarningComponent.YES_NO_OPTION; int dialogVal =
		 * warningComponent.showDialog(GUIManager.getManager() .getFrame(),
		 * (fatalCount > 0 ? "Fatal errors found" : "Warnings found"),
		 * dialogType); if (fatalCount > 0 || dialogVal !=
		 * CheckWarningComponent.YES_VALUE) return; }
		 */
	}

	public OBOSession showSaveDialog() {
		try {
			DataAdapterRegistry registry = getAdapterRegistry();

			GraphicalAdapterChooser<OBOSession, OBOSession> gac = new GraphicalAdapterChooser<OBOSession, OBOSession>(
					registry, OBOEditAdapter.WRITE_ONTOLOGY, GUIManager
							.getManager().getScreenLockQueue(), GUIManager
							.getManager().getFrame(), Preferences
							.getPreferences().getUseModalProgressMonitors(),
					SessionManager.getManager().getSession());

			gac.setHistoryPath(Preferences.getPreferences()
					.getHistoryFilePath());
			boolean worked = gac.showDialog("Save ontology", null);
			if (worked) {
				OBOSession session = (OBOSession) gac.getResult();
				SessionManager.getManager().markChangesFlushed();
				return session;
			} else
				return null;
		} catch (Exception ex) {
			ex.printStackTrace();
			return null;
		}

	}

	public OBOSession showLoadDialog() {
		try {
			DataAdapterRegistry registry = getAdapterRegistry();
			GraphicalAdapterChooser<Void, OBOSession> gac = new GraphicalAdapterChooser<Void, OBOSession>(
					registry, OBOEditAdapter.READ_ONTOLOGY, GUIManager
							.getManager().getScreenLockQueue(), GUIManager
							.getManager().getFrame(), Preferences
							.getPreferences().getUseModalProgressMonitors(),
					null);

			gac.setHistoryPath(Preferences.getPreferences()
					.getHistoryFilePath());
			boolean worked = gac.showDialog("Load ontology", null);
			if (worked) {
				OBOSession session = (OBOSession) gac.getResult();
				return session;
			} else
				return null;
		} catch (Exception ex) {
			ex.printStackTrace();
			return null;
		}
	}
}
