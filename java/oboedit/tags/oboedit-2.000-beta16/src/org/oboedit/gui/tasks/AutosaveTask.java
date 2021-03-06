package org.oboedit.gui.tasks;

import java.io.File;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Timer;
import java.util.TimerTask;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.bbop.dataadapter.DataAdapterException;
import org.bbop.dataadapter.DataAdapterOperationTask;
import org.bbop.dataadapter.FileAdapterConfiguration;
import org.bbop.dataadapter.IOOperation;
import org.bbop.framework.GUIManager;
import org.bbop.framework.GUITask;
import org.bbop.swing.ModalProgressMonitor;
import org.bbop.util.RunnableTimerTask;
import org.obo.dataadapter.OBOFileAdapter;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.event.ReconfigEvent;
import org.oboedit.gui.event.ReconfigListener;

public class AutosaveTask implements GUITask, Runnable {

	protected Timer timer;

	protected ReconfigListener listener = new ReconfigListener() {
		public void configReloaded(ReconfigEvent e) {
			restart();
		}
	};

	public void install() {
		Preferences.getPreferences().addReconfigListener(listener);
		restart();
	}

	public void shutdown() {
		Preferences.getPreferences().removeReconfigListener(listener);
		doShutdown();
	}

	protected void doShutdown() {
		if (timer != null)
			timer.cancel();
		timer = null;
	}

	public void restart() {
		if (timer != null)
			doShutdown();
		if (Preferences.getPreferences().getAutosaveEnabled()) {
			timer = new Timer("autosave", true);
			timer.schedule(new RunnableTimerTask(this), Preferences
					.getPreferences().getAutosaveWaitTime() * 60000,
					Preferences.getPreferences().getAutosaveWaitTime() * 60000);
		}
	}

	protected static String getFileName(Calendar calendar) {
		return "oboedit-save-" + calendar.get(Calendar.DAY_OF_MONTH) + "-"
				+ calendar.get(Calendar.MONTH) + "-"
				+ calendar.get(Calendar.YEAR) + "-"
				+ calendar.get(Calendar.HOUR_OF_DAY) + "-"
				+ calendar.get(Calendar.MINUTE) + ".autosave.obo";
	}

	public void run() {
		final Logger logger = Logger
				.getLogger("org.geneontology.oboedit.autosave");
		final Preferences preferences = Preferences.getPreferences();
		final GregorianCalendar calendar = new GregorianCalendar();
		if (preferences.getAutosaveExpirationDays() > 0) {
			GregorianCalendar expireFileCalendar = new GregorianCalendar();
			File[] files = preferences.getAutosavePath().listFiles();
			for (int i = 0; i < files.length; i++) {
				expireFileCalendar.setTime(new Date(files[i].lastModified()));
				expireFileCalendar.add(Calendar.DAY_OF_MONTH, preferences
						.getAutosaveExpirationDays());
				if (calendar.after(expireFileCalendar)) {
					logger.log(Level.INFO, "Deleting autosave file " + files[i]
							+ "...");
					if (!files[i].delete())
						logger.warning("Couldn't delete expired "
								+ "autosave file " + files[i]);
				}
			}
		}
		if (preferences.getAutosaveEnabled()) {
			final OBOFileAdapter adapter = new OBOFileAdapter();
			if (!preferences.getAutosavePath().exists()) {
				if (!preferences.getAutosavePath().mkdirs()) {
					logger.warning("Couldn't create " + "autosave "
							+ "directory. " + "Autosave " + "disabled.");
					GUIManager.getManager().notifyComplete(this);
					return;
				}
			}
			String saveFile = new File(preferences.getAutosavePath(),
					getFileName(calendar)).toString();
			final FileAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
			config.setWritePath(saveFile);
			DataAdapterOperationTask task = new DataAdapterOperationTask(
					adapter, adapter.WRITE_ONTOLOGY, config, SessionManager
							.getManager().getSession());
			GUIManager.getManager().scheduleTask(task, true);
		}
	}
}
