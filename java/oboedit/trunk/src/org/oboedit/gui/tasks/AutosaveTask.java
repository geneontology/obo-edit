package org.oboedit.gui.tasks;

import java.io.File;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Timer;
import java.util.TimerTask;

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

import org.apache.log4j.*;

public class AutosaveTask implements GUITask, Runnable {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(AutosaveTask.class);

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
		final Preferences preferences = Preferences.getPreferences();
		final GregorianCalendar calendar = new GregorianCalendar();
		if (preferences.getAutosaveExpirationDays() > 0) {
			GregorianCalendar expireFileCalendar = new GregorianCalendar();
			File[] files = preferences.getAutosavePath().listFiles();
			for (int i = 0; files != null && i < files.length; i++) {
				expireFileCalendar.setTime(new Date(files[i].lastModified()));
				expireFileCalendar.add(Calendar.DAY_OF_MONTH, preferences
						.getAutosaveExpirationDays());
				if (calendar.after(expireFileCalendar)) {
					logger.log(Level.INFO, "Deleting autosave file " + files[i]
							+ "...");
					if (!files[i].delete())
						logger.warn("Couldn't delete expired "
								+ "autosave file " + files[i]);
				}
			}
		}
		if (preferences.getAutosaveEnabled()) {
			final OBOFileAdapter adapter = new OBOFileAdapter();
			if (!preferences.getAutosavePath().exists()) {
				if (!preferences.getAutosavePath().mkdirs()) {
					logger.warn("Couldn't create " + "autosave "
						       + "directory " +
						       preferences.getAutosavePath() + "\nAutosave " + "disabled.");
					GUIManager.getManager().notifyComplete(this);
					return;
				}
			}
			// Only do the autosave if there are unsaved changes.
			if (!SessionManager.getManager().needsSave()) {
//			    logger.info("No changes--no need to autosave.");
			    return;
			}
			String saveFile = new File(preferences.getAutosavePath(),
					getFileName(calendar)).toString();
			logger.info("Autosaving backup file " + saveFile + " at " + (new Date()));
			final FileAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
			config.setWritePath(saveFile);
			DataAdapterOperationTask task = new DataAdapterOperationTask(
					adapter, adapter.WRITE_ONTOLOGY, config, SessionManager
							.getManager().getSession());
//			GUIManager.getManager().scheduleTask(task, true);
			// false means *don't* grab focus
			GUIManager.getManager().scheduleTask(task, false);
		}
	}
}
