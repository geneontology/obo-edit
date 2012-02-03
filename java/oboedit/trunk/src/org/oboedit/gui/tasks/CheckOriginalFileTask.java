package org.oboedit.gui.tasks;

import java.io.File;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.Timer;
import javax.swing.JOptionPane;
import org.apache.log4j.*;

import org.bbop.framework.GUIManager;
import org.bbop.framework.GUITask;
import org.bbop.util.RunnableTimerTask;
import org.obo.datamodel.OBOSession;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.event.ReconfigEvent;
import org.oboedit.gui.event.ReconfigListener;
import org.oboedit.gui.menu.FileMenu;

public class CheckOriginalFileTask implements GUITask, Runnable {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CheckOriginalFileTask.class);

	protected Timer timer;

        protected File lastLoadedOrSaved;
        protected long lastChanged = 0;

	protected ReconfigListener listener = new ReconfigListener() {
                // This is triggered when the user saves to a file--this is how we can tell
                // that it was the OE user who changed the file rather than the file
                // changing on disk.
		public void configReloaded(ReconfigEvent e) {
		    //                    logger.debug("CheckOrig: configReloaded " + e); // DEL
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
		timer = new Timer("chedkforupdates", true);
                lastChanged = 0;
                // Check every 5 seconds
		timer.schedule(new RunnableTimerTask(this), 0, 5000);
	}

	public void run() {
	    OBOSession session = SessionManager.getManager().getSession();
	    if (session != null) {
                File file = null;
                Collection filenames = session.getCurrentFilenames();
                if (filenames == null)
                    return;

                for (Object filename : filenames) {
                    file = new File((String)filename);
                    break;
                }
                if (file == null)
                    return;
                if (fileChangedOnDisk(file)) {
                    String message = "<html>File " + lastLoadedOrSaved + " has changed on disk<br> since you last loaded or saved.<br>" +
                        (SessionManager.getManager().needsSave() ? "<br>You may want to reload it. However, you have made changes since you loaded,<br>so be sure to save your work before you reload." :
                         "You may want to reload the updated version, or save your current session<br>as a new file and resolve differences manually.");
                    JOptionPane.showMessageDialog(GUIManager.getManager().getFrame(),
                                                  message, "File changed on disk", JOptionPane.WARNING_MESSAGE);
                }
                lastLoadedOrSaved = file;
                lastChanged = file.lastModified();
                //		if (file != null)
                    //		    logger.debug("CheckOriginalFile: file = " + file + ", last modified = " + file.lastModified());
	    }
	}

    /** Returns true if file has changed on disk since last checked */
    private boolean fileChangedOnDisk(File latest) {
        if (latest == null || lastLoadedOrSaved == null)
            return false;

        if (latest.getAbsolutePath().indexOf(".autosave") >= 0) {
            //            logger.debug("fileChanged: autosave"); // DEL
            // Don't change the last-loaded-or-saved filename if it was just an autosave.
            return false;
        }

        if (!(latest.getAbsolutePath().equals(lastLoadedOrSaved.getAbsolutePath()))) {
            logger.debug("fileChanged: filename changed from " + lastLoadedOrSaved + " to " + latest);
            lastChanged = 0; // So it won't be reported as changed next time we check
            return false; // 
        }
        if (lastChanged == 0)
            return false;
        if (latest.lastModified() != lastChanged) {
            logger.debug("fileChanged: " + latest + " changed on disk! Before = " + lastChanged + ", now = " + latest.lastModified());
            return true;
        }
        return false;
    }

}
