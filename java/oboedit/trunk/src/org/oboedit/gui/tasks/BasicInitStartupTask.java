package org.oboedit.gui.tasks;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.Thread.UncaughtExceptionHandler;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import java.util.logging.StreamHandler;

import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.JToggleButton;

import net.infonode.docking.View;

import org.bbop.framework.ComponentManager;
import org.bbop.framework.GUIComponent;
import org.bbop.framework.GUIManager;
import org.bbop.framework.dock.LayoutDriver;
import org.bbop.framework.dock.idw.BitmapIcon;
import org.bbop.framework.dock.idw.IDWDriver;
import org.bbop.framework.dock.idw.IDWUtil;
import org.bbop.framework.dock.idw.ViewListener;
import org.bbop.io.LoggerStream;
import org.bbop.io.MultiOutputStream;
import org.bbop.swing.GhostImageController;
import org.obo.util.VersionNumber;
import org.oboedit.gui.AbstractSingleActionTask;
import org.oboedit.gui.ExceptionLogger;
import org.oboedit.gui.OBOEditFrame;
import org.oboedit.gui.ObjectSelector;
import org.oboedit.gui.Preferences;

public class BasicInitStartupTask extends AbstractSingleActionTask {

	public void run() {
		VersionNumber version = Preferences.getVersion();
		File prefsDir = new File(System.getProperty("user.home") + "/.oboedit"
				+ (version.isBeta() ? "beta" : "") + "/");
		GUIManager.setPrefsDir(prefsDir);
		configureLogging();
		/*
		 * SplashScreen splash = new SplashScreen(); splash.setSize(400, 400);
		 * SwingUtil.center(splash); splash.start();
		 */
	}
	
	
	protected void configureLogging() {
		LogManager.getLogManager().reset();
		final Logger global = Logger.getLogger("");
		try {
			Handler fh = new FileHandler(new File(GUIManager.getPrefsDir(),
					"oboedit-" + Preferences.getVersion() + "-%g%u.log")
					.getAbsolutePath(), 10485760, 1);
			fh.setLevel(Level.ALL);
			global.addHandler(fh);

			Handler eh = new StreamHandler(System.err, new SimpleFormatter());
			eh.setLevel(Level.WARNING);
			global.addHandler(eh);
		} catch (SecurityException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}

		MultiOutputStream stream = new MultiOutputStream();
		stream.addOutputStream(System.err);
		stream.addOutputStream(new LoggerStream(global, Level.INFO));
		System.setErr(new PrintStream(stream));
		Thread
				.setDefaultUncaughtExceptionHandler(new UncaughtExceptionHandler() {

					public void uncaughtException(Thread t, Throwable e) {
						Logger global = Logger.getLogger("");
						global.log(Level.SEVERE,
								"Uncaught event dispatch exception", e);
					}

				});
		System.setProperty("sun.awt.exception.handler", ExceptionLogger.class
				.getName());
	}


}
