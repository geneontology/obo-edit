package org.bbop.framework;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;

import org.bbop.framework.event.UserEvent;
import org.bbop.framework.event.UserListener;
import org.bbop.swing.BackgroundEventQueue;
import org.bbop.swing.ComponentPath;
import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;
import org.bbop.util.TaskDelegate;

public class GUIManager {

	protected static GUIManager manager;

	protected final static String PREFS_DIR_PROPERTY = "prefsdir";

	protected BackgroundEventQueue screenLockQueue;

	protected BackgroundEventQueue backgroundQueue;

	protected JFrame frame;

	protected boolean started = false;

	protected List<GUITask> activeTasks = new ArrayList<GUITask>();

	protected LinkedList<GUITask> startupTasks = new LinkedList<GUITask>();

	protected MultiMap<String, UserListener> userListeners = new MultiHashMap<String, UserListener>();

	protected static boolean confirmOnExit = true;

	protected static File prefsDir;

	public BackgroundEventQueue getScreenLockQueue() {
		return screenLockQueue;
	}

	public BackgroundEventQueue getBackgroundQueue() {
		return backgroundQueue;
	}

	public void scheduleTask(TaskDelegate<?> task, boolean lockScreen) {
		getQueue(lockScreen).scheduleTask(task);
	}

	protected BackgroundEventQueue getQueue(boolean lockScreen) {
		if (lockScreen)
			return screenLockQueue;
		else
			return backgroundQueue;
	}

	public void scheduleTasks(boolean lockScreen, TaskDelegate<?>... tasks) {
		getQueue(lockScreen).scheduleTasks(tasks);
	}

	public void scheduleDependentTasks(boolean lockScreen,
			TaskDelegate<?>... tasks) {
		getQueue(lockScreen).scheduleDependentTasks(tasks);
	}

	public GUIManager() {
		screenLockQueue = BackgroundEventQueue.getGlobalQueue();
		backgroundQueue = new BackgroundEventQueue();
	}

	public static GUIManager getManager() {
		if (manager == null) {
			manager = new GUIManager();
		}
		return manager;
	}

	public static void setManager(GUIManager manager) {
		GUIManager.manager = manager;
	}

	public void addUserListener(UserListener listener) {
		userListeners.add(listener.getEventType(), listener);
	}

	public void removeUserListener(UserListener listener) {
		userListeners.remove(listener.getEventType(), listener);
	}

	public void fireUserEvent(UserEvent event) {
		Collection<UserListener> listeners = userListeners.get(event.getType());
		for (UserListener listener : listeners) {
			listener.userEventOccurred(event);
		}
	}

	public void addStartupTask(GUITask task) {
		if (started)
			throw new IllegalStateException(
					"Cannot add a new startup task once the gui manager has started");
		startupTasks.add(task);
	}

	public void removeStartupTask(GUITask task) {
		if (started)
			throw new IllegalStateException(
					"Cannot remove a startup task once the gui manager has started");
		startupTasks.remove(task);
	}

	protected GUITask popTask() {
		return startupTasks.removeFirst();
	}

	public void installTask(GUITask task) {
		activeTasks.add(task);
		task.install();
	}

	public void notifyComplete(GUITask task) {
		task.shutdown();
		activeTasks.remove(task);
	}

	public JFrame getFrame() {
		return frame;
	}

	public void setFrame(JFrame frame) {
		this.frame = frame;
	}

	public boolean installMenuItem(String path, JMenuItem item) {
		return ComponentPath.addComponent(path, getFrame().getJMenuBar(), item);
	}

	public boolean uninstallMenuItem(String path, JMenuItem item) {
		return ComponentPath.removeComponent(path, getFrame().getJMenuBar(),
				item);
	}

	public void start() {
		addShutdownHook(new Runnable() {

			public void run() {
				shutdown();
			}
		});

		while (startupTasks.size() > 0) {
			GUITask startupTask = popTask();
			installTask(startupTask);
		}
		started = true;
	}

	public void shutdown() {
		List<GUITask> taskList = new LinkedList<GUITask>(activeTasks);
		for (GUITask task : taskList) {
			task.shutdown();
		}
		started = false;
	}

	public static final File readPrefsDir() {
		File prefsDir = null;
		if (System.getProperty(PREFS_DIR_PROPERTY) != null) {
			prefsDir = new File(System.getProperty(PREFS_DIR_PROPERTY));
			boolean worked = true;
			if (!prefsDir.exists())
				worked = prefsDir.mkdirs();
			if (!worked)
				prefsDir = null;
		}
		if (prefsDir == null) {
			prefsDir = new File(System.getProperty("user.home")
					+ "/.bbopframework");
			if (!prefsDir.exists())
				prefsDir.mkdirs();
		}

		return prefsDir;
	}

	public static File getPrefsDir() {
		if (prefsDir == null) {
			readPrefsDir();
		}
		return prefsDir;
	}

	public static void setPrefsDir(File dir) {
		prefsDir = dir;
		boolean worked = true;
		if (!prefsDir.exists())
			worked = prefsDir.mkdirs();
		if (!worked)
			prefsDir = null;
	}

	protected static List<Runnable> hooks = new LinkedList<Runnable>();

	public static void addShutdownHook(Runnable r) {
		hooks.add(r);
	}

	public static void removeShutdownHook(Runnable r) {
		hooks.remove(r);
	}

	public static void exit(final int status) {
		if (GUIManager.getManager().getFrame() != null && isConfirmOnExit()) {
			if (JOptionPane.showConfirmDialog(GUIManager.getManager()
					.getFrame(), "Really quit?", "Exit?",
					JOptionPane.YES_NO_OPTION) != JOptionPane.YES_OPTION)
				return;
		}

		for (Runnable r : hooks) {
			SwingUtilities.invokeLater(r);
		}
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				System.exit(status);
			}
		});
	}

	public static boolean isConfirmOnExit() {
		return confirmOnExit;
	}

	public static void setConfirmOnExit(boolean confirmOnExit) {
		GUIManager.confirmOnExit = confirmOnExit;
	}
}
