package org.bbop.framework;

import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.io.File;
import java.lang.Thread.UncaughtExceptionHandler;
import java.util.Collection;
import org.apache.log4j.*;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JToolBar;
import javax.swing.LookAndFeel;
import javax.swing.UIManager;

import org.bbop.dataadapter.DataAdapter;
import org.bbop.framework.dock.LayoutDriver;
import org.bbop.framework.dock.idw.IDWDriver;
import org.bbop.swing.SwingUtil;
import org.bbop.util.CollectionUtil;
import org.bbop.util.ExceptionLogger;
import org.simplericity.macify.eawt.Application;
import org.simplericity.macify.eawt.ApplicationEvent;
import org.simplericity.macify.eawt.ApplicationListener;
import org.simplericity.macify.eawt.DefaultApplication;

public abstract class AbstractApplicationStartupTask extends
AbstractSingleActionTask {

	protected VetoableShutdownListener vetoableShutdownListener = new VetoableShutdownListener() {

		public boolean willShutdown() {
			GUIManager.getManager();
			if (GUIManager.getManager().getFrame() != null
					&& GUIManager.isConfirmOnExit()) {
				return JOptionPane.showConfirmDialog(GUIManager.getManager()
						.getFrame(), "Really quit?", "Exit?",
						JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION;
			} else
				return true;
		}
	};

	protected abstract String getPerspectiveResourceDir();

	protected abstract Collection<GUITask> getDefaultTasks();

	protected abstract Collection<DataAdapter> getDefaultDataAdapters();

	protected abstract Collection<GUIComponentFactory<?>> getDefaultComponentFactories();

	protected abstract String getAppID();

	protected abstract String getAppName();

	protected File getPrefsDir() {
		return new File(System.getProperty("user.home") + "/." + getAppID());
	}

	protected String getDefaultPerspectiveResourcePath() {
		if (getPerspectiveResourceDir() != null)
			return getPerspectiveResourceDir() + "/default.idw";
		else
			return null;
	}

	protected String getPerspectiveListResourcePath() {
		if (getPerspectiveResourceDir() != null)
			return getPerspectiveResourceDir() + "/perspectives.xml";
		else
			return null;
	}

	/**
	 * This internal class fixes the issue on Macs where the MacOS adds a
	 * top-level menu for each application (such as OBO-Edit) that includes menu
	 * items such as "About" and "Quit" (shortcut command-Q). If the Mac user
	 * selected Quit from that top-level menu (or typed command-Q), OBO-Edit
	 * would instantly quit without going through the standard OBO-Edit exit
	 * method (asking "Really quit?"). This class uses an Open Source library,
	 * org.simplericity.macify.eawt, that lets us control that top-level menu on
	 * Macs without requiring any Mac-specific code that would cause runtime or
	 * compile-time problems on non-Macs.
	 */
	public class MacApplicationSupport implements ApplicationListener {
		private Application application = new DefaultApplication();

		public void handleAbout(ApplicationEvent event) {
			Action aboutAction = getAboutAction();
			if (aboutAction != null) {
				aboutAction.actionPerformed(new ActionEvent(event.getSource(),
						0, "about"));
				event.setHandled(true);
			}
		}

		public void handleQuit(ApplicationEvent event) {
			Action exitAction = getExitAction();
			if (exitAction != null) {
				exitAction.actionPerformed(new ActionEvent(event.getSource(),
						0, "exit"));
			} else
				GUIManager.exit(0);
		}

		/*
		 * These have to be declared even if we're not going to do anything with
		 * them
		 */
		public void handleOpenApplication(ApplicationEvent event) {
		}

		public void handleOpenFile(ApplicationEvent event) {
			Action openAction = getOpenFileAction(event.getFilename());
			if (openAction != null) {
				openAction.actionPerformed(new ActionEvent(event.getSource(),
						0, "openFile"));
				event.setHandled(true);			
			}
		}

		public void handlePreferences(ApplicationEvent event) {
			Action prefsAction = getPreferencesAction();
			if (prefsAction != null) {
				prefsAction.actionPerformed(new ActionEvent(event.getSource(),
						0, "about"));
				event.setHandled(true);
			}
		}

		public void handlePrintFile(ApplicationEvent event) {
		}

		public void handleReopenApplication(ApplicationEvent event) {
		}
	}

	protected Action getOpenFileAction(String filename) {
		return null;
	}

	protected Action getAboutAction() {
		return null;
	}

	protected Action getExitAction() {
		return null;
	}

	protected Action getPreferencesAction() {
		return null;
	}

	public void run() {
		GUIManager.setPrefsDir(getPrefsDir());
		PluginManager.getManager().setPluginDirs(getPluginDirs());
		configureLogging();
		installPlugins();
		configureUI();
		configureSystem();
		GUIManager.getManager().setFrame(createFrame());
		doPreInstallation();
		installSystemListeners();
		installDefaultDataAdapters();
		installDefaultComponentFactories();
		installDefaultTasks();
		installMenus();
		installDefaultToolBars();
		doOtherInstallations();
		ComponentManager.getManager().setDriver(createLayoutDriver());

		showFrame();
	}

	protected void configureSystem() {
		Application application = new DefaultApplication();
		application.setEnabledAboutMenu(getAboutAction() != null);
		application.setEnabledPreferencesMenu(getPreferencesAction() != null);
		application.addApplicationListener(new MacApplicationSupport());
	}

	protected void installSystemListeners() {
		GUIManager.addVetoableShutdownListener(vetoableShutdownListener);
	}

	protected void doPreInstallation() {
	}

	protected void installDefaultTasks() {
		for (GUITask task : getDefaultTasks()) {
			getManager().installTask(task);
		}
	}

	protected void installDefaultComponentFactories() {
		for (GUIComponentFactory<?> factory : getDefaultComponentFactories()) {
			ComponentManager.getManager().install(factory);
		}
	}

	protected void installMenus() {
		for (JMenuItem menu : getDefaultMenus()) {
			GUIManager.getManager().installMenuItem(null, menu);
		}
	}

	protected void installDefaultToolBars() {
		for (JToolBar toolbar : getDefaultToolBars()) {
			GUIManager.getManager().installToolBar(toolbar);
		}
	}

	protected Collection<? extends JMenuItem> getDefaultMenus() {
		return CollectionUtil.list(new ViewMenu());
	}

	protected Collection<JToolBar> getDefaultToolBars() {
		return null;
	}

	protected void installDefaultDataAdapters() {
		for (DataAdapter adapter : getDefaultDataAdapters()) {
			IOManager.getManager().getAdapterRegistry().addAdapter(adapter);
		}
	}

	protected void doOtherInstallations() {
	}

	protected Color getLightColor() {
		return null;
	}

	protected Font getFont() {
		return null;
	}

	protected LayoutDriver createLayoutDriver() {
		IDWDriver driver = new IDWDriver();
		driver
		.setDefaultPerspectiveResourcePath(getDefaultPerspectiveResourcePath());
		driver.setPerspectiveResourceDir(getPerspectiveResourceDir());
		driver.setPerspectiveListResourcePath(getPerspectiveListResourcePath());
		if (getBackgroundColor() != null)
			driver.setBackground(getBackgroundColor());
		if (getButtonColor() != null)
			driver.setDarkColor(getButtonColor());
		if (getLightColor() != null)
			driver.setLightColor(getLightColor());
		if (getFont() != null)
			driver.setFont(getFont());
		return driver;
	}

	private int[][] dimensionInfo = { { 620, 460, 160 }, { 760, 560, 300 },
			{ 960, 700, 400 } };

	protected void showFrame() {
		JFrame frame = getManager().getFrame();
		frame.setVisible(true);
		SwingUtil.center(frame);
	}

	protected JFrame createFrame() {
		JFrame out = new MainFrame(getAppID());
		out.setTitle(getAppName());
		return out;
	}

	protected Color getButtonColor() {
		return null;
	}

	protected Color getBackgroundColor() {
		return null;
	}

	protected void configureUI() {
		LookAndFeel lf = UIManager.getLookAndFeel();
		if (!lf.getName().startsWith("Mac OS X Aqua")) {
			if (getButtonColor() != null) {
				UIManager.put("ComboBox.selectionBackground", getButtonColor());
				UIManager.put("ComboBox.buttonBackground", getButtonColor());
				UIManager.put("Button.background", getButtonColor());
				UIManager.put("ToggleButton.background", getButtonColor());
			}
			if (getBackgroundColor() != null) {
				UIManager.put("Panel.background", getBackgroundColor());
				UIManager.put("Label.background", getBackgroundColor());
				UIManager.put("OptionPane.background", getBackgroundColor());
				UIManager.put("ScrollPane.background", getBackgroundColor());
				UIManager.put("SplitPane.background", getBackgroundColor());
				UIManager.put("TabbedPane.background", getBackgroundColor());
				UIManager.put("ToolBar.background", getBackgroundColor());
				UIManager
				.put("ToolBar.dockingBackground", getBackgroundColor());
				UIManager.put("ToolBar.floatingBackground",
						getBackgroundColor());
				UIManager.put("ToolBar.viewportBackground",
						getBackgroundColor());
			}
		}

	}

	protected void installPlugins() {
		for (GUITask task : PluginManager.getManager().instantiateAll(
				GUITask.class)) {
			GUIManager.getManager().addStartupTask(task);
		}
		for (GUIComponentFactory<?> factory : PluginManager.getManager()
				.instantiateAll(GUIComponentFactory.class)) {
			ComponentManager.getManager().install(factory);
		}
		if (IOManager.getManager().getAdapterRegistry() != null) {
			for (DataAdapter adapter : PluginManager.getManager()
					.instantiateAll(DataAdapter.class)) {
				IOManager.getManager().getAdapterRegistry().addAdapter(adapter);
			}
		}
	}

	protected File[] getPluginDirs() {
		File[] out = { new File(GUIManager.getPrefsDir(), "extensions") };
		return out;
	}

	protected void configureLogging() {
		
		final Logger logger = Logger.getLogger("");
		PropertyConfigurator.configure("log4j.properties");
		
		logger.info("Configured logging output destinations");
		logger.info("Reset standard error stream");
		logger.info("Testing raw log stream");
		logger.info("Testing standard error stream");

		Thread
		.setDefaultUncaughtExceptionHandler(new UncaughtExceptionHandler() {

			public void uncaughtException(Thread t, Throwable e) {
				Logger global = Logger.getLogger("");
				global.log(Level.FATAL,
						"Uncaught event dispatch exception", e);
			}
		});
		System.setProperty("sun.awt.exception.handler", ExceptionLogger.class
				.getName());
		logger.info("Configured final exception handlers");
	}

}
