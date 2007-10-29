package org.bbop.framework;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.Thread.UncaughtExceptionHandler;
import java.util.Collection;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import java.util.logging.StreamHandler;

import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JToggleButton;
import javax.swing.LookAndFeel;
import javax.swing.UIManager;

import net.infonode.docking.View;

import org.bbop.dataadapter.DataAdapter;
import org.bbop.dataadapter.DataAdapterRegistry;
import org.bbop.framework.dock.LayoutDriver;
import org.bbop.framework.dock.idw.BitmapIcon;
import org.bbop.framework.dock.idw.IDWDriver;
import org.bbop.framework.dock.idw.IDWUtil;
import org.bbop.framework.dock.idw.ViewListener;
import org.bbop.io.LoggerStream;
import org.bbop.io.MultiOutputStream;
import org.bbop.swing.ComponentFactory;
import org.bbop.swing.EnhancedMenuBar;
import org.bbop.swing.GhostImageController;
import org.bbop.swing.SwingUtil;
import org.bbop.util.CollectionUtil;
import org.bbop.util.ExceptionLogger;

public abstract class AbstractApplicationStartupTask extends
		AbstractSingleActionTask {
	protected abstract String getPerspectiveResourceDir();

	protected abstract Collection<GUITask> getDefaultTasks();

	protected abstract Collection<DataAdapter> getDefaultDataAdapters();

	protected abstract Collection<GUIComponentFactory<?>> getDefaultComponentFactories();

	protected abstract DataAdapterRegistry getAdapterRegistry();

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

	public void run() {
		GUIManager.setPrefsDir(getPrefsDir());
		PluginManager.getManager().setPluginDirs(getPluginDirs());
		configureLogging();
		installPlugins();
		configureUI();
		GUIManager.getManager().setFrame(createFrame());
		installDefaultDataAdapters();
		installDefaultComponentFactories();
		installDefaultTasks();
		installMenus();
		doOtherInstallations();
		ComponentManager.getManager().setDriver(createLayoutDriver());

		showFrame();

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

	protected Collection<? extends JMenuItem> getDefaultMenus() {
		return CollectionUtil.list(new ViewMenu());
	}

	protected void installDefaultDataAdapters() {
		for (DataAdapter adapter : getDefaultDataAdapters()) {
			getAdapterRegistry().addAdapter(adapter);
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
		if (getAdapterRegistry() != null) {
			for (DataAdapter adapter : PluginManager.getManager()
					.instantiateAll(DataAdapter.class)) {
				getAdapterRegistry().addAdapter(adapter);
			}
		}
	}

	protected File[] getPluginDirs() {
		File[] out = { new File(GUIManager.getPrefsDir(), "." + getAppID()) };
		return out;
	}

	protected void configureLogging() {
		LogManager.getLogManager().reset();
		final Logger global = Logger.getLogger("");
		try {
			Handler fh = new FileHandler(new File(GUIManager.getPrefsDir(),
					getAppID() + "-%g%u.log").getAbsolutePath(), 10485760, 1);
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
