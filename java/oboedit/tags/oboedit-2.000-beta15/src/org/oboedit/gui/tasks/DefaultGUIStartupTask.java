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
import javax.swing.JFrame;
import javax.swing.JToggleButton;
import javax.swing.LookAndFeel;
import javax.swing.UIManager;

import net.infonode.docking.View;

import org.bbop.expression.ExpressionException;
import org.bbop.framework.ComponentManager;
import org.bbop.framework.DockPanelFactory;
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
import org.bbop.swing.SwingUtil;
import org.obo.dataadapter.GOFlatFileAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.dataadapter.OWLURLReaderAdapter;
import org.obo.dataadapter.SerialAdapter;
import org.obo.dataadapter.XMLHistoryAdapter;
import org.obo.identifier.DefaultIDGenerator;
import org.obo.identifier.IDGenerator;
import org.obo.reasoner.impl.LinkPileReasoner;
import org.obo.reasoner.impl.LinkPileReasonerFactory;
import org.obo.util.VersionNumber;
import org.oboedit.controller.EditActionManager;
import org.oboedit.controller.ExpressionManager;
import org.oboedit.controller.FocusMenuManager;
import org.oboedit.controller.IDManager;
import org.oboedit.controller.IOManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.AbstractSingleActionTask;
import org.oboedit.gui.AdvancedOBOUI;
import org.oboedit.gui.DefaultInputHandler;
import org.oboedit.gui.ExceptionLogger;
import org.oboedit.gui.GOFlatFileGUI;
import org.oboedit.gui.OBOEditFrame;
import org.oboedit.gui.ObjectSelector;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.actions.AddAction;
import org.oboedit.gui.actions.AddConsiderAction;
import org.oboedit.gui.actions.AddParentAction;
import org.oboedit.gui.actions.AddReplacementAction;
import org.oboedit.gui.actions.AddRootAction;
import org.oboedit.gui.actions.ApplyFilterAction;
import org.oboedit.gui.actions.AssertImpliedAction;
import org.oboedit.gui.actions.CloneAction;
import org.oboedit.gui.actions.CompletesAction;
import org.oboedit.gui.actions.CopyAction;
import org.oboedit.gui.actions.DeleteAction;
import org.oboedit.gui.actions.DomainChangeAction;
import org.oboedit.gui.actions.InvNecessaryAction;
import org.oboedit.gui.actions.MergeAction;
import org.oboedit.gui.actions.MoveAction;
import org.oboedit.gui.actions.MultiAddAction;
import org.oboedit.gui.actions.NecessaryAction;
import org.oboedit.gui.actions.RangeChangeAction;
import org.oboedit.gui.actions.RemoveConsiderAction;
import org.oboedit.gui.actions.RemoveRedundantAction;
import org.oboedit.gui.actions.RemoveReplacementAction;
import org.oboedit.gui.actions.RerootAction;
import org.oboedit.gui.actions.TypeChangeAction;
import org.oboedit.gui.event.ReconfigEvent;
import org.oboedit.gui.event.ReconfigListener;
import org.oboedit.gui.factory.AnnotationSummaryComponentFactory;
import org.oboedit.gui.factory.CategoryManagerFactory;
import org.oboedit.gui.factory.ConfigurationManagerFactory;
import org.oboedit.gui.factory.CrossProductInfoFactory;
import org.oboedit.gui.factory.CrossProductMatrixEditorFactory;
import org.oboedit.gui.factory.DAGViewFactory;
import org.oboedit.gui.factory.DbxrefLibraryFactory;
import org.oboedit.gui.factory.ExplanationComponentFactory;
import org.oboedit.gui.factory.ExtendedInfoFactory;
import org.oboedit.gui.factory.GlobalFilterManagerFactory;
import org.oboedit.gui.factory.GraphDAGViewFactory;
import org.oboedit.gui.factory.GraphEditorFactory;
import org.oboedit.gui.factory.HistoryBrowserFactory;
import org.oboedit.gui.factory.IDManagerFactory;
import org.oboedit.gui.factory.IntersectionEditorFactory;
import org.oboedit.gui.factory.NamespaceManagerFactory;
import org.oboedit.gui.factory.OntologyChangeTrackerFactory;
import org.oboedit.gui.factory.ParentEditorFactory;
import org.oboedit.gui.factory.ReasonerManagerFactory;
import org.oboedit.gui.factory.SearchComponentFactory;
import org.oboedit.gui.factory.SynonymCategoryManagerFactory;
import org.oboedit.gui.factory.TermPanelFactory;
import org.oboedit.gui.factory.TextEditorFactory;
import org.oboedit.gui.factory.VerificationManagerFactory;
import org.oboedit.script.GUIScriptDelegate;

public class DefaultGUIStartupTask extends AbstractSingleActionTask {

	public void installDefaultTasks() {
		getManager().installTask(new AutosaveTask());
		getManager().installTask(new PostLoadVerifyTask());
		getManager()
				.installTask(
						new ScreenLockTask(GUIManager.getManager()
								.getScreenLockQueue(), GUIManager.getManager()
								.getFrame(), Preferences.getPreferences()
								.getUseModalProgressMonitors()));
	}

	protected void installDefaultComponentFactories() {
		ComponentManager.getManager().install(new TermPanelFactory());
		ComponentManager.getManager().install(new GraphEditorFactory());
		ComponentManager.getManager().install(new TextEditorFactory());
		ComponentManager.getManager().install(new DAGViewFactory());
		ComponentManager.getManager().install(new GraphDAGViewFactory());
		ComponentManager.getManager().install(new SearchComponentFactory());
		ComponentManager.getManager().install(new IntersectionEditorFactory());
		ComponentManager.getManager().install(new CategoryManagerFactory());
		ComponentManager.getManager().install(
				new SynonymCategoryManagerFactory());
		ComponentManager.getManager().install(new CrossProductInfoFactory());
		ComponentManager.getManager().install(new DbxrefLibraryFactory());
		ComponentManager.getManager().install(new ExtendedInfoFactory());
		// ComponentManager.getManager().install(new GraphVizViewerFactory());
		ComponentManager.getManager().install(new HistoryBrowserFactory());
		ComponentManager.getManager().install(new IDManagerFactory());
		ComponentManager.getManager().install(new ReasonerManagerFactory());
		ComponentManager.getManager().install(new NamespaceManagerFactory());
		ComponentManager.getManager().install(
				new OntologyChangeTrackerFactory());
		ComponentManager.getManager().install(new ParentEditorFactory());
		ComponentManager.getManager().install(
				new CrossProductMatrixEditorFactory());

		ComponentManager.getManager().install(
				new AnnotationSummaryComponentFactory());

		ComponentManager.getManager().install(new GlobalFilterManagerFactory());
		ComponentManager.getManager()
				.install(new ExplanationComponentFactory());
		ComponentManager.getManager()
				.install(new ConfigurationManagerFactory());
		ComponentManager.getManager().install(new VerificationManagerFactory());
		ComponentManager.getManager().install(new DockPanelFactory());
	}

	public void run() {
		SessionManager.getManager().setReasonerFactory(new LinkPileReasonerFactory());
		configureLogging();
		configureUI();
		/*
		 * SplashScreen splash = new SplashScreen(); splash.setSize(400, 400);
		 * SwingUtil.center(splash); splash.start();
		 */
		VersionNumber version = Preferences.getVersion();
		File prefsDir = new File(System.getProperty("user.home") + "/.oboedit"
				+ (version.isBeta() ? "beta" : "") + "/");
		GUIManager.setPrefsDir(prefsDir);

		OBOEditFrame frame = new OBOEditFrame();
		GUIManager.getManager().setFrame(frame);
		FocusMenuManager.install();
		installDefaultDataAdapters();
		installDefaultComponentFactories();
		installDefaultTasks();
		installDefaultActions();
		installGlobalScriptObjects();
		ComponentManager.getManager().setDriver(createLayoutDriver());
		GhostImageController.enable();
		showFrame();
		// ÊRepaintManager.setCurrentManager(new
		// CheckThreadViolationRepaintManager());
		/*
		 * splash.join(); splash.dispose();
		 */
	}

	protected LayoutDriver createLayoutDriver() {
		IDWDriver driver = new IDWDriver();
		driver
				.setDefaultPerspectiveResourcePath("org/oboedit/gui/dock/resources/"
						+ "default.idw");
		driver.setPerspectiveResourceDir("org/oboedit/gui/dock/resources");
		driver.setPerspectiveListResourcePath("org/oboedit/gui/dock/resources/"
				+ "perspectives.xml");
		driver.setBackground(Preferences.defaultBackgroundColor());
		driver.setDarkColor(Preferences.defaultButtonColor());
		driver.setLightColor(Color.white);
		driver.setFont(Preferences.getPreferences().getFont());
		driver.addViewListener(new ViewListener() {
			protected Icon globeIcon = new BitmapIcon(Preferences
					.loadLibraryImage("tiny_globe_icon.gif"));

			protected Icon houseIcon = new BitmapIcon(Preferences
					.loadLibraryImage("tiny_house_icon.gif"));

			protected Icon filterIcon = new BitmapIcon(Preferences
					.loadLibraryImage("tiny_filter_icon.gif"));

			public void viewCreated(View v, final GUIComponent c) {
				if (c instanceof ObjectSelector) {
					final JToggleButton liveButton = IDWUtil
							.createFlatHighlightToggleButton(
									// final JButton custom =
									// ButtonFactory.createFlatHighlightButton(
									globeIcon,
									"Click to switch to local selection mode",
									0, null);
					liveButton.setSelected(((ObjectSelector) c).isLive());
					if (!((ObjectSelector) c).isLive()) {
						liveButton.setIcon(houseIcon);
					}
					liveButton.addActionListener(new AbstractAction() {
						public void actionPerformed(ActionEvent e) {
							((ObjectSelector) c).setLive(liveButton
									.isSelected());
						}
					});
					liveButton.addActionListener(new ActionListener() {

						public void actionPerformed(ActionEvent e) {
							if (liveButton.isSelected()) {
								liveButton.setIcon(globeIcon);
								liveButton
										.setToolTipText("Click to switch to local selection mode");
							} else {
								liveButton.setIcon(houseIcon);
								liveButton
										.setToolTipText("Click to switch to global selection mode");
							}
						}
					});

					v.getCustomTitleBarComponents().add(liveButton);
				}
			}

			public void viewDestroyed(View v, GUIComponent c) {
			}

		});
		return driver;
	}

	protected void installDefaultDataAdapters() {
		final OBOFileAdapter oboadapter = new OBOFileAdapter();
		oboadapter.setAdvancedUI(new AdvancedOBOUI());
		final GOFlatFileAdapter goadapter = new GOFlatFileAdapter();
		goadapter.setAdvancedUI(new GOFlatFileGUI());
		Preferences.getPreferences().addReconfigListener(
				new ReconfigListener() {

					public void configReloaded(ReconfigEvent e) {
						goadapter.setUserName(Preferences.getPreferences()
								.getUserName());
						oboadapter.setUserName(Preferences.getPreferences()
								.getUserName());
						IDGenerator idGen = IDManager.getManager()
								.getIDAdapter();
						if (idGen instanceof DefaultIDGenerator)
							oboadapter
									.setIDProfile(((DefaultIDGenerator) idGen)
											.getProfile());
					}
				});
		goadapter.setAutogenString("OBO-Edit "
				+ Preferences.getVersion().toString());
		oboadapter.setAutogenString("OBO-Edit "
				+ Preferences.getVersion().toString());

		IOManager.getManager().installDataAdapter(oboadapter);
		IOManager.getManager().installDataAdapter(new OWLURLReaderAdapter());
		IOManager.getManager().installDataAdapter(goadapter);
		IOManager.getManager().installDataAdapter(new SerialAdapter());
		IOManager.getManager().installDataAdapter(new XMLHistoryAdapter());
	}

	protected void installDefaultActions() {
		installDefaultDropMenuActions();
		installDefaultEditActions();
		installDefaultInputHandlers();
	}

	protected void installDefaultInputHandlers() {
		DefaultInputHandler defaultInputHandler = new DefaultInputHandler();
		EditActionManager.getManager().addInputHandler(new CopyAction());
		EditActionManager.getManager().addInputHandler(new AddParentAction());
		EditActionManager.getManager().addInputHandler(new MoveAction());
		EditActionManager.getManager().addInputHandler(new MergeAction());
		EditActionManager.getManager().addInputHandler(new TypeChangeAction());
		EditActionManager.getManager().addInputHandler(new ApplyFilterAction());
		EditActionManager.getManager().addInputHandler(defaultInputHandler);
	}

	protected void installDefaultDropMenuActions() {
		EditActionManager.getManager().addDropMenuAction(new CopyAction());
		EditActionManager.getManager().addDropMenuAction(new AddParentAction());
		EditActionManager.getManager().addDropMenuAction(new MoveAction());
		EditActionManager.getManager().addDropMenuAction(new MergeAction());
		EditActionManager.getManager()
				.addDropMenuAction(new TypeChangeAction());
		EditActionManager.getManager().addDropMenuAction(
				new DomainChangeAction());
		EditActionManager.getManager().addDropMenuAction(
				new RangeChangeAction());
		EditActionManager.getManager().addDropMenuAction(
				new AddConsiderAction());
		EditActionManager.getManager().addDropMenuAction(
				new AddReplacementAction());
	}

	protected void installDefaultEditActions() {
		EditActionManager.getManager().addEditAction(new MoveAction());
		EditActionManager.getManager().addEditAction(new CopyAction());
		EditActionManager.getManager().addEditAction(new AddParentAction());
		EditActionManager.getManager().addEditAction(new MergeAction());
		EditActionManager.getManager().addEditAction(new TypeChangeAction());
		EditActionManager.getManager().addEditAction(new AddAction());
		EditActionManager.getManager().addEditAction(new MultiAddAction());
		EditActionManager.getManager().addEditAction(new DeleteAction(false));
		EditActionManager.getManager().addEditAction(new DeleteAction(true));
		EditActionManager.getManager().addEditAction(
				new RemoveRedundantAction());
		EditActionManager.getManager().addEditAction(new AssertImpliedAction());

		EditActionManager.getManager().addEditAction(new CloneAction());
		EditActionManager.getManager().addEditAction(new AddRootAction());
		EditActionManager.getManager().addEditAction(new RerootAction());
		EditActionManager.getManager().addEditAction(new NecessaryAction());
		EditActionManager.getManager().addEditAction(new InvNecessaryAction());
		EditActionManager.getManager().addEditAction(new CompletesAction());
		EditActionManager.getManager()
				.addEditAction(new RemoveConsiderAction());
		EditActionManager.getManager().addEditAction(
				new RemoveReplacementAction());
	}

	protected void configureUI() {
		LookAndFeel lf = UIManager.getLookAndFeel();
		if (!lf.getName().startsWith("Mac OS X Aqua")) {
			UIManager.put("ComboBox.selectionBackground", Preferences
					.defaultButtonColor());
			UIManager.put("ComboBox.buttonBackground", Preferences
					.defaultButtonColor());
			UIManager
					.put("Button.background", Preferences.defaultButtonColor());
			UIManager.put("ToggleButton.background", Preferences
					.defaultButtonColor());
			UIManager.put("Panel.background", Preferences
					.defaultBackgroundColor());
			UIManager.put("Label.background", Preferences
					.defaultBackgroundColor());
			UIManager.put("OptionPane.background", Preferences
					.defaultBackgroundColor());
			UIManager.put("ScrollPane.background", Preferences
					.defaultBackgroundColor());
			UIManager.put("SplitPane.background", Preferences
					.defaultBackgroundColor());
			UIManager.put("TabbedPane.background", Preferences
					.defaultBackgroundColor());
			UIManager.put("ToolBar.background", Preferences
					.defaultBackgroundColor());
			UIManager.put("ToolBar.dockingBackground", Preferences
					.defaultBackgroundColor());
			UIManager.put("ToolBar.floatingBackground", Preferences
					.defaultBackgroundColor());
			UIManager.put("ToolBar.viewportBackground", Preferences
					.defaultBackgroundColor());
		}

	}

	protected void installGlobalScriptObjects() {
		try {
			ExpressionManager.getManager().getContext().setGlobalVariable(
					"GUI", new GUIScriptDelegate(), false);
		} catch (ExpressionException ex) {
			// do nothing; this will always work
		}
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

	protected void showFrame() {
		JFrame frame = getManager().getFrame();
		SwingUtil.center(frame);
		frame.setVisible(true);
	}

}
