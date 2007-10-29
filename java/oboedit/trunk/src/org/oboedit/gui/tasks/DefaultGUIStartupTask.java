package org.oboedit.gui.tasks;

import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.lang.Thread.UncaughtExceptionHandler;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.logging.FileHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import java.util.logging.StreamHandler;

import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JToggleButton;
import javax.swing.LookAndFeel;
import javax.swing.UIManager;

import net.infonode.docking.View;

import org.bbop.dataadapter.DataAdapter;
import org.bbop.dataadapter.DataAdapterRegistry;
import org.bbop.expression.ExpressionException;
import org.bbop.framework.AbstractApplicationStartupTask;
import org.bbop.framework.AbstractSingleActionTask;
import org.bbop.framework.ComponentManager;
import org.bbop.framework.DockPanelFactory;
import org.bbop.framework.GUIComponent;
import org.bbop.framework.GUIComponentFactory;
import org.bbop.framework.GUIManager;
import org.bbop.framework.GUITask;
import org.bbop.framework.ViewMenu;
import org.bbop.framework.dock.LayoutDriver;
import org.bbop.framework.dock.idw.BitmapIcon;
import org.bbop.framework.dock.idw.IDWDriver;
import org.bbop.framework.dock.idw.IDWUtil;
import org.bbop.framework.dock.idw.ViewListener;
import org.bbop.io.LoggerStream;
import org.bbop.io.MultiOutputStream;
import org.bbop.swing.GhostImageController;
import org.bbop.swing.SwingUtil;
import org.bbop.util.CollectionUtil;
import org.bbop.util.ExceptionLogger;
import org.obo.dataadapter.GOFlatFileAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.dataadapter.OWLURLReaderAdapter;
import org.obo.dataadapter.SerialAdapter;
import org.obo.dataadapter.XMLHistoryAdapter;
import org.obo.datamodel.OBOProperty;
import org.obo.filters.Filter;
import org.obo.filters.LinkFilter;
import org.obo.identifier.DefaultIDGenerator;
import org.obo.identifier.IDGenerator;
import org.obo.reasoner.impl.LinkPileReasoner;
import org.obo.reasoner.impl.LinkPileReasonerFactory;
import org.obo.util.FilterUtil;
import org.obo.util.TermUtil;
import org.obo.util.VersionNumber;
import org.oboedit.controller.EditActionManager;
import org.oboedit.controller.ExpressionManager;
import org.oboedit.controller.FocusMenuManager;
import org.oboedit.controller.IDManager;
import org.oboedit.controller.IOManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.AdvancedOBOUI;
import org.oboedit.gui.DefaultInputHandler;
import org.oboedit.gui.Filterable;
import org.oboedit.gui.GOFlatFileGUI;
import org.oboedit.gui.ObjectSelector;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.actions.AddAction;
import org.oboedit.gui.actions.AddConsiderAction;
import org.oboedit.gui.actions.AddParentAction;
import org.oboedit.gui.actions.AddReplacementAction;
import org.oboedit.gui.actions.AddRootAction;
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
import org.oboedit.gui.components.ConfigurableTextComponent;
import org.oboedit.gui.event.ReconfigEvent;
import org.oboedit.gui.event.ReconfigListener;
import org.oboedit.gui.factory.ConfigurableMessageComponentFactory;
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
import org.oboedit.gui.factory.LinkSearchComponentFactory;
import org.oboedit.gui.factory.NamespaceManagerFactory;
import org.oboedit.gui.factory.OntologyChangeTrackerFactory;
import org.oboedit.gui.factory.ParentEditorFactory;
import org.oboedit.gui.factory.ReasonerManagerFactory;
import org.oboedit.gui.factory.SearchComponentFactory;
import org.oboedit.gui.factory.SynonymCategoryManagerFactory;
import org.oboedit.gui.factory.TermImageComponentFactory;
import org.oboedit.gui.factory.TermPanelFactory;
import org.oboedit.gui.factory.TextEditorFactory;
import org.oboedit.gui.factory.VerificationManagerFactory;
import org.oboedit.gui.menu.EditMenu;
import org.oboedit.gui.menu.FileMenu;
import org.oboedit.gui.menu.OEHelpMenu;
import org.oboedit.script.GUIScriptDelegate;

public class DefaultGUIStartupTask extends AbstractApplicationStartupTask {

	@Override
	protected Collection<GUITask> getDefaultTasks() {
		ScreenLockTask screenLockTask = new ScreenLockTask(GUIManager
				.getManager().getScreenLockQueue(), GUIManager.getManager()
				.getFrame(), Preferences.getPreferences()
				.getUseModalProgressMonitors());
		return CollectionUtil.list(new AutosaveTask(),
				new PostLoadVerifyTask(), new FrameNameUpdateTask(),
				screenLockTask);
	}
	
	@Override
	protected String getAppName() {
		return "OBO-Edit version " + Preferences.getVersion();
	}

	@Override
	protected Collection<GUIComponentFactory<?>> getDefaultComponentFactories() {
		return (Collection) CollectionUtil.list(new TermPanelFactory(),
				new GraphEditorFactory(), new TextEditorFactory(),
				new DAGViewFactory(), new GraphDAGViewFactory(),
				new SearchComponentFactory(), new LinkSearchComponentFactory(),
				new IntersectionEditorFactory(), new CategoryManagerFactory(),

				new SynonymCategoryManagerFactory(),
				new CrossProductInfoFactory(), new DbxrefLibraryFactory(),
				new ExtendedInfoFactory(), new HistoryBrowserFactory(),
				new IDManagerFactory(), new ReasonerManagerFactory(),
				new NamespaceManagerFactory(),

				new OntologyChangeTrackerFactory(), new ParentEditorFactory(),

				new CrossProductMatrixEditorFactory(),

				new AnnotationSummaryComponentFactory(),

				new GlobalFilterManagerFactory(),
				new ExplanationComponentFactory(),
				new ConfigurationManagerFactory(),
				new VerificationManagerFactory(), new DockPanelFactory(),
				new TermImageComponentFactory(), new ConfigurableMessageComponentFactory());

	}

	@Override
	protected void doOtherInstallations() {
		FocusMenuManager.install();
		installDefaultActions();
		installGlobalScriptObjects();
		GhostImageController.enable();
	}

	@Override
	protected String getPerspectiveResourceDir() {
		return "org/oboedit/gui/dock/resources";
	}

	@Override
	protected Color getBackgroundColor() {
		return Preferences.defaultBackgroundColor();
	}

	@Override
	protected Color getButtonColor() {
		return Preferences.defaultButtonColor();
	}

	@Override
	protected Color getLightColor() {
		return Color.white;
	}

	protected Font getFont() {
		return Preferences.getPreferences().getFont();
	}

	protected LayoutDriver createLayoutDriver() {
		IDWDriver driver = (IDWDriver) super.createLayoutDriver();
		driver.addViewListener(new ViewListener() {
			protected Icon globeIcon = new BitmapIcon(Preferences
					.loadLibraryImage("tiny_globe_icon.gif"));

			protected Icon houseIcon = new BitmapIcon(Preferences
					.loadLibraryImage("tiny_house_icon.gif"));

			protected Icon filterIcon = new BitmapIcon(Preferences
					.loadLibraryImage("tiny_filter_icon.gif"));

			protected Icon filterInvIcon = new BitmapIcon(Preferences
					.loadLibraryImage("tiny_filter_icon.gif"));

			public void viewCreated(View v, final GUIComponent c) {
				if (c instanceof Filterable) {
					final JButton filterButton = IDWUtil
							.createFlatHighlightButton(filterIcon,
									"Quick filtering", 0, null);
					filterButton.addActionListener(new ActionListener() {
						public void actionPerformed(ActionEvent arg0) {
							JPopupMenu menu = new JPopupMenu();
							JLabel label = new JLabel("Quick filtering");
							label
									.setFont(label.getFont().deriveFont(
											Font.BOLD));

							menu.add(label);
							menu.addSeparator();
							JMenuItem showAllItem = new JMenuItem(
									"Show all relationship types");
							showAllItem.addActionListener(new ActionListener() {

								public void actionPerformed(ActionEvent arg0) {
									((Filterable) c).setLinkFilter(null);
								}
							});
							menu.add(showAllItem);
							JMenu showOnlyMenu = new JMenu(
									"Show a single relationship");
							menu.add(showOnlyMenu);
							JMenu showParticularMenu = new JMenu(
									"Show particular relationships");
							menu.add(showParticularMenu);
							final Map<JCheckBoxMenuItem, OBOProperty> items = new HashMap<JCheckBoxMenuItem, OBOProperty>();
							Filter f = ((Filterable) c).getLinkFilter();
							for (final OBOProperty p : TermUtil
									.getRelationshipTypes(SessionManager
											.getManager().getSession())) {
								JMenuItem onlyItem = new JMenuItem(p.toString());
								onlyItem
										.addActionListener(new ActionListener() {
											public void actionPerformed(
													ActionEvent arg0) {
												((Filterable) c)
														.setLinkFilter(FilterUtil
																.getTypeFilter(p));
											}
										});
								showOnlyMenu.add(onlyItem);
								JCheckBoxMenuItem item = new JCheckBoxMenuItem(
										"Show " + p);
								items.put(item, p);
								item.setSelected(f == null
										|| FilterUtil.filtersOn(f, p));
								item.addActionListener(new ActionListener() {

									public void actionPerformed(ActionEvent arg0) {
										List<OBOProperty> selected = new ArrayList<OBOProperty>();
										for (JCheckBoxMenuItem item : items
												.keySet()) {
											if (item.isSelected())
												selected.add(items.get(item));
										}
										((Filterable) c)
												.setLinkFilter(FilterUtil
														.getTypeFilter(selected
																.toArray(new OBOProperty[0])));
									}
								});
								showParticularMenu.add(item);
							}
							menu.show(filterButton,
									filterButton.getWidth() / 2, filterButton
											.getHeight() / 2);

						}
					});
					v.getCustomTitleBarComponents().add(filterButton);
				}
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

	@Override
	protected Collection<DataAdapter> getDefaultDataAdapters() {
		List<DataAdapter> adapters = new LinkedList();
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
		adapters.add(oboadapter);
		adapters.add(goadapter);
		adapters.add(new OWLURLReaderAdapter());
		adapters.add(new SerialAdapter());
		adapters.add(new XMLHistoryAdapter());
		return adapters;
	}

	protected void installDefaultActions() {
		installDefaultDropMenuActions();
		installDefaultEditActions();
		installDefaultInputHandlers();
	}

	protected void installDefaultInputHandlers() {
		EditActionManager.getManager().addInputHandler(
				new DefaultInputHandler());
		EditActionManager.getManager().addInputHandler(new CopyAction());
		EditActionManager.getManager().addInputHandler(new AddParentAction());
		EditActionManager.getManager().addInputHandler(new MoveAction());
		EditActionManager.getManager().addInputHandler(new MergeAction());
		EditActionManager.getManager().addInputHandler(new TypeChangeAction());
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

	@Override
	protected Collection<? extends JMenuItem> getDefaultMenus() {
		return CollectionUtil.list(new FileMenu(), new EditMenu(),
				new ViewMenu(), new OEHelpMenu());
	}

	protected void installGlobalScriptObjects() {
		try {
			ExpressionManager.getManager().getContext().setGlobalVariable(
					"GUI", new GUIScriptDelegate(), false);
		} catch (ExpressionException ex) {
			// do nothing; this will always work
		}
	}

	@Override
	protected DataAdapterRegistry getAdapterRegistry() {
		return IOManager.getManager().getAdapterRegistry();
	}

	@Override
	protected String getAppID() {
		// TODO Auto-generated method stub
		return "oboedit";
	}

	@Override
	protected File getPrefsDir() {
		VersionNumber version = Preferences.getVersion();
		File prefsDir = new File(System.getProperty("user.home") + "/.oboedit"
				+ (version.isBeta() ? "beta" : "") + "/");
		return prefsDir;
	}
}
