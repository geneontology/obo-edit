package org.oboedit.gui.tasks;

import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.JToggleButton;
import javax.swing.UIManager;

import net.infonode.docking.View;

import org.apache.log4j.Logger;
import org.bbop.dataadapter.DataAdapter;
import org.bbop.expression.ExpressionException;
import org.bbop.framework.AbstractApplicationStartupTask;
import org.bbop.framework.DockPanelFactory;
import org.bbop.framework.GUIComponent;
import org.bbop.framework.GUIComponentFactory;
import org.bbop.framework.GUIManager;
import org.bbop.framework.GUITask;
import org.bbop.framework.HelpManager;
import org.bbop.framework.LayoutMenu;
import org.bbop.framework.PluginManager;
import org.bbop.framework.ScreenLockTask;
import org.bbop.framework.VetoableShutdownListener;
import org.bbop.framework.ViewMenus;
import org.bbop.framework.dock.LayoutDriver;
import org.bbop.framework.dock.idw.BitmapIcon;
import org.bbop.framework.dock.idw.IDWDriver;
import org.bbop.framework.dock.idw.IDWUtil;
import org.bbop.framework.dock.idw.ViewListener;
import org.bbop.swing.GhostImageController;
import org.bbop.util.CollectionUtil;
import org.bbop.util.MultiArrayListMap;
import org.bbop.util.MultiMap;
import org.obo.dataadapter.GOFlatFileAdapter;
//import org.obo.dataadapter.GOStyleAnnotationFileAdapter;
import org.obo.dataadapter.OBOFileAdapter;
//import org.obo.dataadapter.OWLURLReaderAdapter;
import org.obo.dataadapter.SerialAdapter;
import org.obo.dataadapter.SimpleLinkFileAdapter;
import org.obo.dataadapter.XMLHistoryAdapter;
import org.obo.datamodel.OBOProperty;
import org.obo.filters.Filter;
import org.obo.filters.SearchCriterion;
import org.obo.identifier.DefaultIDGenerator;
import org.obo.identifier.IDGenerator;
//import org.obo.owl.dataadapter.OWLAdapter;
import org.obo.util.FilterUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.EditActionManager;
import org.oboedit.controller.ExpressionManager;
import org.oboedit.controller.FilterManager;
import org.oboedit.controller.FocusMenuManager;
import org.oboedit.controller.IDManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.AdvancedOBOUI;
//import org.oboedit.gui.AdvancedOWLUI;
import org.oboedit.gui.DefaultInputHandler;
import org.oboedit.gui.Filterable;
import org.oboedit.gui.GOFlatFileGUI;
import org.oboedit.gui.ObjectSelector;
import org.oboedit.gui.OntologyEditor;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.actions.AddAction;
import org.oboedit.gui.actions.AddConsiderAction;
import org.oboedit.gui.actions.AddGenusDifferentiaAction;
import org.oboedit.gui.actions.AddParentAction;
import org.oboedit.gui.actions.AddReplacementAction;
import org.oboedit.gui.actions.AddRootAction;
import org.oboedit.gui.actions.CloneAction;
import org.oboedit.gui.actions.CompletesAction;
import org.oboedit.gui.actions.CopyAction;
import org.oboedit.gui.actions.DeleteAction;
import org.oboedit.gui.actions.DomainChangeAction;
import org.oboedit.gui.actions.InvNecessaryAction;
import org.oboedit.gui.actions.MakeSubclassesMutuallyDisjointAction;
import org.oboedit.gui.actions.MergeAction;
import org.oboedit.gui.actions.MoveAction;
import org.oboedit.gui.actions.MultiAddAction;
import org.oboedit.gui.actions.NameUnnamedTermsAction;
import org.oboedit.gui.actions.NecessaryAction;
import org.oboedit.gui.actions.RangeChangeAction;
import org.oboedit.gui.actions.RemoveConsiderAction;
import org.oboedit.gui.actions.RemoveReplacementAction;
import org.oboedit.gui.actions.RerootAction;
import org.oboedit.gui.actions.TypeChangeAction;
import org.oboedit.gui.components.imageplugin.factory.TermImageComponentFactory;
import org.oboedit.gui.components.imageplugin.saveimage.InstallTask;
import org.oboedit.gui.components.ontologyGeneration.factory.OntologyGenerationComponentFactory;
import org.oboedit.gui.event.ReconfigEvent;
import org.oboedit.gui.event.ReconfigListener;
import org.oboedit.gui.factory.AssertLinksComponentFactory;
import org.oboedit.gui.factory.AnnotationSummaryComponentFactory;
import org.oboedit.gui.factory.ConfigurableMessageComponentFactory;
import org.oboedit.gui.factory.ConfigurationManagerFactory;
import org.oboedit.gui.factory.CrossProductInfoFactory;
import org.oboedit.gui.factory.CrossProductMatrixEditorFactory;
import org.oboedit.gui.factory.DbxrefLibraryFactory;
import org.oboedit.gui.factory.ExplanationComponentFactory;
import org.oboedit.gui.factory.ExtendedInfoFactory;
import org.oboedit.gui.factory.GlobalFilterManagerFactory;
import org.oboedit.gui.factory.GraphEditorFactory;
import org.oboedit.gui.factory.GraphViewFactory;
import org.oboedit.gui.factory.GraphvizViewFactory;
import org.oboedit.gui.factory.HistoryBrowserFactory;
import org.oboedit.gui.factory.IDManagerFactory;
import org.oboedit.gui.factory.IDResolutionComponentFactory;
import org.oboedit.gui.factory.IntersectionEditorFactory;
import org.oboedit.gui.factory.LinkSearchComponentFactory;
import org.oboedit.gui.factory.NamespaceManagerFactory;
import org.oboedit.gui.factory.OBOMergeCanvasFactory;
import org.oboedit.gui.factory.OntologyChangeTrackerFactory;
import org.oboedit.gui.factory.ParentEditorFactory;
import org.oboedit.gui.factory.ReasonerManagerFactory;
import org.oboedit.gui.factory.RemoveRedundantLinksComponentFactory;
import org.oboedit.gui.factory.SearchComponentFactory;
import org.oboedit.gui.factory.SearchResultsComponentFactory;
//import org.oboedit.gui.factory.SemanticParserManagerFactory;
import org.oboedit.gui.factory.SubsetManagerFactory;
import org.oboedit.gui.factory.SynonymTypeManagerFactory;
import org.oboedit.gui.factory.TableOfContentsFactory;
import org.oboedit.gui.factory.TermPanelFactory;
import org.oboedit.gui.factory.TextEditorFactory;
import org.oboedit.gui.factory.TreeViewFactory;
import org.oboedit.gui.factory.VerificationManagerFactory;
import org.oboedit.gui.filter.GeneralRendererSpecField;
import org.oboedit.gui.filter.MaxParentCountCriterion;
import org.oboedit.gui.menu.EditMenu;
import org.oboedit.gui.menu.FileMenu;
import org.oboedit.gui.menu.OEHelpMenu;
import org.oboedit.script.GUIScriptDelegate;


public class DefaultGUIStartupTask extends AbstractApplicationStartupTask {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefaultGUIStartupTask.class);

	@Override
	protected Collection<GUITask> getDefaultTasks() {
		ScreenLockTask screenLockTask = new ScreenLockTask(GUIManager
				.getManager().getScreenLockQueue(), GUIManager.getManager()
				.getFrame(), Preferences.getPreferences()
				.getUseModalProgressMonitors());
		return CollectionUtil.list(new AutosaveTask(),
				new PostLoadVerifyTask(), new PreSaveVerifyTask(),
				new FrameNameUpdateTask(), screenLockTask,
				new InstallTask(),
				new LoadRelationIconsTask()
		// , new AnnotationNumberFetchBehaviorTask()
		// , new LineNumberFetchBehaviorTask()
		);
	}

	@Override
	protected void installSystemListeners() {
		VetoableShutdownListener listener = new VetoableShutdownListener() {

			public boolean willShutdown() {
				if (GUIManager.getManager().getFrame() != null) {
					if (SessionManager.getManager().needsSave()) {
						return JOptionPane.showConfirmDialog(GUIManager
								.getManager().getFrame(),
								"<html>There are unsaved changes to your "
								+ "ontology.<br>Really quit?</html>",
								"Exit?", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION;
					} else if (GUIManager.isConfirmOnExit())
						return JOptionPane.showConfirmDialog(GUIManager
								.getManager().getFrame(), "Really quit?",
								"Exit?", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION;
				}
				return true;
			}
		};
		GUIManager.addVetoableShutdownListener(listener);
	}

	@Override
	protected Action getAboutAction() {
		return new AbstractAction("About") {

			public void actionPerformed(ActionEvent actionEvent) {
				OEHelpMenu.showAboutFrame();
			}
		};
	}

	@Override
	protected String getAppName() {
		return "OBO-Edit version " + Preferences.getVersion();
	}

	@Override
	protected Collection<GUIComponentFactory<?>> getDefaultComponentFactories() {
		return CollectionUtil.<GUIComponentFactory<?>>list(
				new TermPanelFactory(),
				new TermImageComponentFactory(),
				new GraphEditorFactory(), 
				new TextEditorFactory(),
				new TableOfContentsFactory(),
				new IDResolutionComponentFactory(), 
				new TreeViewFactory(),
				new GraphViewFactory(), 
				new SearchComponentFactory(),
				new LinkSearchComponentFactory(),
				new IntersectionEditorFactory(), 
				new SubsetManagerFactory(),
				new GraphvizViewFactory(), 
				new SynonymTypeManagerFactory(),
				new CrossProductInfoFactory(), 
				new DbxrefLibraryFactory(),
				new ExtendedInfoFactory(), 
				new HistoryBrowserFactory(),
				new IDManagerFactory(), 
				new ReasonerManagerFactory(),
                                //				new SemanticParserManagerFactory(),
				new NamespaceManagerFactory(),
				new OBOMergeCanvasFactory(),
				new OntologyChangeTrackerFactory(), 
				new ParentEditorFactory(),
				new CrossProductMatrixEditorFactory(),
				new AnnotationSummaryComponentFactory(),
				new GlobalFilterManagerFactory(),
				new ExplanationComponentFactory(),
				new ConfigurationManagerFactory(),
				new VerificationManagerFactory(), 
				new DockPanelFactory(),
				new ConfigurableMessageComponentFactory(),
				new SearchResultsComponentFactory(),
				new OntologyGenerationComponentFactory(),
				new AssertLinksComponentFactory(),
				new RemoveRedundantLinksComponentFactory()
		);
	}

	@Override
	protected void doOtherInstallations() {
		FilterManager.getManager().addCriterion(new MaxParentCountCriterion());
		Preferences.getPreferences().addReconfigListener(
				new ReconfigListener() {

					public void configReloaded(ReconfigEvent e) {
						SessionManager.getManager().getSession()
						.setCurrentUser(
								Preferences.getPreferences()
								.getUserName());
					}
				});
		UIManager.put("Tree.paintLines", Boolean.FALSE);
		HelpManager.getManager().setHelpSetFile(
				new File(Preferences.getInstallationDirectory(),
				"docs/OBO-Edit.hs"));
		FocusMenuManager.install();
		installDefaultActions();
		installGlobalScriptObjects();
		GhostImageController.enable();
	}

	@Override
	protected void installPlugins() {
		super.installPlugins();
		for (SearchCriterion<?, ?> crit : PluginManager.getManager()
				.instantiateAll(SearchCriterion.class)) {
			FilterManager.getManager().addCriterion(crit);
		}
		for (GeneralRendererSpecField<?> field : PluginManager.getManager()
				.instantiateAll(GeneralRendererSpecField.class)) {
			FilterManager.getManager().addRenderSpecField(field);
		}
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
			// Note: if any of these icons is missing from the jar, it causes a weird exception later.
			// Should handle that better (but non-trivial, because the image loading happens in a
			// separate thread).
			protected Icon globalModeIcon = new BitmapIcon(Preferences
					.loadLibraryImage("tiny_blue_globe_icon.gif"));

			protected Icon localModeIcon = new BitmapIcon(Preferences
					.loadLibraryImage("tiny_green_house_icon.gif"));

			// scroll lock for OTE
			protected Icon lockViewIcon = new BitmapIcon(Preferences
					.loadLibraryImage("locked_black.gif"));	
			protected Icon unlockViewIcon = new BitmapIcon(Preferences
					.loadLibraryImage("unlocked_black.gif"));

			protected Icon filterIcon = new BitmapIcon(Preferences
					.loadLibraryImage("tiny_filter_icon.gif"));


			protected Icon cameraIcon = new BitmapIcon(Preferences
					.loadLibraryImage("tiny_camera_icon.gif"));

			protected MultiMap<GUIComponent, JComponent> compMap = new MultiArrayListMap<GUIComponent, JComponent>();

			public void viewCreated(View v, final GUIComponent c) {
				if (c instanceof Filterable) {
					final JButton filterButton = IDWUtil
					.createFlatHighlightButton(filterIcon,
							"Quick Filtering", 0, null);
					filterButton.addActionListener(new ActionListener() {
						public void actionPerformed(ActionEvent arg0) {
							JPopupMenu menu = new JPopupMenu();
							JLabel label = new JLabel("Quick Filtering");
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
					compMap.add(c, filterButton);
				}

				// Local and Global mode buttons for OTE and Graph Editor
				// and lock/unlock view modes for OTE
				if (c instanceof ObjectSelector) {
					final JToggleButton globalModeButton = IDWUtil.createFlatHighlightToggleButton(
							globalModeIcon, "Global Mode Set - Select to Switch to Local Mode",
							0, null);

					if (c instanceof OntologyEditor){
						((OntologyEditor) c).setLiveButton(globalModeButton);

//						// lock/unlock buttons for OTE top panel button panel
//						final JToggleButton lockViewButton = IDWUtil.createFlatHighlightToggleButton(
//								lockViewIcon, "Lock OTE",0, null);
//
//						lockViewButton.addActionListener(new ActionListener() {
//							public void actionPerformed(ActionEvent e) {
//								//if OTE is locked - unlock
//								if(GUIManager.getManager().getOTELockStatus()){
//									GUIManager.getManager().setOTELockStatus(false);
//									lockViewButton.setIcon(lockViewIcon);
//									lockViewButton.setToolTipText("Unlock OTE");
////									setLockedPath(null);					
//								} 
//								else {
//									//lock
//									GUIManager.getManager().setOTELockStatus(true);
//									lockViewButton.setIcon(unlockViewIcon);
//									lockViewButton.setToolTipText("Lock OTE");
////									if(selectedPaths.length >0){
////										setLockedPath(selectedPaths[0]);
////									}
//								}//else
//							}
//
//						});
//						compMap.add(c, lockViewButton);
					}// c instance of OntologyEditor

					
					// local/ global setup
					globalModeButton.setSelected(((ObjectSelector) c).isLive());
					if (!((ObjectSelector) c).isLive()) {
						globalModeButton.setIcon(localModeIcon);
					}
					globalModeButton.addActionListener(new AbstractAction() {
						public void actionPerformed(ActionEvent e) {
//							logger.debug("ObjectSelector c: " + c);
							((ObjectSelector) c).setLive(globalModeButton.isSelected());
						}
					});
					globalModeButton.addActionListener(new ActionListener() {
						public void actionPerformed(ActionEvent e) {
							if (globalModeButton.isSelected()) {
								globalModeButton.setIcon(globalModeIcon);
								globalModeButton.setToolTipText("Global Mode Set - Select to Switch to Local Mode");
							} else {
								globalModeButton.setIcon(localModeIcon);
								globalModeButton.setToolTipText("Local Mode Set - Select to Switch to Global Mode");
							}
						}
					});
					compMap.add(c, globalModeButton);
				}
				
				// Add camera icon to list of icons to add to titlebars
				final JButton cameraButton = IDWUtil.createFlatHighlightButton(
						cameraIcon, "Save Image", 0,
						null);
				compMap.add(c, cameraButton);
				cameraButton.addActionListener(new ActionListener() {
					public void actionPerformed(ActionEvent e) {
						InstallTask.saveImage(c);
					}
				});
				cameraButton.setToolTipText("Save Image");

				// Add icons to titlebars
				for (JComponent bc : compMap.get(c)) {
					v.getCustomTitleBarComponents().add(bc);
					// Also add to tab in case theme is not using titlebar style
					v.getCustomTabComponents().add(bc);
				}
			}

			public void viewDestroyed(View v, GUIComponent c) {
				for (JComponent bc : compMap.get(c)) {
					v.getCustomTitleBarComponents().remove(bc);
					// Also remove from tab
					v.getCustomTabComponents().remove(bc);
				}
				compMap.remove(c);
			}

		});
		return driver;
	}

	@Override
	protected Collection<DataAdapter> getDefaultDataAdapters() {
		List<DataAdapter> adapters = new LinkedList<DataAdapter>();
		final OBOFileAdapter oboadapter = new OBOFileAdapter();
		oboadapter.setAdvancedUI(new AdvancedOBOUI());
                // OWL adapter not currently supported (Oct 2011)
                //		final OWLAdapter owladapter = new OWLAdapter();
                //		owladapter.setAdvancedUI(new AdvancedOWLUI());
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
		adapters.add(new SimpleLinkFileAdapter());
                // Not currently supported.
                //		adapters.add(new GOStyleAnnotationFileAdapter());
                //		adapters.add(owladapter);
                //		adapters.add(new OWLURLReaderAdapter());
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
		EditActionManager.getManager().addEditAction(new AddGenusDifferentiaAction());
		EditActionManager.getManager().addEditAction(new MultiAddAction());
		EditActionManager.getManager().addEditAction(new DeleteAction(false));
		EditActionManager.getManager().addEditAction(new DeleteAction(true));
		EditActionManager.getManager().addEditAction(new NameUnnamedTermsAction());
		EditActionManager.getManager().addEditAction(new CloneAction());
		EditActionManager.getManager().addEditAction(new AddRootAction());
		EditActionManager.getManager().addEditAction(new RerootAction());
		EditActionManager.getManager().addEditAction(new MakeSubclassesMutuallyDisjointAction());
		EditActionManager.getManager().addEditAction(new NecessaryAction());
		EditActionManager.getManager().addEditAction(new InvNecessaryAction());
		EditActionManager.getManager().addEditAction(new CompletesAction());
		EditActionManager.getManager().addEditAction(new RemoveConsiderAction());
		EditActionManager.getManager().addEditAction(new RemoveReplacementAction());
	}

	@Override
	protected Collection<? extends JMenuItem> getDefaultMenus() {
		//		return CollectionUtil.list(new FileMenu(), new EditMenu(),
		//		new ViewMenu(), new LayoutMenu(), new OEHelpMenu());
		// New menu organization
		List<JMenuItem> menus = new ArrayList<JMenuItem>();
		menus.add(new FileMenu());
		menus.add(new EditMenu());
		menus.add(new LayoutMenu());

		List<JMenu> viewMenus = new ViewMenus().getMenus();
                //		String[] advancedMenuItems = {"SemanticParser Manager", "Intersection Editor", "Cross-Product Matrix Editor"};
		String[] advancedMenuItems = {"Intersection Editor", "Cross-Product Matrix Editor"};
		String[] reqReasonerMenuItems = { "Explanations", "Assert Implied Links Panel", "Remove Redundant Links Panel"};

		for (JMenu m : viewMenus){
			menus.add(m);
			//			logger.debug("menu: " + m.getText().toString() + " # items: " + m.getItemCount());
			if(m.getText().toString().equalsIgnoreCase("Reasoner") || m.getText().toString().equalsIgnoreCase("Editors")){

				//components that can be switched on through the Advanced configuration settings
				for (int i=0; i < m.getItemCount(); i++) {
					for(String advItem : advancedMenuItems){
						if(m.getItem(i).getText().equalsIgnoreCase(advItem)){
							m.getItem(i).setEnabled(false);
						}
					}
				}

				//components that require the reasoner to be on
				if(!SessionManager.getManager().getUseReasoner()){
					for (int i = 0; i < m.getItemCount(); i++) {
						for(String reqReasonerMenuItem : reqReasonerMenuItems){
							if(m.getItem(i).getText().equalsIgnoreCase(reqReasonerMenuItem)){
								m.getItem(i).setEnabled(false);
							}
						}
					}
				}
			}
		}
		menus.add(new OEHelpMenu());
		return menus;
	}

	@Override
	protected File[] getPluginDirs() {
		File[] out = { new File(GUIManager.getPrefsDir(), "extensions"),
				new File(Preferences.getInstallationDirectory(), "extensions") };
		return out;
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
	protected String getAppID() {
		// TODO Auto-generated method stub
		//		return "oboedit";
		return Preferences.getAppName();
	}

	// Moved to Preferences
	@Override
	public File getPrefsDir() {
		return Preferences.getOBOEditPrefsDir();
	}

	@Override
	protected void installDefaultToolBars() {
	}

}
