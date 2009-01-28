package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.SpinnerNumberModel;

import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.ConfigurationPanel;
import org.bbop.framework.GUIComponent;
import org.hsqldb.lib.Iterator;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.RootAlgorithm;
import org.obo.filters.Filter;
import org.obo.reasoner.impl.OnTheFlyReasoner.ReasonerLink;
import org.obo.util.TermUtil;
import org.oboedit.controller.EditActionManager;
import org.oboedit.controller.SelectionManager;
import org.oboedit.graph.BoundsGuarantor;
import org.oboedit.graph.DemoDecorator;
import org.oboedit.graph.DragDropEditBehavior;
import org.oboedit.graph.FocusPicker;
import org.oboedit.graph.GraphLayout;
import org.oboedit.graph.HierarchicalGraphLayout;
import org.oboedit.graph.LinkButtonBehavior;
import org.oboedit.graph.LinkExpanderRightClickMenuFactory;
import org.oboedit.graph.OverviewCameraBehavior;
import org.oboedit.graph.QuickSearchBehavior;
import org.oboedit.graph.RootDisplayRightClickMenuFactory;
import org.oboedit.graph.SelectionBehavior;
import org.oboedit.graph.ToolbarBehavior;
import org.oboedit.graph.TooltipBehavior;
import org.oboedit.piccolo.WordBubbleNode;
import org.oboedit.graph.ViewBehavior;
import org.oboedit.graph.VisibilityDropBehavior;
import org.oboedit.graph.ZoomWidgetBehavior;
import org.oboedit.gui.EditActionToolbar;
import org.oboedit.gui.HTMLNodeLabelProvider;
import org.oboedit.gui.InputHandlerI;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.event.ReloadEvent;
import org.oboedit.gui.event.ReloadListener;
import org.oboedit.gui.event.ReconfigEvent;
import org.oboedit.gui.event.ReconfigListener;
import org.oboedit.gui.event.SelectionEvent;
import org.oboedit.gui.event.SelectionListener;
import org.oboedit.gui.filter.RenderedFilter;
import org.oboedit.util.GUIUtil;

import org.apache.log4j.*;

public class GraphEditor extends LinkDatabaseCanvas implements GUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GraphEditor.class);

	//	protected static final Color lockGray = new Color(200, 200, 200);
	protected static final Color lockGray = new Color(240, 240, 240);  // when panel is in local selection mode

	protected boolean tooltipFlareVisible = true;
	protected boolean tooltipBubbleVisible = true;
	boolean tooltipBehaviorInPlace = true;


	public static class GraphEditorConfiguration extends OntologyEditorConfiguration {
		public GraphEditorConfiguration() {
			super();
		}

		protected boolean disableAnimations;
		protected long layoutDuration = 500;  // milliseconds (user-configurable)
		protected boolean expandPaths = true;
		protected boolean tooltipFlareVisible = true;
		protected boolean tooltipBubbleVisible = true;

		public boolean getTooltipFlareVisible() {
			//logger.debug("GraphEditorConfiguration: getTooltipFlareVisible: " +
					//"tooltipFlareVisible = " + tooltipFlareVisible);
			return tooltipFlareVisible;
		}

		public void setTooltipFlareVisible(boolean tooltipFlareVisible) {
			//logger.debug("GraphEditorConfiguration: setTooltipFlareVisible: " +
					//"tooltipFlareVisible = " + tooltipFlareVisible);
			this.tooltipFlareVisible = tooltipFlareVisible;
		}

		public boolean getTooltipBubbleVisible() {
			//logger.debug("GraphEditorConfiguration: getTooltipBubbleVisible: " +
					//"tooltipBubbleVisible = " + tooltipBubbleVisible);
			return tooltipBubbleVisible;
		}

		public void setTooltipBubbleVisible(boolean tooltipBubbleVisible) {
			//logger.debug("GraphEditorConfiguration: setTooltipBubbleVisible: " +
					//"tooltipBubbleVisible = " + tooltipBubbleVisible);
			this.tooltipBubbleVisible = tooltipBubbleVisible;
		}

		public GraphEditorConfiguration(Filter<?> filter, Filter<?> filter2,
				List<RenderedFilter> list, List<RenderedFilter> list2,
				String basicHTML, boolean b, long l, int i, String string,
				String handlerID, boolean c, boolean d, String algorithmStr,
				boolean e) {
			//logger.debug("GraphEditorConfiguration: Constructor without arguments");

		}

		public boolean getDisableAnimations() {
			return disableAnimations;
		}

		public void setDisableAnimations(boolean disableAnimations) {
			this.disableAnimations = disableAnimations;
		}

		public GraphEditorConfiguration(Filter<?> termFilter,
				Filter<?> linkFilter, List<RenderedFilter> objectRenderers,
				List<RenderedFilter> linkRenderers, String basicHTML,
				boolean disableAnimations, long layoutDuration,
				int showToolbar, String toolbarPosition, String dragActionID,
				boolean revert, boolean live, String rootAlgorithm,
				boolean expandPaths
				, boolean tooltipBubbleVisible, 
				boolean tooltipFlareVisible) 
		{	
			super(termFilter, linkFilter, objectRenderers, linkRenderers,
					basicHTML, showToolbar, toolbarPosition, dragActionID,
					revert, live, rootAlgorithm);
			//logger.debug("GraphEditorConfiguration: Constructor with arguments");
			this.tooltipBubbleVisible = tooltipBubbleVisible;
			this.tooltipFlareVisible = tooltipFlareVisible;
			this.disableAnimations = disableAnimations;
			this.layoutDuration = layoutDuration;
			this.expandPaths = expandPaths;
		}

		public long getLayoutDuration() {
			return layoutDuration;
		}

		public void setLayoutDuration(long layoutDuration) {
			this.layoutDuration = layoutDuration;
		}

		@Override
		public String getToolbarPosition() {
			return BorderLayout.SOUTH;
		}

		protected boolean isExpandPaths() {
			return expandPaths;
		}

		protected void setExpandPaths(boolean expandPaths) {
			this.expandPaths = expandPaths;
		}
	}

	protected String id;

	protected String title;

	public String getTitle() {
		if (title == null)
			return getID();
		return title;
	}

	// Note: if this method returns false, then this component continues
	// to run in the background even when it's not in the current layout.
	// The downside of returning true (which is the default for this
	// method) is that if the user switches between layouts (e.g. Edit and
	// Verify), the component forgets what it was showing.
	public boolean teardownWhenHidden() {
		//		return false;
		return true;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	protected ReloadListener reloadListener = new ReloadListener() {
		public void reload(ReloadEvent e) {
			//		    logger.info("GraphEditor.reload: e = " + e); // DEL
			if (e.isHistory() || e.isRoot() || e.isReasoner() || e.isOntologyReload()
					|| e.isFilter()) {
				updateDatasources();
				if (linkDatabase.getObjects().size() == 0) {
					Collection<? extends LinkedObject> roots = TermUtil
					.getRoots(getRootAlgorithm(), getLinkDatabase());
					addPostLayoutAction(new Runnable() {

						public void run() {
							panToObjects();
						}

					});
					setVisibleObjects(roots);
				}
			} else
				relayout();
		}
	};

	protected ReconfigListener reconfigListener = new ReconfigListener() {

		public void configReloaded(ReconfigEvent e) {
			relayout();  // Need?  Is this already called elsewhere?
		}

	};

	protected JPanel panel;
	protected RootAlgorithm rootAlgorithm = RootAlgorithm.GREEDY;
	protected EditActionToolbar toolbar;
	protected DragDropEditBehavior dragDropBehavior;
	protected boolean revertToDefaultAction = false;

	public ConfigurationPanel getConfigurationPanel() {
		//logger.debug("GraphEditor: getConfigurationPanel()");
		return new OntologyEditorConfigEditor() {
			protected JCheckBox animationsBox;
			protected JSpinner animationDurationSpinner;
			protected JCheckBox expandSelectionPathsBox;
			protected JCheckBox tooltipBubbleVisibleCheckBox;
			protected JCheckBox tooltipFlareVisibleCheckBox;
			protected JLabel tooltipConfigLabel;

			@Override
			protected JComponent createGUIConfigPanel() {
				//logger.debug("GraphEditor: createGUIConfigPanel");
				animationDurationSpinner = new JSpinner(new SpinnerNumberModel(
						//										1000, 1, null, 100));
						500, 1, null, 100));
				animationsBox = new JCheckBox("Enable animations");
				JComponent out = super.createGUIConfigPanel();
				int durationHeight = (int) animationDurationSpinner
				.getPreferredSize().getHeight();
				final Box animationDurationBox = new Box(BoxLayout.X_AXIS);
				animationDurationBox.add(new JLabel("Animation duration"));
				animationDurationBox.add(Box.createHorizontalStrut(10));
				animationDurationBox.add(animationDurationSpinner);
				animationDurationBox.add(new JLabel("ms"));
				animationDurationBox.add(Box.createHorizontalGlue());
				animationDurationBox.setMaximumSize(new Dimension(
						Integer.MAX_VALUE, durationHeight));

				animationsBox.addActionListener(new ActionListener() {

					public void actionPerformed(ActionEvent e) {
						animationDurationBox.setEnabled(animationsBox
								.isSelected());
						animationDurationSpinner.setEnabled(animationsBox
								.isSelected());
					}

				});
				expandSelectionPathsBox = new JCheckBox(
				"Expand full path of selected terms");
				final Box tooltipConfigurationBox = new Box(BoxLayout.X_AXIS);

				tooltipConfigLabel = new JLabel("Tooltips");
				tooltipBubbleVisibleCheckBox = new JCheckBox("Show tooltips");
				tooltipFlareVisibleCheckBox = new JCheckBox("Show tooltips with stalks");
				tooltipConfigurationBox.add(tooltipConfigLabel);
				tooltipConfigurationBox.add(tooltipBubbleVisibleCheckBox);
				tooltipConfigurationBox.add(tooltipFlareVisibleCheckBox);

				out.add(animationsBox);
				out.add(animationDurationBox);
				out.add(expandSelectionPathsBox);
				out.add(tooltipConfigurationBox);
				out.validate();
				return out;
			}

			/**
			 * Takes the settings shown in the config panel and puts them into a GraphEditorConfiguration object 
			 * so that they can be passed to the
			 * GraphEditor object. This is run when the config panel is closed. 
			 * GraphEditor.setConfiguration is run immediately afterwards as is called next by OntologyEditorConfigEditor.commit().
			 */
			@Override
			public void commitConfig(OntologyEditorConfiguration c) {
				//logger.debug("GraphEditor: commitConfig");
				GraphEditorConfiguration config = (GraphEditorConfiguration) c;
				config.setDisableAnimations(!animationsBox.isSelected());
				// For some reason, this doesn't seem to work.  Keeps getting reset to 1000!
				config.setLayoutDuration(((Number) animationDurationSpinner
						.getValue()).longValue());
				config.setExpandPaths(expandSelectionPathsBox.isSelected());
				config.setTooltipBubbleVisible(tooltipBubbleVisibleCheckBox.isSelected());
				 config.setTooltipFlareVisible(tooltipFlareVisibleCheckBox.isSelected());
				setTooltipBubbleVisible(tooltipBubbleVisibleCheckBox.isSelected());
				setTooltipFlareVisible(tooltipFlareVisibleCheckBox.isSelected());

				//If tooltipBehavior was previously enable then disable it. 
				Collection<ViewBehavior> listOfAddedBehaviors = new Vector<ViewBehavior>(viewBehaviors);	
				TooltipBehavior currentTooltipBehavior = null;
				
				for (ViewBehavior viewBehavior : listOfAddedBehaviors) {
					if (viewBehavior instanceof TooltipBehavior){
						//logger.debug("GraphEditor: commitConfig: viewBehavior = " + viewBehavior);
						currentTooltipBehavior = (TooltipBehavior) viewBehavior;
					}
					
				}
				
				//We remove the tooltipViewBehavior, because if it was previously added and then the checkbox is selected and the 
				//panel closed then it will be added again. This would mean we would have two tooltipBehavior objects added at the same time. 
				//
				if (currentTooltipBehavior != null){
					removeViewBehavior(currentTooltipBehavior);
					}

				//We do not need to have another step adding the tooltipBehavior, even if the checkbox is checked, because if it is
				//checked then the tooltipBubbleVisible variable will be set to true in the GraphEditorConfiguration object and this will
				//be passed to setCofiguration (which is run next as it is the next thing called by the by the commit() method 
				//inherited from OntologyEditorConfigEditor). If the variable is true, setConfiguration will then add the behavior. 
				
				//We decided to do this add in setConfiguration rather than in commitConfig, because setConfiguration is also run 
				// when a new GraphEditor panel is made, whereas commitConfig is not. 

				super.commitConfig(c);
			}

			/**
			 * Takes the settings information from the GraphEditor object and sets the controls in the new config panel to reflect these. 
			 */
			@Override
			protected void initConfig(OntologyEditorConfiguration c) {
				//logger.debug("GraphEditor: initConfig");					
				GraphEditorConfiguration config = (GraphEditorConfiguration) c;
				animationsBox.setSelected(!config.getDisableAnimations());
				//				//logger.debug("initConfig: setting value of spinner to " + config.getLayoutDuration());
				animationDurationSpinner.setValue(config.getLayoutDuration());
				expandSelectionPathsBox.setSelected(config.isExpandPaths());
				tooltipBubbleVisibleCheckBox.setSelected(config.getTooltipBubbleVisible());
				tooltipFlareVisibleCheckBox.setSelected(config.getTooltipFlareVisible());
				super.initConfig(config);
			}
		};
	}

	public GraphEditor(String id) {
		this(id, new HierarchicalGraphLayout());
		//logger.debug("GraphEditor: Constructor");
	}

	public GraphEditor(String id, GraphLayout graphLayout) {
		super(graphLayout);
		this.id = id;
		//logger.debug("GraphEditor: Constructor with layout argument");

	}

	public void completeDrop() {
		if (isRevertToDefaultAction()) {
			toolbar.setCurrentHandler(EditActionManager.getManager()
					.getDefaultInputHandler());
		}
	}

	public DragDropEditBehavior getDragDropEditBehavior() {
		return dragDropBehavior;
	}


	protected void addDefaultBehaviors() {
		//logger.debug("GraphEditor: addDefaultBehaviors");
		dragDropBehavior = new DragDropEditBehavior();
		addViewBehavior(dragDropBehavior);
		addViewBehavior(getRightClickBehavior());
		addViewBehavior(new ToolbarBehavior());
		addViewBehavior(new BoundsGuarantor());
		addViewBehavior(new FocusPicker());
		addViewBehavior(new QuickSearchBehavior());
		addViewBehavior(new SelectionBehavior(false));
		addViewBehavior(new LinkButtonBehavior());
		addViewBehavior(new ZoomWidgetBehavior());

		//TODO
		//make this conditional on tooltipBubbleVisible being true. 
		if(this.tooltipBubbleVisible){

			//logger.debug("GraphEditor: AddDefaultBehaviors, " +
					//"this.tooltipBubbleVisible = " + this.tooltipBubbleVisible);
			//logger.debug("GraphEditor: AddDefaultBehaviors, tooltipBehavior added.");
			TooltipBehavior tooltipBehaviorObject = new TooltipBehavior();
			tooltipBehaviorObject.setTooltipFlareVisible(tooltipFlareVisible);
			addViewBehavior(tooltipBehaviorObject); 
		}	else {
			TooltipBehavior tooltipBehaviorObject = new TooltipBehavior();
			tooltipBehaviorObject.setTooltipFlareVisible(tooltipFlareVisible);
			removeViewBehavior(tooltipBehaviorObject);
		}

		//

		addViewBehavior(new VisibilityDropBehavior());
		addViewBehavior(new OverviewCameraBehavior());
		// TODO Seth & Chris
		// remove this line to use the demodecorator
		//		addDecorator(new DemoDecorator());
	}


	protected void removeDefaultBehaviors() {
		//logger.debug("GraphEditor: removeDefaultBehaviors");
		Collection<ViewBehavior> temp = new LinkedList<ViewBehavior>(viewBehaviors);
		for (ViewBehavior behavior : temp) {
			//logger.debug(behavior);

		}
		//removeViewBehavior(behavior);
	}

	//





	protected void installRightClickBehaviors() {
		addMenuFactory(new LinkExpanderRightClickMenuFactory());
		addMenuFactory(new RootDisplayRightClickMenuFactory());
		// addMenuFactory(new SaveScreenMenuFactory());
	}

	public void init() {
		//logger.debug("GraphEditor: init");
		setDropTarget(dropTarget);

		GUIUtil.addReloadListener(reloadListener);
		Preferences.getPreferences().addReconfigListener(reconfigListener);
		updateDatasources();
		toolbar.updateGestureList();
		// Didn't help.  Was trying to fix issue where the first time you open the Graph Editor,
		// it is blank rather than showing the currently selected terms.
		//		reloadListener.reload(
		//		    new ReloadEvent(this, null, true, false, false, false, false)); // ?
		//		if (linkDatabase.getObjects().size() == 0) {
		//		    Collection<? extends LinkedObject> roots = TermUtil
		//			.getRoots(getRootAlgorithm(), getLinkDatabase());
		//		    logger.info("init: setVisibleObjects(" + roots); // DEL
		//		    setVisibleObjects(roots);
		//		}
	}

	public void cleanup() {
		//logger.debug("GraphEditor: cleanup()");
		super.cleanup();
		setDropTarget(null);

		GUIUtil.removeReloadListener(reloadListener);
		Preferences.getPreferences().removeReconfigListener(reconfigListener);
		Collection<ViewBehavior> temp = new LinkedList<ViewBehavior>(
				viewBehaviors);
		for (ViewBehavior behavior : temp) {
			removeViewBehavior(behavior);
		}
		linkDatabase = null;
	}

	public JComponent getComponent() {
		//logger.debug("GraphEditor: getComponent()");
		if (panel == null) {
			panel = new JPanel();
			panel.setLayout(new BorderLayout());
			panel.add(this, "Center");
			//logger.debug("GraphEditor: getComponent");
			toolbar = new EditActionToolbar(panel, getDragDropEditBehavior()
					.getInputListener(), false);
			toolbar.setToolbarPosition(BorderLayout.SOUTH);
		}
		return panel;
	}
	/**
	 * Takes the settings from the GraphEditor object and creates a GraphEditorConfiguration object that can be written to the 
	 * XML settings file or used to initialize the configuration panel. 
	 */
	public ComponentConfiguration getConfiguration() {
		//logger.debug("GraphEditor: getConfiguration()");
		String algorithmStr = null;
		if (getRootAlgorithm() == RootAlgorithm.STRICT)
			algorithmStr = "STRICT";
		else if (getRootAlgorithm() == RootAlgorithm.GREEDY)
			algorithmStr = "GREEDY";
		String basicHTML = null;
		if (getNodeLabelProvider() instanceof HTMLNodeLabelProvider) {
			basicHTML = ((HTMLNodeLabelProvider) getNodeLabelProvider())
			.getHtmlExpression();
		}
		InputHandlerI handler = toolbar.getCurrentHandler();
		String handlerID = handler == null ? null : handler.getID();
		return new GraphEditorConfiguration(getTermFilter(), getLinkFilter(),
				getObjectRenderers(), getLinkRenderers(), basicHTML,
				getDisableAnimations(), getLayoutDuration(), toolbar
				.getShowToolbar(), toolbar.getToolbarPosition(),
				handlerID, isRevertToDefaultAction(),
				isLive(), algorithmStr, isExpandSelectionPaths()
				, getTooltipBubbleVisible(), getTooltipFlareVisible()
		);
	}

	public String getID() {
		return id;
	}

	public boolean isSingleton() {
		return false;
	}

	/**
	 * Takes the configuration settings from the configuration object or from the XML file and applies them to the
	 * GraphEditor object.
	 */
	public void setConfiguration(ComponentConfiguration config) {
		if (config instanceof GraphEditorConfiguration) {
			//logger.debug("GraphEditor: setConfiguration()");
			GraphEditorConfiguration gec = (GraphEditorConfiguration) config;
			//			setLinkFilter(gec.getLinkFilter());
			//			setTermFilter(gec.getTermFilter());
			setFilters(gec.getTermFilter(), gec.getLinkFilter());  // avoids an unneeded reload
			setLinkRenderers(gec.getLinkRenderers());
			setObjectRenderers(gec.getObjectRenderers());
			setDisableAnimations(gec.getDisableAnimations());
			setLayoutDuration(gec.getLayoutDuration());
			setExpandSelectionPaths(gec.isExpandPaths());
			setHTMLExpression(gec.getBasicHTML());
			setTooltipBubbleVisible(gec.getTooltipBubbleVisible());
			setTooltipFlareVisible(gec.getTooltipFlareVisible());
			
			if (tooltipBubbleVisible){
				//logger.debug("GraphEditor: setConfiguration, " +
						//"tooltipBubbleVisible is true.");
				//logger.debug("GraphEditor: setConfiguration, tooltipBehavior added.");
				TooltipBehavior tooltipBehaviorObject = new TooltipBehavior();
				tooltipBehaviorObject.setTooltipFlareVisible(tooltipFlareVisible);
				addViewBehavior(tooltipBehaviorObject); 
			}
			
			setLive(gec.isLive());


			if (gec.getRootAlgorithm() != null) {
				if (gec.getRootAlgorithm().equals("STRICT")) {
					setRootAlgorithm(RootAlgorithm.STRICT);
				} else if (gec.getRootAlgorithm().equals("GREEDY")) {
					setRootAlgorithm(RootAlgorithm.GREEDY);
				}
			}
			setRevertToDefaultAction(gec.isRevertToDefaultAction());
			toolbar.setShowToolbar(gec.getShowToolbar());
			//logger.debug("GraphEditor.setConfig: NOT calling relayout"); // DEL
			//			relayout();    // !Need?  Or does the caller always end up calling relayout() sooner or later anyway?
		}
	}

	private void setTooltipFlareVisible(boolean tooltipFlareVisible) {
		//logger.debug("GraphEditor: setTooltipFlareVisible," +
				//" tooltipFlareVisible = " + tooltipFlareVisible);
		this.tooltipFlareVisible = tooltipFlareVisible;

	}

	private void setTooltipBubbleVisible(boolean tooltipBubbleVisible) {
		//logger.debug("GraphEditor: setTooltipBubbleVisible," +
				//" tooltipBubbleVisible = " + tooltipBubbleVisible);
		this.tooltipBubbleVisible = tooltipBubbleVisible;

	}

	private boolean getTooltipFlareVisible() {
		//logger.debug("GraphEditor: getTooltipFlareVisible," +
				//" tooltipFlareVisible = " + tooltipFlareVisible);
		return tooltipFlareVisible;

	}

	private boolean getTooltipBubbleVisible() {
		//logger.debug("GraphEditor: getTooltipBubbleVisible," +
				//" tooltipBubbleVisible = " + tooltipBubbleVisible);
		return tooltipBubbleVisible;		
	}

	public void setHTMLExpression(String htmlExpression) {
		if (getNodeLabelProvider() instanceof HTMLNodeLabelProvider) {
			((HTMLNodeLabelProvider) getNodeLabelProvider()).
			setHtmlExpression(htmlExpression);
		}
	}

	public void setXML(String xml) {
	}

	public boolean isXMLSettable() {
		return false;
	}

	public RootAlgorithm getRootAlgorithm() {
		return rootAlgorithm;
	}

	public void setRootAlgorithm(RootAlgorithm rootAlgorithm) {
		this.rootAlgorithm = rootAlgorithm;
	}

	protected boolean isRevertToDefaultAction() {
		return revertToDefaultAction;
	}

	protected void setRevertToDefaultAction(boolean revertToDefaultAction) {
		this.revertToDefaultAction = revertToDefaultAction;
	}

	@Override
	public Color getBackground() {
		// Using background color to indicate global vs. local selection mode.
		if (isLive())
			return Color.white;
		else
			return lockGray;
	}
}