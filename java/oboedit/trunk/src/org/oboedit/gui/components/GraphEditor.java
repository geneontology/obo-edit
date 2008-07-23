package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

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

	public static class GraphEditorConfiguration extends
			OntologyEditorConfiguration {
		protected boolean disableAnimations;
	        protected long layoutDuration = 500;  // milliseconds (user-configurable)
		protected boolean expandPaths = true;

		public GraphEditorConfiguration() {
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
				boolean expandPaths) {
			super(termFilter, linkFilter, objectRenderers, linkRenderers,
					basicHTML, showToolbar, toolbarPosition, dragActionID,
					revert, live, rootAlgorithm);
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
		return new OntologyEditorConfigEditor() {
			protected JCheckBox animationsBox;
			protected JSpinner animationDurationSpinner;
			protected JCheckBox expandSelectionPathsBox;

			@Override
			protected JComponent createGUIConfigPanel() {
				animationDurationSpinner = new JSpinner(new SpinnerNumberModel(
						1000, 1, null, 100));
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

				out.add(animationsBox);
				out.add(animationDurationBox);
				out.add(expandSelectionPathsBox);
				out.validate();
				return out;
			}

			@Override
			public void commitConfig(OntologyEditorConfiguration c) {
				GraphEditorConfiguration config = (GraphEditorConfiguration) c;
				config.setDisableAnimations(!animationsBox.isSelected());
				config.setLayoutDuration(((Number) animationDurationSpinner
						.getValue()).longValue());
				config.setExpandPaths(expandSelectionPathsBox.isSelected());
				super.commitConfig(c);
			}

			@Override
			protected void initConfig(OntologyEditorConfiguration c) {
				GraphEditorConfiguration config = (GraphEditorConfiguration) c;
				animationsBox.setSelected(!config.getDisableAnimations());
				animationDurationSpinner.setValue(config.getLayoutDuration());
				expandSelectionPathsBox.setSelected(config.isExpandPaths());
				super.initConfig(config);
			}
		};
	}

	public GraphEditor(String id) {
		this(id, new HierarchicalGraphLayout());
	}

	public GraphEditor(String id, GraphLayout graphLayout) {
		super(graphLayout);
		this.id = id;
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
		addViewBehavior(new TooltipBehavior());
		addViewBehavior(new VisibilityDropBehavior());
		addViewBehavior(new OverviewCameraBehavior());
		// TODO Seth & Chris
		// remove this line to use the demodecorator
//		addDecorator(new DemoDecorator());
	}

	protected void installRightClickBehaviors() {
		addMenuFactory(new LinkExpanderRightClickMenuFactory());
		addMenuFactory(new RootDisplayRightClickMenuFactory());
		// addMenuFactory(new SaveScreenMenuFactory());
	}

	public void init() {
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
		if (panel == null) {
			panel = new JPanel();
			panel.setLayout(new BorderLayout());
			panel.add(this, "Center");
			toolbar = new EditActionToolbar(panel, getDragDropEditBehavior()
					.getInputListener(), false);
			toolbar.setToolbarPosition(BorderLayout.SOUTH);
		}
		return panel;
	}
	
	public ComponentConfiguration getConfiguration() {
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
				isLive(), algorithmStr, isExpandSelectionPaths());
	}

	public String getID() {
		return id;
	}

	public boolean isSingleton() {
		return false;
	}

	public void setConfiguration(ComponentConfiguration config) {
		if (config instanceof GraphEditorConfiguration) {
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
			System.out.println("GraphEditor.setConfig: NOT calling relayout"); // DEL
//			relayout();    // !Need?  Or does the caller always end up calling relayout() sooner or later anyway?
		}
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
