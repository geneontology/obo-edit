package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.LinkedList;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSpinner;
import javax.swing.JTabbedPane;
import javax.swing.SpinnerModel;
import javax.swing.SpinnerNumberModel;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeListener;

import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.ConfigurationPanel;
import org.bbop.framework.GUIComponent;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.RootAlgorithm;
import org.obo.filters.Filter;
import org.obo.filters.FilterPair;
import org.obo.filters.FilterPairImpl;
import org.obo.util.TermUtil;
import org.oboedit.controller.EditActionManager;
import org.oboedit.controller.SelectionManager;
import org.oboedit.graph.BoundsGuarantor;
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
import org.oboedit.gui.FilterComponent;
import org.oboedit.gui.LinkFilterEditorFactory;
import org.oboedit.gui.TermFilterEditorFactory;
import org.oboedit.gui.event.ReloadEvent;
import org.oboedit.gui.event.ReloadListener;
import org.oboedit.gui.event.SelectionEvent;
import org.oboedit.gui.event.SelectionListener;
import org.oboedit.util.GUIUtil;

public class GraphEditor extends LinkDatabaseCanvas implements GUIComponent {

	public static class GraphEditorConfiguration extends
			OntologyEditorConfiguration {
		protected boolean disableAnimations;
		protected long layoutDuration = 1000;
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
				Filter<?> linkFilter, boolean disableAnimations,
				long layoutDuration, int showToolbar, String toolbarPosition,
				String dragActionID, boolean revert, boolean live,
				String rootAlgorithm, boolean expandPaths) {
			super(termFilter, linkFilter, showToolbar, toolbarPosition,
					dragActionID, revert, live, rootAlgorithm);
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

	protected SelectionListener globalSelectionListener = new SelectionListener() {
		public void selectionChanged(SelectionEvent e) {
			if (isLive) {
				removeSelectionListener(globalSelectionNotifier);
				select(e.getSelection());
				addSelectionListener(globalSelectionNotifier);
			}
		}
	};

	protected SelectionListener globalSelectionNotifier = new SelectionListener() {
		public void selectionChanged(SelectionEvent e) {
			if (isLive()) {
				SelectionManager.getManager().removeSelectionListener(
						globalSelectionListener);
				SelectionManager.setGlobalSelection(getSelection());
				SelectionManager.getManager().addSelectionListener(
						globalSelectionListener);
			}
		}
	};

	public String getTitle() {
		if (title == null)
			return getID();
		return title;
	}

	public boolean teardownWhenHidden() {
		return false;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	protected ReloadListener reloadListener = new ReloadListener() {
		public void reload(ReloadEvent e) {
			if (e.isHistory() || e.isRoot() || e.isReasoner()) {
				updateDatasources();
				if (linkDatabase.getObjects().size() == 0) {
					Collection<? extends LinkedObject> roots = TermUtil
							.getRoots(getRootAlgorithm(), getLinkDatabase());
					addPostLayoutAction(new Runnable() {

						public void run() {
							System.err.println("isLayoutOut = " + isLayingOut);
							panToObjects();
						}

					});
					setVisibleObjects(roots);
				}
			} else
				relayout();
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
		addViewBehavior(new SelectionBehavior());
		// addViewBehavior(new LinkoutMeterBehavior());
		addViewBehavior(new LinkButtonBehavior());
		addViewBehavior(new ZoomWidgetBehavior());
		addViewBehavior(new TooltipBehavior());
		addViewBehavior(new VisibilityDropBehavior());
		addViewBehavior(new OverviewCameraBehavior());
	}

	protected void installRightClickBehaviors() {
		addMenuFactory(new LinkExpanderRightClickMenuFactory());
		addMenuFactory(new RootDisplayRightClickMenuFactory());
//		addMenuFactory(new SaveScreenMenuFactory());
	}

	public void init() {
		setDropTarget(dropTarget);
		SelectionManager.getManager().addSelectionListener(
				globalSelectionListener);
		addSelectionListener(globalSelectionNotifier);
		GUIUtil.addReloadListener(reloadListener);
		updateDatasources();
		toolbar.updateGestureList();
	}

	public void cleanup() {
		setDropTarget(null);
		SelectionManager.getManager().removeSelectionListener(
				globalSelectionListener);
		removeSelectionListener(globalSelectionNotifier);
		GUIUtil.removeReloadListener(reloadListener);
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
		return new GraphEditorConfiguration(getTermFilter(), getLinkFilter(),
				getDisableAnimations(), getLayoutDuration(), toolbar
						.getShowToolbar(), toolbar.getToolbarPosition(),
				toolbar.getCurrentHandler().getID(), isRevertToDefaultAction(),
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
			setLinkFilter(gec.getLinkFilter());
			setTermFilter(gec.getTermFilter());
			setDisableAnimations(gec.getDisableAnimations());
			setLayoutDuration(gec.getLayoutDuration());
			setExpandSelectionPaths(gec.isExpandPaths());
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
}
