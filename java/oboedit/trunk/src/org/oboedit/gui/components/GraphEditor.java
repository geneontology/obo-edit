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
import org.obo.filters.FilterPair;
import org.obo.filters.FilterPairImpl;
import org.obo.util.TermUtil;
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
import org.oboedit.graph.SaveScreenMenuFactory;
import org.oboedit.graph.SelectionBehavior;
import org.oboedit.graph.ToolbarBehavior;
import org.oboedit.graph.TooltipBehavior;
import org.oboedit.graph.ViewBehavior;
import org.oboedit.graph.VisibilityDropBehavior;
import org.oboedit.graph.ZoomWidgetBehavior;
import org.oboedit.gui.FilterComponent;
import org.oboedit.gui.LinkFilterEditorFactory;
import org.oboedit.gui.TermFilterEditorFactory;
import org.oboedit.gui.event.ReloadEvent;
import org.oboedit.gui.event.ReloadListener;
import org.oboedit.gui.event.SelectionEvent;
import org.oboedit.gui.event.SelectionListener;
import org.oboedit.util.GUIUtil;

public class GraphEditor extends LinkDatabaseCanvas implements GUIComponent {

	public static class GraphEditorConfiguration implements
			ComponentConfiguration {
		protected FilterPair filterPair;
		protected boolean disableAnimations;
		protected long layoutDuration = 1000;

		public GraphEditorConfiguration() {
		}

		public boolean getDisableAnimations() {
			return disableAnimations;
		}

		public void setDisableAnimations(boolean disableAnimations) {
			this.disableAnimations = disableAnimations;
		}

		public FilterPair getFilterPair() {
			return filterPair;
		}

		public void setFilterPair(FilterPair filterPair) {
			this.filterPair = filterPair;
		}

		public GraphEditorConfiguration(FilterPair filterPair,
				boolean disableAnimations, long layoutDuration) {
			super();
			this.filterPair = filterPair;
			this.disableAnimations = disableAnimations;
			this.layoutDuration = layoutDuration;
		}

		public long getLayoutDuration() {
			return layoutDuration;
		}

		public void setLayoutDuration(long layoutDuration) {
			this.layoutDuration = layoutDuration;
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

	protected ConfigurationPanel configPanel = new ConfigurationPanel() {

		@Override
		public void commit() {
			commitConfig();
		}

		@Override
		public void init() {
			initConfig();
		}

	};

	protected JCheckBox animationsBox = new JCheckBox("Enable animations");
	protected JSpinner animationDurationSpinner = new JSpinner(
			new SpinnerNumberModel(1000, 1, null, 100));

	protected ReloadListener reloadListener = new ReloadListener() {
		public void reload(ReloadEvent e) {
			if (e.isHistory() || e.isRoot() || e.isReasoner()) {
				updateDatasources();
				if (linkDatabase.getObjects().size() == 0) {
					Collection<? extends LinkedObject> roots = TermUtil
							.getRoots(getLinkDatabase());
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
	protected FilterComponent termFilterComponent;
	protected FilterComponent linkFilterComponent;
	protected JTabbedPane configTabbedPane = new JTabbedPane();
	protected JCheckBox linkFilterButton = new JCheckBox("Filter links");
	protected JCheckBox termFilterButton = new JCheckBox("Filter terms");
	protected JPanel termFilterPanel = new JPanel();
	protected JPanel linkFilterPanel = new JPanel();

	public ConfigurationPanel getConfigurationPanel() {
		return configPanel;
	}

	protected void createConfigurationPanel() {
		configPanel.setLayout(new BorderLayout());
		configPanel.removeAll();
		configPanel.add(configTabbedPane, "Center");
		configPanel.validate();

		termFilterPanel.setLayout(new BorderLayout());
		termFilterPanel.add(termFilterButton, "North");

		linkFilterPanel.setLayout(new BorderLayout());
		linkFilterPanel.add(linkFilterButton, "North");

		int durationHeight = (int) animationDurationSpinner.getPreferredSize()
				.getHeight();
		final Box animationDurationBox = new Box(BoxLayout.X_AXIS);
		animationDurationBox.add(new JLabel("Animation duration"));
		animationDurationBox.add(Box.createHorizontalStrut(10));
		animationDurationBox.add(animationDurationSpinner);
		animationDurationBox.add(new JLabel("ms"));
		animationDurationBox.add(Box.createHorizontalGlue());
		animationDurationBox.setMaximumSize(new Dimension(Integer.MAX_VALUE,
				durationHeight));
		// animationDurationSpinner.setMaximumSize(new Dimension(
		// Integer.MAX_VALUE, durationHeight));

		final JPanel guiConfigPanel = new JPanel();
		guiConfigPanel
				.setLayout(new BoxLayout(guiConfigPanel, BoxLayout.Y_AXIS));
		guiConfigPanel.add(animationsBox);
		guiConfigPanel.add(animationDurationBox);
		guiConfigPanel.add(Box.createVerticalGlue());

		animationsBox.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				animationDurationBox.setEnabled(animationsBox.isSelected());
				animationDurationSpinner.setEnabled(animationsBox.isSelected());
			}

		});

		termFilterComponent = new FilterComponent(new TermFilterEditorFactory());
		termFilterComponent.setBorder(new TitledBorder("Term Filter"));
		linkFilterComponent = new FilterComponent(new LinkFilterEditorFactory());
		linkFilterComponent.setBorder(new TitledBorder("Link Filter"));

		termFilterComponent.setButtonVisible(false);
		linkFilterComponent.setButtonVisible(false);

		termFilterComponent.setShowButton(false);

		termFilterButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				updateConfigFilterPanels();
			}
		});
		linkFilterButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				updateConfigFilterPanels();
			}
		});
		configTabbedPane.add(guiConfigPanel, "GUI Settings");
		configTabbedPane.add(termFilterPanel, "Term filtering");
		configTabbedPane.add(linkFilterPanel, "Link filtering");
	}

	protected void initConfig() {
		animationsBox.setSelected(!getDisableAnimations());
		animationDurationSpinner.setValue(getLayoutDuration());
		FilterPair pair = getFilter();
		linkFilterButton.setSelected(pair != null
				&& pair.getLinkFilter() != null);
		termFilterButton.setSelected(pair != null
				&& pair.getObjectFilter() != null);
		if (pair != null && pair.getLinkFilter() != null)
			linkFilterComponent.setFilter(pair.getLinkFilter());
		if (pair != null && pair.getObjectFilter() != null)
			termFilterComponent.setFilter(pair.getObjectFilter());
		updateConfigFilterPanels();
	}

	protected void commitConfig() {
		// animationsBox.addActionListener(new ActionListener() {
		//
		// public void actionPerformed(ActionEvent e) {
		// TooltipBehavior behavior = null;
		// for (ViewBehavior viewBehavior : getViewBehaviors()) {
		// if (viewBehavior instanceof TooltipBehavior) {
		// behavior = (TooltipBehavior) viewBehavior;
		// }
		// }
		// setDisableAnimations(!animationsBox.isSelected());
		// if (behavior != null) {
		// behavior
		// .setTooltipVisibleDelay(TooltipBehavior.DEFAULT_TOOLTIP_VISIBILITY_DELAY);
		// }
		// }
		// });
		setDisableAnimations(!animationsBox.isSelected());
		setLayoutDuration(((Number) animationDurationSpinner.getValue())
				.longValue());
		FilterPair filterPair = new FilterPairImpl();
		if (termFilterButton.isSelected()) {
			filterPair.setObjectFilter(termFilterComponent.getFilter());
		}
		if (linkFilterButton.isSelected()) {
			filterPair.setLinkFilter(linkFilterComponent.getFilter());
		}
		setFilter(filterPair);
	}

	protected void updateConfigFilterPanels() {
		if (termFilterButton.isSelected())
			termFilterPanel.add(termFilterComponent, "Center");
		else
			termFilterPanel.remove(termFilterComponent);
		if (linkFilterButton.isSelected())
			linkFilterPanel.add(linkFilterComponent, "Center");
		else
			linkFilterPanel.remove(linkFilterComponent);
		// termFilterPanel.validate();
		// linkFilterPanel.validate();
		configPanel.validate();
		configPanel.repaint();
	}

	public GraphEditor(String id) {
		this(id, new HierarchicalGraphLayout());
	}

	public GraphEditor(String id, GraphLayout graphLayout) {
		super(graphLayout);
		this.id = id;
		createConfigurationPanel();
	}

	protected void addDefaultBehaviors() {
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
		addViewBehavior(new DragDropEditBehavior());
		addViewBehavior(new OverviewCameraBehavior());
	}

	protected void installRightClickBehaviors() {
		addMenuFactory(new LinkExpanderRightClickMenuFactory());
		addMenuFactory(new RootDisplayRightClickMenuFactory());
		addMenuFactory(new SaveScreenMenuFactory());
	}

	public void init() {
		setDropTarget(dropTarget);
		SelectionManager.getManager().addSelectionListener(
				globalSelectionListener);
		addSelectionListener(globalSelectionNotifier);
		GUIUtil.addReloadListener(reloadListener);
		updateDatasources();
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
		}
		return panel;
	}

	public ComponentConfiguration getConfiguration() {
		return new GraphEditorConfiguration(getFilter(),
				getDisableAnimations(), getLayoutDuration());
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
			setFilter(gec.getFilterPair());
			setDisableAnimations(gec.getDisableAnimations());
			setLayoutDuration(gec.getLayoutDuration());
		}
	}

	public void setXML(String xml) {
	}

	public boolean isXMLSettable() {
		return false;
	}
}
