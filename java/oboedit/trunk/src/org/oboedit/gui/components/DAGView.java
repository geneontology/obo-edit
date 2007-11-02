package org.oboedit.gui.components;

import javax.swing.*;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.GUIManager;
import org.bbop.swing.*;
import org.bbop.swing.plaf.DragFriendlyTreeUI;
import org.bbop.util.*;
import org.obo.datamodel.*;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.gui.event.*;
import org.oboedit.util.PathUtil;

import java.util.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.tree.*;
import javax.swing.event.*;

public class DAGView extends AbstractGUIComponent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected RestrictedJTree tree;

	protected JScrollPane pane;

	protected JLabel emptyLabel = new JLabel("No terms selected");

	protected JLabel statusLabel = new JLabel("No paths loaded");

	protected JCheckBox multiTermCheckbox = new JCheckBox(
			"Show paths to multiple selected terms");

	protected JCheckBox trimPathsCheckbox = new JCheckBox(
			"Collapse already shown paths");

	protected JCheckBox showNonTransitiveCheckbox = new JCheckBox(
			"Show non-transitive paths");

	protected JButton configButton = new JButton("Config");

	protected JProgressBar progressBar = new JProgressBar();

	protected SessionManager sessionManager = SessionManager.getManager();

	public static class DAGViewConfig implements ComponentConfiguration {
		protected boolean multiSelect = false;

		protected boolean trimPaths = true;

		protected boolean showNonTransitive = false;

		public DAGViewConfig() {
		}

		public void setShowNonTransitive(boolean showNonTransitive) {
			this.showNonTransitive = showNonTransitive;
		}

		public boolean getShowNonTransitive() {
			return showNonTransitive;
		}

		public void setMultiSelect(boolean multiSelect) {
			this.multiSelect = multiSelect;
		}

		public boolean getMultiSelect() {
			return multiSelect;
		}

		public void setTrimPaths(boolean trimPaths) {
			this.trimPaths = trimPaths;
		}

		public boolean getTrimPaths() {
			return trimPaths;
		}
	}

	protected DAGViewConfig configuration = new DAGViewConfig();

	protected boolean multiTerm() {
		return multiTermCheckbox.isSelected();
	}

	protected SelectionListener selectionListener;

	protected HistoryListener historyListener;

	TreeModel model;

	ReconfigListener reconfigListener = new ReconfigListener() {
		public void configReloaded(ReconfigEvent e) {
			setToolTips();
		}
	};

	MouseInputAdapter clickListener = new MouseInputAdapter() {
		@Override
		public void mouseClicked(MouseEvent e) {
			if (SwingUtilities.isMiddleMouseButton(e)
					|| SwingUtilities.isRightMouseButton(e)) {
				TreePath path = tree.getSelectionPath();
				TreePath[] paths = { path };
				SelectionManager.setGlobalSelection(SelectionManager
						.createSelectionFromPaths(DAGView.this, paths, null,
								SessionManager.getManager()
										.getCurrentLinkDatabase(),
								RootAlgorithm.GREEDY, true));
				/*
				 * if (path != null) { Link link = (Link)
				 * path.getLastPathComponent(); TreePath newpath = TermUtil.
				 * getShortestPath(link, RootAlgorithm.GREEDY, controller.
				 * getCurrentLinkDatabase()); TreePath [] paths = { newpath };
				 * controller.getPrimarySelector().select(paths); }
				 */
			}
		}
	};

	protected BackgroundEventQueue eventQueue;

	@Override
	public String getName() {
		return "DAG Viewer";
	}

	public ComponentConfiguration getConfiguration() {
		configuration.setMultiSelect(multiTermCheckbox.isSelected());
		configuration.setTrimPaths(trimPaths());
		configuration.setShowNonTransitive(showNonTransitive());
		return configuration;
	}

	public void setConfiguration(ComponentConfiguration configuration) {
		if (configuration instanceof DAGViewConfig)
			this.configuration = (DAGViewConfig) configuration;

		multiTermCheckbox.setSelected(this.configuration.getMultiSelect());
		trimPathsCheckbox.setSelected(this.configuration.getTrimPaths());
		showNonTransitiveCheckbox.setSelected(this.configuration
				.getShowNonTransitive());
	}

	public DAGView(String id) {
		super(id);
		eventQueue = new BackgroundEventQueue();
		trimPathsCheckbox
				.setToolTipText("Collapse parts of paths that have already been shown to greatly speed up DAG Viewer redraws.");
		multiTermCheckbox
				.setToolTipText("If multiple terms are selected, show paths to all of them.");
		showNonTransitiveCheckbox
				.setToolTipText("Show paths that contain non-transitive relationships. Enabling this option will cause the view to become very confusing in some ontologies");
		progressBar.setStringPainted(true);
		multiTermCheckbox.setOpaque(false);
		trimPathsCheckbox.setOpaque(false);
		showNonTransitiveCheckbox.setOpaque(false);
		ActionListener updateListener = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				update();
			}
		};
		multiTermCheckbox.addActionListener(updateListener);
		trimPathsCheckbox.addActionListener(updateListener);
		showNonTransitiveCheckbox.addActionListener(updateListener);
		configButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				showConfigurationWindow();
			}
		});
		eventQueue.addStartupNotifier(new ProgressBarUpdateRunnable(eventQueue, progressBar));
	}

	public void setToolTips() {
		if (Preferences.getPreferences().getShowToolTips())
			ToolTipManager.sharedInstance().registerComponent(tree);
		else
			ToolTipManager.sharedInstance().unregisterComponent(tree);
	}

	public void init() {
		removeAll();
		DefaultTreeSelectionModel selectionModel = new DefaultTreeSelectionModel();
		selectionModel
				.setSelectionMode(TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
		setLayout(new BorderLayout());
		setPreferredSize(new Dimension(200, 200));
		setOpaque(true);
		tree = new RestrictedJTree();
		tree.setUI(getDefaultUI());
		tree.setCellRenderer(new OBOCellRenderer());
		tree.setSelectionModel(selectionModel);
		tree.putClientProperty("JTree.lineStyle", "Angled");
		pane = new JScrollPane(tree);

		/*
		 * String multiSelectStr = props.getProperty("multiselect"); boolean
		 * multiSelect = false; if (multiSelectStr != null &&
		 * multiSelectStr.equals("true")) multiSelect = true;
		 * multiTermCheckbox.setSelected(multiSelect);
		 */
		JPanel controlPanel = new JPanel();
		controlPanel.setOpaque(false);
		controlPanel.setLayout(new BorderLayout());
		// controlPanel.add(checkboxPanel, "East");
		controlPanel.add(configButton, "East");
		controlPanel.add(statusLabel, "Center");

		add("Center", pane);
		add("South", controlPanel);
		emptyLabel.setHorizontalAlignment(SwingConstants.CENTER);

		tree.setRootVisible(false);
		tree.setShowsRootHandles(true);
		update();
		selectionListener = new SelectionListener() {
			public void selectionChanged(SelectionEvent e) {
				update();
			}
		};
		historyListener = new HistoryListener() {

			public void applied(HistoryAppliedEvent event) {
				update();
			}

			public void reversed(HistoryAppliedEvent event) {
				update();
			}

		};

		SelectionManager.getManager().addSelectionListener(selectionListener);
		Preferences.getPreferences().addReconfigListener(reconfigListener);
		sessionManager.addHistoryListener(historyListener);
		tree.addMouseListener(clickListener);
		setToolTips();
	}

	public void cleanup() {
		SelectionManager.getManager()
				.removeSelectionListener(selectionListener);
		Preferences.getPreferences().removeReconfigListener(reconfigListener);
		sessionManager.removeHistoryListener(historyListener);
	}

	protected TreePath trimPathToNode(TreePath path, Object node) {
		Object[] objects = path.getPath();
		int length = 0;
		boolean stop = false;
		for (int i = objects.length - 1; i >= 0; i--) {
			if (objects[i].equals(node)) {
				if (stop) {
					length = i + 1;
					break;
				} else
					stop = true;
			}
		}
		if (length == 0)
			return path;
		else {
			Object[] nodes = new Object[length];
			for (int i = 0; i < length; i++)
				nodes[i] = objects[i];
			return new TreePath(nodes);
		}
	}

	public void finishUpdate(TreePath[] paths) {
		if (paths.length == 0) {
			if (isAncestorOf(progressBar)) {
				remove(progressBar);
				validate();
			}

			if (isAncestorOf(pane)) {
				remove(pane);
			}
			validate();
		} else {
			progressBar.setString("Setting model...");
			progressBar.repaint();

			progressBar.setString("Selecting paths...");
			progressBar.repaint();
			for (int i = 0; i < paths.length; i++) {
				if (tree.isVisible(paths[i]))
					tree.addSelectionPath(paths[i]);
			}
			// tree.setSelectionPaths(paths);
			if (isAncestorOf(progressBar)) {
				remove(progressBar);
				validate();
			}
			if (!isAncestorOf(pane)) {
				add(pane, "Center");
				validate();
			}
		}
		statusLabel.setText(paths.length + " path"
				+ (paths.length == 1 ? "" : "s") + " loaded.");
		repaint();
	}
	
	protected void doUpdate() {
		final PathTask task = new PathTask();
		task.addPostExecuteRunnable(new Runnable() {

			public void run() {
				Collection<TreePath> pathc = task.getResults();
				Iterator<TreePath> it = pathc.iterator();
				while(it.hasNext()) {
					TreePath path = (TreePath) it.next();
					if (PathUtil.pathIsCircular(path)
							|| (!showNonTransitive() && PathUtil
									.pathContainsNonTransitive(path))) {
						it.remove();
					}
				}
				TreePath [] paths = pathc.toArray(new TreePath[0]);
				model = new PathTreeModel(paths);
				tree.setModel(model);
				tree.refresh(true);
				finishUpdate(paths);
			}			
		});
		task.setLinkDatabase(SessionManager.getManager()
				.getCurrentLinkDatabase());
		
		Set<LinkedObject> termSet = new HashSet<LinkedObject>();
		if (multiTerm()) {
			termSet
					.addAll(SelectionManager.getGlobalSelection()
							.getTerms());
		} else
			termSet.add(SelectionManager.getGlobalSelection()
					.getTermSubSelection());
		
		task.setTerms(termSet);
		eventQueue.scheduleTask(task);
	}

	protected void showConfigurationWindow() {
		JButton closeButton = new JButton("Close");

		JPanel checkboxPanel = new JPanel();
		checkboxPanel.setLayout(new BoxLayout(checkboxPanel, BoxLayout.Y_AXIS));
		checkboxPanel.add(multiTermCheckbox);
		checkboxPanel.add(Box.createVerticalStrut(2));
		checkboxPanel.add(trimPathsCheckbox);
		checkboxPanel.add(Box.createVerticalStrut(2));
		checkboxPanel.add(showNonTransitiveCheckbox);
		checkboxPanel.add(Box.createVerticalStrut(10));
		checkboxPanel.add(closeButton);

		final JDialog dialog = new JDialog(GUIManager.getManager().getFrame(),
				true);
		closeButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				dialog.dispose();
			}
		});

		dialog.setContentPane(checkboxPanel);
		dialog.pack();
		dialog.setVisible(true);
	}

	public void update() {
		eventQueue.cancelAll();
		if (SelectionManager.getGlobalSelection().isEmpty()) {
			if (!isAncestorOf(emptyLabel)) {
				remove(pane);
				add(emptyLabel, "Center");
				validate();
				repaint();
			}
		} else {
			if (isAncestorOf(emptyLabel)) {
				remove(emptyLabel);
			}

			if (isAncestorOf(pane)) {
				remove(pane);
			}

			if (!isAncestorOf(progressBar)) {
				add(progressBar, "Center");
			}
			validate();
			repaint();
			doUpdate();
		}
	}

	protected DragFriendlyTreeUI getDefaultUI() {
		DragFriendlyTreeUI ui = new DragFriendlyTreeUI();
		ui.setRightChildIndent(0);
		return ui;
	}

	public boolean showNonTransitive() {
		return showNonTransitiveCheckbox.isSelected();
	}

	public boolean trimPaths() {
		return trimPathsCheckbox.isSelected();
	}

	private class RestrictedJTree extends JTree {
		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		protected class VisibleRunnable implements Runnable {
			protected TreePath path;

			public VisibleRunnable(TreePath path) {
				this.path = path;
			}

			public void run() {
				makeVisible(path);
			}
		}

		protected Runnable visibleRunnable = new Runnable() {
			public void run() {
				makeVisible(path);
			}
		};

		boolean expandAllowed = false;

		TreePath path;

		public void refresh() {
			refresh(false);
		}

		public void refresh(boolean fromThread) {
			if (model == null)
				return;
			expandAllowed = true;
			expandPaths(fromThread);
			expandAllowed = false;
			repaint();
		}

		protected void expandPaths(boolean fromThread) {
			Set<Object> seenem = new HashSet<Object>();
			Object root = getModel().getRoot();
			expandPaths(null, root, seenem, fromThread);
		}

		protected void expandPaths(TreePath parentPath, Object o,
				Set<Object> seenem, boolean fromThread) {
			if (trimPaths() && seenem.contains(o)) {
				return;
			}
			seenem.add(o);
			TreePath path;
			if (parentPath == null)
				path = new TreePath(o);
			else
				path = parentPath.pathByAddingChild(o);
			makeVisible(path);
			/*
			 * try { SwingUtilities.invokeLater(new VisibleRunnable(path)); }
			 * catch (Exception ex) {}
			 */
			int childCount = getModel().getChildCount(o);
			for (int i = 0; i < childCount; i++) {
				expandPaths(path, getModel().getChild(o, i), seenem, fromThread);
			}
		}

		@Override
		public String getToolTipText(MouseEvent e) {
			TreePath path = getPathForLocation(e.getX(), e.getY());
			if (path == null)
				return null;
			Object o = path.getLastPathComponent();
			if (!(o instanceof Link))
				return null;
			LinkedObject child = ((Link) o).getChild();
			return child.getID();
		}

		@Override
		public void makeVisible(TreePath path) {
			expandAllowed = true;
			super.makeVisible(path);
			expandAllowed = false;
		}
		/*
		 * public void expandPath(TreePath path) { if (expandAllowed)
		 * super.expandPath(path); }
		 * 
		 * public void expandRow(int row) { if (expandAllowed)
		 * super.expandRow(row); }
		 * 
		 * public void collapsePath(TreePath path) { }
		 * 
		 * public void collapseRow(int row) { }
		 */
	}

	public JComponent getComponent() {
		return this;
	}

	public boolean isXMLSettable() {
		return false;
	}

	public void setXML(String xml) {
	}
}
