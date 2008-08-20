package org.oboedit.gui.components.treeView;

/** This class used to be called DAGView */

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
import org.oboedit.gui.components.graphvizViewer.GraphvizConfigPanel;
import org.oboedit.gui.components.graphvizViewer.GraphvizSettings;
import org.oboedit.gui.event.*;
import org.oboedit.util.GUIUtil;
import org.oboedit.util.PathUtil;
import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.ConfigurationPanel;


import java.util.*;
import java.awt.*;
import java.awt.event.*;

import javax.swing.tree.*;
import javax.swing.event.*;

import org.apache.log4j.*;

public class TreeView extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TreeView.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	protected JScrollPane pane;
	protected JLabel emptyLabel = new JLabel("No terms selected");
	protected JLabel statusLabel = new JLabel("No paths loaded");
	protected JButton configButton = new JButton("Config");
	protected JProgressBar progressBar = new JProgressBar();
	protected TreeViewSettings treeViewSettingsInstance = new TreeViewSettings();
	TreeViewConfigPanel treeViewConfigPanelInstance;
	public RestrictedJTree tree;


	protected boolean multiTerm() {
		//logger.debug("TreeView: multiTerm method.");
		return treeViewSettingsInstance.getMultiSelect();
	}

	protected SelectionListener selectionListener;
	protected ReloadListener historyListener;
	TreeModel model;

	ReconfigListener reconfigListener = new ReconfigListener() {
		public void configReloaded(ReconfigEvent e) {
			setToolTips();
			//logger.debug("TreeView: configReloaded method.");
		}
	};

	MouseInputAdapter clickListener = new MouseInputAdapter() {
		@Override
		public void mouseClicked(MouseEvent e) {
			//logger.debug("TreeView: mouseClicked method.");
			if (SwingUtilities.isMiddleMouseButton(e)
					|| SwingUtilities.isRightMouseButton(e)) {
				TreePath path = tree.getSelectionPath();
				TreePath[] paths = { path };
				SelectionManager.setGlobalSelection(SelectionManager
						.createSelectionFromPaths(TreeView.this, paths, null,
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


	
	
	@Override
	public String getName() {
//		return "DAG Viewer";
		//logger.debug("TreeView: getName method.");

		return "Tree Viewer";
	}


	@Override
	public ConfigurationPanel getConfigurationPanel() {
		//logger.debug("TreeView: getConfigurationPanel()");

		if (treeViewConfigPanelInstance == null) {
			treeViewConfigPanelInstance = new TreeViewConfigPanel(this);
		}

		return treeViewConfigPanelInstance;
	}

	public void setToolTips() {
		//logger.debug("TreeView: setToolTips method.");

		if (Preferences.getPreferences().getShowToolTips())
			ToolTipManager.sharedInstance().registerComponent(tree);
		else
			ToolTipManager.sharedInstance().unregisterComponent(tree);
	}

	
	@Override
	public ComponentConfiguration getConfiguration() {
		//logger.debug("TreeView: getConfiguration method.");
//		treeViewSettingsInstance.setMultiSelect(treeViewSettingsInstance.getMultiSelect());
//		treeViewSettingsInstance.setTrimPaths(trimPaths());
//		treeViewSettingsInstance.setShowNonTransitive(showNonTransitive());
		return treeViewSettingsInstance;
	}



	public void setConfiguration(TreeViewSettings treeViewSettingsInstance) {
		//logger.debug("TreeView: setConfiguration method.");

		if (treeViewSettingsInstance != null && treeViewSettingsInstance instanceof TreeViewSettings)
			this.treeViewSettingsInstance = (TreeViewSettings) this.treeViewSettingsInstance;

//		multiTermCheckbox.setSelected(this.treeViewSettingsInstance.getMultiSelect());
//		trimPathsCheckbox.setSelected(this.treeViewSettingsInstance.getTrimPaths());
//		showNonTransitiveCheckbox.setSelected(this.treeViewSettingsInstance
//		.getShowNonTransitive());
	}

	public TreeView(String id) {
		super(id);
		//logger.debug("TreeView: constructor.");

	}
	

	
	public void cleanup() {
		//logger.debug("TreeView: cleanup method.");

		SelectionManager.getManager()
		.removeSelectionListener(selectionListener);
		Preferences.getPreferences().removeReconfigListener(reconfigListener);
		GUIUtil.addReloadListener(historyListener);
	}

	protected TreePath trimPathToNode(TreePath path, Object node) {
		//logger.debug("TreeView: trimPathToNode method.");

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
		//logger.debug("TreeView: finishUpdate method.");

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
		//logger.debug("TreeView: doUpdate method.");

		final PathTask task = new PathTask();
		task.addPostExecuteRunnable(new Runnable() {

			public void run() {
				//logger.debug("TreeView: run method.");

				Collection<TreePath> pathc = task.getResults();
				Iterator<TreePath> it = pathc.iterator();
				while(it.hasNext()) {
					TreePath path = (TreePath) it.next();
					if (PathUtil.pathIsCircular(path)
							|| (!treeViewSettingsInstance.getShowNonTransitive() == true && PathUtil //changed this line.
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
		treeViewConfigPanelInstance.eventQueue.scheduleTask(task);
	}

//	protected void showConfigurationWindow() {
//		//logger.debug("TreeView: showConfigurationWindow method.");
//
//		JButton closeButton = new JButton("Close");
//
//		JPanel checkboxPanel = new JPanel();
//		checkboxPanel.setLayout(new BoxLayout(checkboxPanel, BoxLayout.Y_AXIS));
//		checkboxPanel.add(multiTermCheckbox);
//		checkboxPanel.add(Box.createVerticalStrut(2));
//		checkboxPanel.add(trimPathsCheckbox);
//		checkboxPanel.add(Box.createVerticalStrut(2));
//		checkboxPanel.add(showNonTransitiveCheckbox);
//		checkboxPanel.add(Box.createVerticalStrut(10));
//		checkboxPanel.add(closeButton);
//
//		final JDialog dialog = new JDialog(GUIManager.getManager().getFrame(),
//				true);
//		closeButton.addActionListener(new ActionListener() {
//			public void actionPerformed(ActionEvent e) {
//				dialog.dispose();
//			}
//		});
//
//		dialog.setContentPane(checkboxPanel);
//		dialog.pack();
//		dialog.setVisible(true);
//	}

	public void update() {
		//logger.debug("TreeView: update method.");

		treeViewConfigPanelInstance.eventQueue.cancelAll();
		if (SelectionManager.getGlobalSelection().isEmpty()) {
			if (!isAncestorOf(treeViewConfigPanelInstance.emptyLabel)) {
				remove(treeViewConfigPanelInstance.pane);
				add(treeViewConfigPanelInstance.emptyLabel, "Center");
				validate();
				repaint();
			}
		} else {
			if (isAncestorOf(treeViewConfigPanelInstance.emptyLabel)) {
				remove(treeViewConfigPanelInstance.emptyLabel);
			}

			if (isAncestorOf(treeViewConfigPanelInstance.pane)) {
				remove(treeViewConfigPanelInstance.pane);
			}

			if (!isAncestorOf(treeViewConfigPanelInstance.progressBar)) {
				add(treeViewConfigPanelInstance.progressBar, "Center");
			}
			validate();
			repaint();
			doUpdate();
		}
	}

	protected DragFriendlyTreeUI getDefaultUI() {
		//logger.debug("TreeView: getDefaultUI method.");

		DragFriendlyTreeUI ui = new DragFriendlyTreeUI();
		ui.setRightChildIndent(0);
		return ui;
	}

//	public boolean showNonTransitive() {
//		//logger.debug("TreeView: showNonTransitive method.");
//		return showNonTransitiveCheckbox.isSelected();
//	}
//
//	public boolean trimPaths() {
//		//logger.debug("TreeView: trimPaths method.");
//		return trimPathsCheckbox.isSelected();
//	}

//	public JComponent getComponent() {
//		//logger.debug("TreeView: getComponent method.");
//		return this;
//	}

	public boolean isXMLSettable() {
		//logger.debug("TreeView: isXMLSettable method.");
		return false;
	}

	public void setXML(String xml) {
		//logger.debug("TreeView: setXML method.");
	}
}
