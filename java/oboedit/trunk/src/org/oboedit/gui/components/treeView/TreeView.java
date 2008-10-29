package org.oboedit.gui.components.treeView;

/** This class used to be called DAGView */

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JScrollPane;
import javax.swing.JTree;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;
import javax.swing.event.MouseInputAdapter;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeSelectionModel;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import org.apache.log4j.Logger;
import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.ConfigurationPanel;
import org.bbop.swing.BackgroundEventQueue;
import org.bbop.swing.PathTreeModel;
import org.bbop.swing.ProgressBarUpdateRunnable;
import org.bbop.swing.plaf.DragFriendlyTreeUI;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.RootAlgorithm;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.OBOCellRenderer;
import org.oboedit.gui.PathTask;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.event.ReconfigEvent;
import org.oboedit.gui.event.ReconfigListener;
import org.oboedit.gui.event.ReloadEvent;
import org.oboedit.gui.event.ReloadListener;
import org.oboedit.gui.event.SelectionEvent;
import org.oboedit.gui.event.SelectionListener;
import org.oboedit.util.GUIUtil;
import org.oboedit.util.PathUtil;

/**
 * 
  * @author John Day-Richter, Jennifer Deegan, and Nicolas Rodriguez.<br>
 * Docs by Jennifer Deegan and Nicolas Rodriguez.<br>
 *<br>
 *
 *This is the main part of the component and includes the tie-ins to the configuration settings in the JavaBean TreeViewSettings.java and
 *to the configuration panel in TreeViewConfigPanel.java. <br>
 *
 *AbstractGUIComponent docs at:<br>
 *http://oboedit.org/api/bbop/org/bbop/framework/AbstractGUIComponent.html
 *
 */

public class TreeView extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TreeView.class);

	private static final long serialVersionUID = 1L;
	protected JScrollPane scrollPane;
	protected JLabel emptyLabel = new JLabel("No terms selected");
	protected JLabel statusLabel = new JLabel("No paths loaded");
	protected JProgressBar progressBar = new JProgressBar();
	protected TreeViewSettings treeViewSettingsInstance;
	TreeViewConfigPanel treeViewConfigPanelInstance;
	public RestrictedJTree restrictedJTreeInstance;
	protected BackgroundEventQueue eventQueue;

	

	/**
	 * @param id
	 *
	 * The constructor inherits the id variable.
	 * A new BackgroundEventQueue is initiated.
	 * The progress bar setStringPainted boolean is set to true so that a percentage value will be displayed to show 
	 * percentage of process complete on the progress bar. 
	 * An actionListener is added.
	 * 
	 * Addition of the startupNotifier is to get the backgroundEventThread, and the progress bar is added to this. 
	 * 
	 * If I move the contents of init into here then the Tree Viewer doesn't open at all any more. 
	 * 
	 */
	public TreeView(String id) {
		super(id);
		//logger.debug("TreeView Constructor: id = " + id);
		//logger.debug("TreeView: constructor.");

		//logger.debug("TreeView: constructor, treeViewInstance = " + this);

		treeViewSettingsInstance = new TreeViewSettings();
		eventQueue = new BackgroundEventQueue();
		progressBar.setStringPainted(true);
		ActionListener updateListener = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				update();
			}
		};
		eventQueue.addStartupNotifier(new ProgressBarUpdateRunnable(eventQueue, progressBar));
	}


	
	
	/**
	 * 	  
	 * Returns whether or not the component is set up to carry over multiple selection of terms from the Ontology Tree Editor.
	 *
	 * @return whether or not the component is set up to carry over multiple selection of terms from the Ontology Tree Editor.
	 */
	//Method that returns a boolean. Returns the boolean for the current settings object.
	protected boolean multiTerm() {
		//logger.debug("TreeView: multiTerm method.");
		return treeViewSettingsInstance.getMultiSelect();
	}

	/**
	 * Listens for selection of a different term in any other component. 
	 * <br/>
	 * Enables the term selected in another component to be displayed in the Tree View. 
	 * Updates the Tree View to show the newly selected terms. 
	 * 
	 */
	//This is a variable of the class TreeView but it also has an inner class. 	
	protected SelectionListener selectionListener = new SelectionListener() {
		//SelectionEvent is a class that can contain a term or list of terms that has been selected. 
		//Why is 'e' not used?
		public void selectionChanged(SelectionEvent e) {
			//logger.debug("\n\n\nTreeView: SelectionChanged: SelectionEvent = " + e.getSelection());
			//logger.debug("TreeView: selectionListener method.");
			update();
		}
	};
	
	
	/**
	 * Listens for undo or redo command that would require the Tree Viewer to reload to a different state.
	 */
	protected ReloadListener historyListener = new ReloadListener() {
		public void reload(ReloadEvent e) {
			update();
		}
	};

	
	/**
	 * Declares a TreeModel variable pointing to a TreeModel that will hold information about the ontology tree that is selected.
	 */
	TreeModel model;

	
	/**
	 * 
	 */
	ReconfigListener reconfigListener = new ReconfigListener() {
		public void configReloaded(ReconfigEvent e) {
			setToolTips();
			//logger.debug("TreeView: configReloaded method.");
		}
	};

	/**
	 * Passes on right or middle mouse clicks to send current selection to the Global Selection Listener.
	 * This is the listener that enables terms selected in the Tree Viewer to be displayed in all other
	 * components that are currently set on Global Selection. 
	 */
	MouseInputAdapter clickListener = new MouseInputAdapter() {
		@Override
		public void mouseClicked(MouseEvent e) {
			//logger.debug("TreeView: mouseClicked method.");
			if (SwingUtilities.isMiddleMouseButton(e)
					|| SwingUtilities.isRightMouseButton(e)) {
				TreePath path = restrictedJTreeInstance.getSelectionPath();
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


	
	/**
	 * This sets up the GUI of the main display area of the component, where the tree will be shown. 
	 * This is separate from the GUI of the config system, which is set up in TreeViewConfigPanel.  
	 */
	public void init() {
		//logger.debug("TreeView: init Method.");
		removeAll();
		DefaultTreeSelectionModel selectionModel = new DefaultTreeSelectionModel();
		
		//Enables the selection model to include selection of terms that are not in the same branch. 
		selectionModel
				.setSelectionMode(TreeSelectionModel.DISCONTIGUOUS_TREE_SELECTION);
		setLayout(new BorderLayout());
		setPreferredSize(new Dimension(200, 200));
		setOpaque(true);
		restrictedJTreeInstance = new RestrictedJTree(treeViewSettingsInstance);
		restrictedJTreeInstance.setUI(getDefaultUI());
		
		//Causes the terms names to be shown in the display as a JLabel in a coloured box. This is how the tree diagram is constructed.
		restrictedJTreeInstance.setCellRenderer(new OBOCellRenderer());  
		
		
		restrictedJTreeInstance.setSelectionModel(selectionModel);
		restrictedJTreeInstance.putClientProperty("JTree.lineStyle", "Angled");
		scrollPane = new JScrollPane(restrictedJTreeInstance);

		/*
		 * String multiSelectStr = props.getProperty("multiselect"); boolean
		 * multiSelect = false; if (multiSelectStr != null &&
		 * multiSelectStr.equals("true")) multiSelect = true;
		 * multiTermCheckbox.setSelected(multiSelect);
		 */
		JPanel controlPanel = new JPanel();
		controlPanel.setOpaque(false);
		controlPanel.setLayout(new BorderLayout());
		controlPanel.add(statusLabel, "Center");

		add("Center", scrollPane);
		add("South", controlPanel);
		emptyLabel.setHorizontalAlignment(SwingConstants.CENTER);

		restrictedJTreeInstance.setRootVisible(false);
		restrictedJTreeInstance.setShowsRootHandles(true);
		update();
		
		SelectionManager.getManager().addSelectionListener(selectionListener);
		Preferences.getPreferences().addReconfigListener(reconfigListener);
		GUIUtil.addReloadListener(historyListener);
		restrictedJTreeInstance.addMouseListener(clickListener);
		setToolTips();
	}

	
	/**
	 * Provides the name of the component.
	 * This doesn't seem to actually override anything. 
	 */
	@Override
	public String getName() {
		//logger.debug("TreeView: getName method.");

		return "Tree Viewer";
	}


	/**
	 * Part of the standard OBO-Edit component configuration system. 
	 * Calls the TreeViewConfigPanel, which returns a new treeViewConfigPanelInstance.
	 */
	@Override
	public ConfigurationPanel getConfigurationPanel() {
		//logger.debug("TreeView: getConfigurationPanel()");

		//logger.debug("TreeView: getConfiguration: treeViewInstance = " + this);
		
		if (treeViewConfigPanelInstance == null) {
			treeViewConfigPanelInstance = new TreeViewConfigPanel(this);
		}

		return treeViewConfigPanelInstance;
	
	}

	/**
	 * Examines preferences, and according to results registers component as being configured to show tooltips on mouseover. 
	 */
	public void setToolTips() {
		//logger.debug("TreeView: setToolTips method.");

		if (Preferences.getPreferences().getShowToolTips())
			ToolTipManager.sharedInstance().registerComponent(restrictedJTreeInstance);
		else
			ToolTipManager.sharedInstance().unregisterComponent(restrictedJTreeInstance);
	}
	
	/**
	 * Part of the standard OBO-Edit component configuration system. 
	 * Calls the TreeViewSettings class, which returns a new treeViewSettingsInstance.
	 */
	@Override
	public ComponentConfiguration getConfiguration() {
		//logger.debug("TreeView: getConfiguration method : multiSelect = " + treeViewSettingsInstance.getMultiSelect());
		
		//logger.debug("TreeView: getConfiguration method: treeViewSettingsInstance = " + treeViewSettingsInstance);
		//logger.debug("TreeView: getConfiguration method: treeViewInstance = " + this);
		
		return treeViewSettingsInstance;
	}


/**
 * @param treeViewSettingsInstance
 * 
 * Sets the current treeViewSettings object to treeViewSettingsInstance for use in this class. 
 *
 */
	public void setConfiguration(ComponentConfiguration treeViewSettingsInstance) {
		//logger.debug("TreeView: setConfiguration method.");

		if (treeViewSettingsInstance != null && treeViewSettingsInstance instanceof TreeViewSettings) {
			this.treeViewSettingsInstance = (TreeViewSettings) treeViewSettingsInstance;
			update();
		}
	}


	/**
	 * Removes the selectionListener and the ReconfigListener and adds the ReloadListener. 
	 * 
	 * Adds a reloadListener which may be to reload the display undo or redo is used. 
	 * 
	 */	
	public void cleanup() {
		//logger.debug("TreeView: cleanup method.");

		SelectionManager.getManager()
		.removeSelectionListener(selectionListener);
		Preferences.getPreferences().removeReconfigListener(reconfigListener);
		GUIUtil.addReloadListener(historyListener);
	}

	
	/**
	 * Takes the full TreePath object that is the path to root, and the object listing the node(s) that have been selected
	 * and populates an object with the list of terms and relationships that are between the root and the selected node(s)
	 * 
	 * @param path A path in a tree.
	 * @param node A node in the tree that will be used to trim the path if it is present in the path.
	 * @return The trimmed path or the original path untrimmed if the node was not part of that path.
	 */
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
	
	/**
	 * 
	 * @param paths
	 *
	 * Checks if there are any terms selected in the TreePath[] that has been passed. 
	 * If there are then is sets up the progress bar to display progress on displaying the terms.
	 *
	 */
	public void finishUpdate(TreePath[] paths) {
		//logger.debug("TreeView: finishUpdate method.");

		if (paths.length == 0) {
			if (isAncestorOf(progressBar)) {
				remove(progressBar);
				validate();
			}

			if (isAncestorOf(scrollPane)) {
				remove(scrollPane);
			}
			validate();
		} else {
			progressBar.setString("Setting model...");
			progressBar.repaint();

			progressBar.setString("Selecting paths...");
			progressBar.repaint();
			for (int i = 0; i < paths.length; i++) {
				if (restrictedJTreeInstance.isVisible(paths[i]))
					restrictedJTreeInstance.addSelectionPath(paths[i]);
			}
			// restrictedJTreeInstance.setSelectionPaths(paths);
			if (isAncestorOf(progressBar)) {
				remove(progressBar);
				validate();
			}
			if (!isAncestorOf(scrollPane)) {
				add(scrollPane, "Center");
				validate();
			}
		}
		statusLabel.setText(paths.length + " path"
				+ (paths.length == 1 ? "" : "s") + " loaded.");

		expandAll(restrictedJTreeInstance); //Comment this line to make the 'collapse all but one instance of a term' option work. 
		validate();
		repaint();
	}
	
	public void expandAll(JTree tree) {
	    int row = 0;
	    while (row < tree.getRowCount()) {
	      tree.expandRow(row);
	      row++;
	      }
	    }


	  public void expandToLast(JTree tree) {
	    // expand to the last leaf from the root
	    DefaultMutableTreeNode  root;
	    root = (DefaultMutableTreeNode) tree.getModel().getRoot();
	    tree.scrollPathToVisible(new TreePath(root.getLastLeaf().getPath()));
	    }

	/**
	 * 
	 * Seems to make an array of the paths from root down to each selected node, and each node in between.
	 * 
	 */
	protected void doUpdate() {
		//logger.debug("TreeView: doUpdate method.");

		final PathTask task = new PathTask();
		task.addPostExecuteRunnable(new Runnable() {

			public void run() {
				//logger.debug("TreeView: doUpdate.task.AddPostExecuteRunnable : run method.");

				Collection<TreePath> pathc = task.getResults();
				Iterator<TreePath> it = pathc.iterator();
				while(it.hasNext()) {
					TreePath path = (TreePath) it.next();
					if (PathUtil.pathIsCircular(path)
							|| (!treeViewSettingsInstance.getShowNonTransitive() == true && 
									PathUtil.pathContainsNonTransitive(path))) {  //changed this line.
						it.remove();
					}
				}
				TreePath [] paths = pathc.toArray(new TreePath[0]);
				model = new PathTreeModel(paths);
				restrictedJTreeInstance.setModel(model);
				//restrictedJTreeInstance.refresh(true); //If this line is commented 
				//out then the tree does not expand.
				finishUpdate(paths);
				
			}			
		});
		task.setLinkDatabase(SessionManager.getManager()
				.getCurrentLinkDatabase());

		Set<LinkedObject> termSet = new HashSet<LinkedObject>();
		//The line above makes an object that can contain a set of other objects. Since it is set to contain LinkedObjects 
		//this time it is able to contain the set of linked terms that will show in the graph. 
		// The set is called termSet.		
		
		if (multiTerm()) {
			termSet
			.addAll(SelectionManager.getGlobalSelection()
					.getTerms());
		} else {
			termSet.add(SelectionManager.getGlobalSelection()
					.getTermSubSelection());
		}
		
		task.setTerms(termSet);
		treeViewConfigPanelInstance.eventQueue.scheduleTask(task);
	
		restrictedJTreeInstance.refresh(true); //Added this so the tree just redraws once at the end
												//rather than continuously throughout.
	
	
	}

//	protected void showConfigurationWindow() {
//		logger.debug("TreeView: showConfigurationWindow method.");
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

	/**
	 * Validates and repaints, but may also run doUpdate depending on the state of global selection. 
	 */
	public void update() {
		//logger.debug("TreeView: update method.");
		//logger.debug("TreeView: update method: treeViewSettingsInstance = " + treeViewSettingsInstance);

		treeViewConfigPanelInstance.eventQueue.cancelAll();
		if (SelectionManager.getGlobalSelection().isEmpty()) {
			if (!isAncestorOf(treeViewConfigPanelInstance.emptyLabel)) {

				validate();
				repaint();
				//logger.debug("TreeView: update method, getGlobalSelection is empty and we have just validated.");
			}
		} else {

			
			doUpdate();
			validate();
			repaint();
		}
	}

	protected DragFriendlyTreeUI getDefaultUI() {
		//logger.debug("TreeView: getDefaultUI method.");

		DragFriendlyTreeUI ui = new DragFriendlyTreeUI();
		ui.setRightChildIndent(0);
		return ui;
	}


//	public JComponent getComponent() {
//		logger.debug("TreeView: getComponent method.");
//		return this;
//	}

	
	/**
	 * Allows asking whether XML is settable. Returns false. 
	 * 
	 */
	public boolean isXMLSettable() {
		//logger.debug("TreeView: isXMLSettable method.");
		return false;
	}

	/**
	 * 
	 * Doesn't do anything. 
	 * 
	 */
	public void setXML(String xml) {
		//logger.debug("TreeView: setXML method.");
	}
}
