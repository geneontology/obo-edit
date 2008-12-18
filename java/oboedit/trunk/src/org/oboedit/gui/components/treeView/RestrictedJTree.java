package org.oboedit.gui.components.treeView;

import java.awt.event.MouseEvent;
import java.util.HashSet;
import java.util.Set;

import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;

import org.apache.log4j.*;

/**
 * @author John Day-Richer, Jennifer Deegan, and Nicolas Rodriguez.<br>
 * Docs by Jennifer Deegan and Nicolas Rodriguez.<br>
 * An extension of JTree, this class enables the component to open only some of the branches of the tree, for example so that only the first
 * instance of a given term will be opened out, and the others will have their branches closed up. In a normal JTree the options would be for the 
 * branches to be either all open or all closed. 
 */

public class RestrictedJTree extends JTree {
	//initialize logger
	protected final static Logger logger = Logger.getLogger(RestrictedJTree.class);
	
	TreeViewSettings treeViewSettingsInstance;
	TreePath treePathInstance;

	private static final long serialVersionUID = 1L;

	//I added this constructor.
	public RestrictedJTree(TreeViewSettings treeViewSettingsInstance) {
		super();
		this.treeViewSettingsInstance = treeViewSettingsInstance;
		//logger.debug("RestrictedJTree constructor: treeViewSettingsInstance = " + treeViewSettingsInstance);
	}

	/**
	 */
	protected Runnable visibleRunnable = new Runnable() {
		public void run() {
			makeVisible(treePathInstance);
			//logger.debug("RestrictedJTree: visibleRunnable: treePathInstance is " + treePathInstance);
		}
	};


	public void refresh() {
		refresh(false);
		//logger.debug("RestrictedJTree: refresh method.");	
	}

	boolean expandAllowed = true;

	/**
	 * @param fromThread
	 * This seems to redraw the display of the tree in the component window. 
	 * (unless there is nothing selected)
	 * 
	 */
	public void refresh(boolean fromThread) {
		//logger.debug("RestrictedJTree: refresh method with " + fromThread + " arg.");

		if (getModel() == null)
			return;
		//logger.debug("RestrictedJTree: refresh method : treeModelInstance != null.");
		expandAllowed = true;
		expandPaths(fromThread);
		expandAllowed = false;
		repaint(); //If this line is commented out then selecting terms in the OTE does not lead to them showing
		//the Tree Viewer.
	}

	/**
	 * @param fromThread
	 *This seems to expand the paths to the term that has been selected. 
	 */
	protected void expandPaths(boolean fromThread) {
		logger.debug("RestrictedJTree.expandPaths");
		Set<Object> seenem = new HashSet<Object>();
		Object root = getModel().getRoot();
		expandPaths(null, root, seenem, fromThread); // If this line is commented out then the tree does not expand.
	}

	/**
	 * @param parentPath
	 * @param o
	 * @param seenem
	 * @param fromThread
	 * This is another version of expandPaths that had more arguments. 
	 */
	protected void expandPaths(TreePath parentPath, Object o,
			Set<Object> seenem, boolean fromThreads) {
		//logger.debug("RestrictedJTree: expandPaths method with several args.");	
		if (o == null){
			return;
		}
		//I changed trimPaths() to treeViewSettingsInstance.getTrimPaths()
		if (treeViewSettingsInstance.getTrimPaths() && seenem.contains(o)) {
			logger.debug("RestrictedJTree: expandPaths : recursion ended.");
			return;
		}
		seenem.add(o);
		TreePath path;
		if (parentPath == null)
			path = new TreePath(o);
		else {
			path = parentPath.pathByAddingChild(o);
		}
		makeVisible(path);

		//			  try { SwingUtilities.invokeLater(new VisibleRunnable(treePathInstance)); }
		//			  catch (Exception ex) {}
		//			 
		int childCount = getModel().getChildCount(o);
		for (int i = 0; i < childCount; i++) {
			expandPaths(path, getModel().getChild(o, i), seenem, fromThreads);
		}
	}

	/**
	 * This method produces the tool tips for mouseover. 
	 */

	@Override
	public String getToolTipText(MouseEvent e) {
		TreePath path = getPathForLocation(e.getX(), e.getY());
		if (path == null)
			return null;
		Object o = path.getLastPathComponent();
		if (!(o instanceof Link))
			return null;
		LinkedObject child = ((Link) o).getChild();
		//logger.debug("RestrictedJTree: getToolTipText method.");	
		return child.getID();
	}

	/**
	 * makeVisible method is duplicated from VisibleRunnable and maybe should  be 
	 * taken out into an abstract class so they can share it. 
	 * It overrides the version in JTree,
	 * which is explained at http://java.sun.com/j2se/1.4.2/docs/api/javax/swing/JTree.html
	 * 
	 * It's not clear to me yet what super.makeVisible(treePathInstance) is for.
	 *
	 */
	@Override
	public void makeVisible(TreePath path) {
		//logger.debug("RestrictedJTree: makeVisible method.");
		//logger.debug("RestrictedJTree: makeVisible method: path = " + path);
		expandAllowed = true;		//If these two lines are commented out, then when I click a term in the OTE
		super.makeVisible(path);	//it appears closed up in the TreeViewer, and I have to click the plus signs to see the children.  
		expandAllowed = false;	//If this line is commented out it doesn't seem to make any difference.

	}

	//		public void expandPath(TreePath treePathInstance) {
	//		if (expandAllowed) {
	//			super.expandPath(treePathInstance);
	//		}
	//	}
	//
	//	public void expandRow(int row) {
	//		if (expandAllowed)
	//			super.expandRow(row);
	//	}
	//
	//	public void collapsePath(TreePath treePathInstance) {
	//	}
	//
	//	public void collapseRow(int row) {
	//	}

}
