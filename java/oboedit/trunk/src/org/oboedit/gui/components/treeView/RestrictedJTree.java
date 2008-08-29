package org.oboedit.gui.components.treeView;

import java.awt.event.MouseEvent;
import java.util.HashSet;
import java.util.Set;

import javax.swing.JTree;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;


/**
 * 
 * @author John Day-Richter
 * 
 * Docs by J. Deegan.
 * 29th August 2008.
 * 
 * RestrictedJTree class
 * 
 * 
 * 
 * 
 * 
 */

	public class RestrictedJTree extends JTree {
		
		TreeModel treeModelInstance;
		TreeViewSettings treeViewSettingsInstance;
		TreePath treePathInstance;
	
		
		private static final long serialVersionUID = 1L;
		
		//I added this constructor.
		public RestrictedJTree(TreeViewSettings treeViewSettingsInstance) {
			super();

			this.treeViewSettingsInstance = treeViewSettingsInstance;
			
			System.out.println("RestrictedJTree constructor: treeViewSettingsInstance = " + treeViewSettingsInstance);
		}
		

		/**
		 * Why does this instantiation of a visibleRunnable object have the run() method in it,
		 * when this method is already present in the visibleRunnable code?
		 * The run method calls makeVisible, which then calls makeVisible. Not sure why.
		 */
		protected Runnable visibleRunnable = new Runnable() {
			public void run() {
				makeVisible(treePathInstance);
				System.out.println("RestrictedJTree: visibleRunnable: treePathInstance is " + treePathInstance);
			}
		};

		
		public void refresh() {
			refresh(false);
			System.out.println("RestrictedJTree: refresh method.");	
		}

		boolean expandAllowed = false;
		
		/**
		 * 
		 * @param fromThread
		 * 
		 * This seems to redraw the display of the tree in the component window. 
		 * (unless there is nothing selected)
		 * 
		 */
		public void refresh(boolean fromThread) {
			if (treeModelInstance == null)
				return;
			expandAllowed = true;
			expandPaths(fromThread);
			expandAllowed = false;
			repaint();
			System.out.println("RestrictedJTree: refresh method with fromThread arg.");	

		}

		/**
		 * 
		 * @param fromThread
		 *
		 *This seems to expand the paths to the term that has been selected. 
		 *
		 */
		protected void expandPaths(boolean fromThread) {
			Set<Object> seenem = new HashSet<Object>();
			Object root = getModel().getRoot();
			expandPaths(null, root, seenem, fromThread);
			System.out.println("RestrictedJTree: expandPaths method.");	

		}

		/**
		 * 
		 * @param parentPath
		 * @param o
		 * @param seenem
		 * @param fromThread
		 *
		 * This is another version of expandPaths that had more arguments. 
		 *
		 */
		protected void expandPaths(TreePath parentPath, Object o,
				Set<Object> seenem, boolean fromThread) {

			System.out.println("RestrictedJTree: expandPaths method with several args.");	

			//I changed trimPaths() to treeViewSettingsInstance.getTrimPaths()
			if (treeViewSettingsInstance.getTrimPaths() && seenem.contains(o)) {
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
			 * try { SwingUtilities.invokeLater(new VisibleRunnable(treePathInstance)); }
			 * catch (Exception ex) {}
			 */
			int childCount = getModel().getChildCount(o);
			for (int i = 0; i < childCount; i++) {
				expandPaths(path, getModel().getChild(o, i), seenem, fromThread);
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
			System.out.println("RestrictedJTree: getToolTipText method.");	

			return child.getID();
		}

		/**
		 * makeVisible method is duplicated from VisibleRunnable and maybe should  be 
		 * taken out into an abstract class so they can share it. 
		 * It overrides the version in JTree, though I cannot access the code of JTree to see what is changed.
		 *
		 * It's not clear to me yet what super.makeVisible(treePathInstance) is for.
		 *
		 */
		@Override
		public void makeVisible(TreePath path) {
			expandAllowed = true;
			super.makeVisible(path);
			expandAllowed = false;
			System.out.println("RestrictedJTree: makeVisible method.");	
			System.out.println("makeVisible method: path = " + path);

		}
		/*
		 * public void expandPath(TreePath treePathInstance) { if (expandAllowed)
		 * super.expandPath(treePathInstance); }
		 * 
		 * public void expandRow(int row) { if (expandAllowed)
		 * super.expandRow(row); }
		 * 
		 * public void collapsePath(TreePath treePathInstance) { }
		 * 
		 * public void collapseRow(int row) { }
		 */
	}
