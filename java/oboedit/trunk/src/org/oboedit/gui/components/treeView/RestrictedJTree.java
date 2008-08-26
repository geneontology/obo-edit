package org.oboedit.gui.components.treeView;

import java.awt.event.MouseEvent;
import java.util.HashSet;
import java.util.Set;

import javax.swing.JTree;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;

	public class RestrictedJTree extends JTree {
		
		TreeModel model;
		TreeViewSettings treeViewSettingsInstance;
		TreePath path;
		
		private static final long serialVersionUID = 1L;
		
		//I added this constructor.
		public RestrictedJTree(TreeViewSettings treeViewSettingsInstance) {
			super();

			this.treeViewSettingsInstance = treeViewSettingsInstance;
			System.out.println("RestrictedJTree constructor: treeViewSettingsInstance = " + treeViewSettingsInstance);
		}
		

		protected Runnable visibleRunnable = new Runnable() {
			public void run() {
				makeVisible(path);
				System.out.println("RestrictedJTree: visibleRunnable: path is " + path);
			}
		};

		boolean expandAllowed = false;
	
		public void refresh() {
			refresh(false);
			System.out.println("RestrictedJTree: refresh method.");	
		}

		public void refresh(boolean fromThread) {
			if (model == null)
				return;
			expandAllowed = true;
			expandPaths(fromThread);
			expandAllowed = false;
			repaint();
			System.out.println("RestrictedJTree: refresh method with fromThread arg.");	

		}

		protected void expandPaths(boolean fromThread) {
			Set<Object> seenem = new HashSet<Object>();
			Object root = getModel().getRoot();
			expandPaths(null, root, seenem, fromThread);
		}

		protected void expandPaths(TreePath parentPath, Object o,
				Set<Object> seenem, boolean fromThread) {

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
