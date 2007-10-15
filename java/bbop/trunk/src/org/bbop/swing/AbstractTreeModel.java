package org.bbop.swing;

import javax.swing.tree.*;
import javax.swing.event.*;
import java.util.*;

public abstract class AbstractTreeModel implements TreeModel {

    protected List listeners = new LinkedList();

    public void addTreeModelListener(TreeModelListener l) {
	listeners.add(l);
    }

    public void removeTreeModelListener(TreeModelListener l) {
	listeners.remove(l);
    }

    protected void fireTreeNodesChanged(TreeModelEvent e) {
	Iterator it = listeners.iterator();
	while(it.hasNext()) {
	    TreeModelListener l = (TreeModelListener) it.next();
	    l.treeNodesChanged(e);
	}
    }
    protected void fireTreeNodesInserted(TreeModelEvent e) {
	Iterator it = listeners.iterator();
	while(it.hasNext()) {
	    TreeModelListener l = (TreeModelListener) it.next();
	    l.treeNodesInserted(e);
	}
    }

    protected void fireTreeNodesRemoved(TreeModelEvent e) {
	Iterator it = listeners.iterator();
	while(it.hasNext()) {
	    TreeModelListener l = (TreeModelListener) it.next();
	    l.treeNodesRemoved(e);
	}
    }

    protected void fireTreeStructureChanged(TreeModelEvent e) {
	Iterator it = listeners.iterator();
	while(it.hasNext()) {
	    TreeModelListener l = (TreeModelListener) it.next();
	    l.treeStructureChanged(e);
	}
    }

    public void valueForPathChanged(TreePath path, Object newValue) {
    }

    public void reload() {
	fireTreeStructureChanged(new TreeModelEvent(this,
						    new TreePath(getRoot())));
    }
}
