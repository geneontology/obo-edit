package org.oboedit.gui.factory;

/** Note: this class used to be called DAGViewFactory (which made DAGViews) */

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.GUIComponentFactory;
import org.bbop.framework.GUIComponentFactory.FactoryCategory;
import org.oboedit.gui.components.TreeView;

public class TreeViewFactory extends AbstractComponentFactory<TreeView> {
	
	public TreeViewFactory() {
	}
	
	public TreeView doCreateComponent(String id) {
		return new TreeView(id);
	}

	public String getName() {
//		return "DAG View";
		return "Tree Viewer";
	}
	
	public String getID() {
//		return "DAG_VIEW";
		return "TREE_VIEW";
	}
	
	@Override
	public boolean getPreferSeparateWindow() {
	        // By user request, have this start out as a normal docked window rather than a free-floating (undocked) one.
		return false;
	}
	
	public FactoryCategory getCategory() {
		return FactoryCategory.INFO;
	}

	@Override
	public String getHelpTopicID() {
		return "Tree_Viewer";
	}
}
