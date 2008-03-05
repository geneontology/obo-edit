package org.oboedit.gui.factory;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.GUIComponentFactory;
import org.bbop.framework.GUIComponentFactory.FactoryCategory;
import org.oboedit.gui.components.DAGView;

public class DAGViewFactory extends AbstractComponentFactory<DAGView> {
	
	public DAGViewFactory() {
	}
	
	public DAGView doCreateComponent(String id) {
		return new DAGView(id);
	}

	public String getName() {
		return "DAG View";
	}
	
	public String getID() {
		return "DAG_VIEW";
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
		return "The_DAG_Viewer";
	}
}
