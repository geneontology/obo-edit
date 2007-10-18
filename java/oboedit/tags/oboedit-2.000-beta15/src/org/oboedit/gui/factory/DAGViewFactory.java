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
		addID("DAG_VIEW");
		addID("plugin:org.geneontology.oboedit.plugin.DAGView");
	}
	
	public DAGView doCreateComponent(String id) {
		return new DAGView(id);
	}

	public String getName() {
		return "DAG View";
	}
	
	@Override
	public boolean getPreferSeparateWindow() {
		return true;
	}
	
	public FactoryCategory getCategory() {
		return FactoryCategory.INFO;
	}
}
