package org.oboedit.gui.factory;

import java.util.Collections;
import java.util.List;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.GUIComponentFactory;
import org.bbop.framework.GUIComponentFactory.FactoryCategory;
import org.oboedit.gui.components.DAGViewCanvas;

public class GraphDAGViewFactory extends AbstractComponentFactory<DAGViewCanvas> {

	public GraphDAGViewFactory() {
	}
	
	public String getID() {
		return "GRAPH_DAG_VIEW";
	}
	
	public DAGViewCanvas doCreateComponent(String id) {
		return new DAGViewCanvas(id);
	}

	public String getName() {
		return "Graph DAG Viewer";
	}
	
	
	public FactoryCategory getCategory() {
		return FactoryCategory.INFO;
	}
}
