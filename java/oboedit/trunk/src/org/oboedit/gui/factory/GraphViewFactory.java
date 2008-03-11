package org.oboedit.gui.factory;

/** Was GraphDAGViewFactory, which made DAGViewCanvases */

import java.util.Collections;
import java.util.List;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.GUIComponentFactory;
import org.bbop.framework.GUIComponentFactory.FactoryCategory;
import org.oboedit.gui.components.GraphViewCanvas;

public class GraphViewFactory extends AbstractComponentFactory<GraphViewCanvas> {

	public GraphViewFactory() {
	}
	
	public String getID() {
//		return "GRAPH_DAG_VIEW";
		return "GRAPH_VIEW";
	}
	
	public GraphViewCanvas doCreateComponent(String id) {
		return new GraphViewCanvas(id);
	}

	public String getName() {
//		return "Graph DAG Viewer";
		return "Graph Viewer";
	}
	
	
	public FactoryCategory getCategory() {
		return FactoryCategory.INFO;
	}

	@Override
	public String getHelpTopicID() {
		return "Graph_Viewer";
	}
}
