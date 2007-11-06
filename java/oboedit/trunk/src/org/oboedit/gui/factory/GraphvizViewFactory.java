package org.oboedit.gui.factory;

import java.util.Collections;
import java.util.List;

//DAGViewCanvas becomes GraphvizCanvas
//GRAPH_DAG_VIEW becomes GRAPHVIZ_VIEW

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.GUIComponentFactory;
import org.bbop.framework.GUIComponentFactory.FactoryCategory;
import org.oboedit.gui.components.GraphvizCanvas;

public class GraphvizViewFactory extends AbstractComponentFactory<GraphvizCanvas> {

	public GraphvizViewFactory() {
		addID("GRAPHVIZ_VIEW");
	}
	public GraphvizCanvas doCreateComponent(String id) {
		return new GraphvizCanvas(id);
	}

	public String getName() {
		return "Graphviz Viewer";
	}
	
	
	public FactoryCategory getCategory() {
		return FactoryCategory.INFO;
	}
}
