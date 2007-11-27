package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.GraphvizCanvas;

public class GraphvizViewFactory extends AbstractComponentFactory<GraphvizCanvas> {

	public GraphvizViewFactory() {
	}
	
	public String getID() {
		return "GRAPHVIZ_VIEW";
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
