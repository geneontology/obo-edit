package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.graphvizViewer.GraphvizCanvas;

import org.apache.log4j.*;

public class GraphvizViewFactory extends AbstractComponentFactory<GraphvizCanvas> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GraphvizViewFactory.class);

	public GraphvizViewFactory() {
	}
	
	public String getID() {
		return "GRAPHVIZ_VIEW";
	}
	
	public GraphvizCanvas doCreateComponent(String id) {
		System.out.println("doCreateComponent: Calling new Graphviz component.");
		return new GraphvizCanvas(id);
	}

	public String getName() {
		return "Graphviz Viewer";
	}
	
	
	public FactoryCategory getCategory() {
		return FactoryCategory.VIEWERS;
	}
}
