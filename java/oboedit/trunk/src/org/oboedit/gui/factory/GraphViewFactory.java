package org.oboedit.gui.factory;

/** Was GraphDAGViewFactory, which made DAGViewCanvases */

import java.util.Collections;
import java.util.List;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.GUIComponentFactory;
import org.bbop.framework.GUIComponentFactory.FactoryCategory;
import org.oboedit.gui.components.GraphViewCanvas;

import org.apache.log4j.*;

public class GraphViewFactory extends AbstractComponentFactory<GraphViewCanvas> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GraphViewFactory.class);

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
		return FactoryCategory.VIEWERS;
	}

	@Override
	public String getHelpTopicID() {
		return "Graph_Viewer";
	}
}
