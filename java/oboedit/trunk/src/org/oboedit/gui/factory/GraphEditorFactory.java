package org.oboedit.gui.factory;

/** This class name was not changed in the name change of 3/2008 */

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.GraphEditor;
import org.oboedit.gui.components.LinkDatabaseCanvas;

import org.apache.log4j.*;

public class GraphEditorFactory extends AbstractComponentFactory<GraphEditor> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GraphEditorFactory.class);

	public GraphEditorFactory() {
	}
	
	public String getID() {
		return "GRAPH_EDITOR";
	}

	public GraphEditor doCreateComponent(String id) {
		return new GraphEditor(id);
	}

	public String getName() {
		return "Graph Editor";
	}
	
	public FactoryCategory getCategory() {
		return FactoryCategory.EDITORS;
	}

	@Override
	public String getHelpTopicID() {
		return "Graph_Editor";
	}

}
