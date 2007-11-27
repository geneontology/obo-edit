package org.oboedit.gui.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.GraphEditor;
import org.oboedit.gui.components.LinkDatabaseCanvas;

public class GraphEditorFactory extends AbstractComponentFactory<GraphEditor> {

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
		return FactoryCategory.ONTOLOGY;
	}
}
