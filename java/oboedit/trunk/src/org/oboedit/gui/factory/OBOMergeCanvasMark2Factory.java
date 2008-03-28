package org.oboedit.gui.factory;


import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.GraphViewCanvas;
import org.oboedit.gui.components.OBOMergeCanvasMark2;

public class OBOMergeCanvasMark2Factory extends AbstractComponentFactory<OBOMergeCanvasMark2> {

	public OBOMergeCanvasMark2Factory() {
	}
	
	public String getID() {
		return "OBO_MERGE_VIEW";
	}
	
	public OBOMergeCanvasMark2 doCreateComponent(String id) {
		return new OBOMergeCanvasMark2(id);
	}

	public String getName() {
		return "OBO Merge";
	}
	
	
	public FactoryCategory getCategory() {
		return FactoryCategory.INFO;
	}

	@Override
	public String getHelpTopicID() {
		return "OBO_Merge";
	}
}
