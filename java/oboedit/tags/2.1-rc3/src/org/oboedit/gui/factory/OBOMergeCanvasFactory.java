package org.oboedit.gui.factory;

/*
 * Factory for the OBOMerge GUI.
 */

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.OBOMergeCanvas;

import org.apache.log4j.*;

public class OBOMergeCanvasFactory extends AbstractComponentFactory<OBOMergeCanvas> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBOMergeCanvasFactory.class);

	public OBOMergeCanvasFactory() {
	}
	
	public String getID() {
		return "OBO_MERGE_VIEW";
	} 
	
	public OBOMergeCanvas doCreateComponent(String id) {
		return new OBOMergeCanvas(id);
	}

	public String getName() {
		return "OBO Merge";
	}
	
	
	public FactoryCategory getCategory() {
		return FactoryCategory.TOOLS;
	}

	@Override
	public String getHelpTopicID() {
		return "OBO_Merge";
	}
}
