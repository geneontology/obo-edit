package org.oboedit.gui.components.graphvizViewer;

import org.bbop.framework.ComponentConfiguration;

import org.apache.log4j.*;

public class GraphViewConfiguration implements
	ComponentConfiguration {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GraphViewConfiguration.class);
	protected boolean succinctDisplay = true;
	protected boolean showPerType = true;
	protected boolean allTypes = true;
	protected boolean nonTransitive = false;



	public boolean isSuccinctDisplay() {
		return succinctDisplay;
	}

	public void setSuccinctDisplay(boolean succinctDisplay) {
		this.succinctDisplay = succinctDisplay;
	}

	public boolean isShowPerType() {
		return showPerType;
	}

	public void setShowPerType(boolean showPerType) {
		this.showPerType = showPerType;
	}

	public boolean isAllTypes() {
		return allTypes;
	}

	public void setAllTypes(boolean allTypes) {
		this.allTypes = allTypes;
	}

	public boolean isNonTransitive() {
		return nonTransitive;
	}

	public void setNonTransitive(boolean nonTransitive) {
		this.nonTransitive = nonTransitive;
	}


}
