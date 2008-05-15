package org.oboedit.gui.components.graphvizViewer;

import org.bbop.framework.ComponentConfiguration;

public class GraphViewConfiguration implements
ComponentConfiguration {
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
