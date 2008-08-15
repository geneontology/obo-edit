package org.oboedit.gui.components.treeView;

import org.bbop.framework.ComponentConfiguration;

public class TreeViewConfig implements ComponentConfiguration {
	protected boolean multiSelect = false;

	protected boolean trimPaths = true;

	protected boolean showNonTransitive = false;

	
	public TreeViewConfig() {

	System.out.println("TreeViewConfig: entered this method to access getters and setters.");
	
	}

	public void setShowNonTransitive(boolean showNonTransitive) {
		this.showNonTransitive = showNonTransitive;
	}

	public boolean getShowNonTransitive() {
		return showNonTransitive;
	}

	public void setMultiSelect(boolean multiSelect) {
		this.multiSelect = multiSelect;
	}

	public boolean getMultiSelect() {
		return multiSelect;
	}

	public void setTrimPaths(boolean trimPaths) {
		this.trimPaths = trimPaths;
	}

	public boolean getTrimPaths() {
		return trimPaths;
	}
}
