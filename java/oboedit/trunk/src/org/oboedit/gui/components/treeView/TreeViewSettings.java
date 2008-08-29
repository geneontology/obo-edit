package org.oboedit.gui.components.treeView;

import org.apache.log4j.Logger;
import org.bbop.framework.ComponentConfiguration;

public class TreeViewSettings implements ComponentConfiguration {
	protected boolean multiSelect = getMultiSelect();

	protected boolean trimPaths = getTrimPaths();

	protected boolean showNonTransitive = getShowNonTransitive();

	TreeView treeViewInstance;
	TreeViewSettings treeViewSettingsInstance;
	
	// initialize logger
	protected final static Logger logger = Logger.getLogger(TreeViewConfigPanel.class);
	
	public TreeViewSettings(TreeView treeViewInstance) {

	treeViewInstance = this.treeViewInstance;
//	treeViewInstance.treeViewSettingsInstance = treeViewSettingsInstance;	
	
	logger.debug("TreeViewSettings: entered this method to access getters and setters.");
	
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
