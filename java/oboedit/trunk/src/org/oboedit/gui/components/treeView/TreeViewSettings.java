package org.oboedit.gui.components.treeView;

import org.apache.log4j.Logger;
import org.bbop.framework.ComponentConfiguration;

/**
  * @author John Day-Richter, Jennifer Deegan, and Nicolas Rodriguez.<br>
  * Docs by Jennifer Deegan and Nicolas Rodriguez.
  *
 */
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
		System.out.println("TreeViewSettings: setShowNonTransitive");
		this.showNonTransitive = showNonTransitive;
	}

	public boolean getShowNonTransitive() {
		System.out.println("TreeViewSettings: getShowNonTransitive");
		return showNonTransitive;
	}

	/**
	 * @param multiSelect
	 */
	public void setMultiSelect(boolean multiSelect) {
		System.out.println("TreeViewSettings: setMultiSelect");
		this.multiSelect = multiSelect;
	}

	/**
	 * Returns whether or not the component is set up to carry over multiple selection of terms from the Ontology Tree Editor.
	 * 
	 * @return whether or not the component is set up to carry over multiple selection of terms from the Ontology Tree Editor.
	 */
	public boolean getMultiSelect() {
		System.out.println("TreeViewSettings: getMultiSelect");
		return multiSelect;
	}

	public void setTrimPaths(boolean trimPaths) {
		System.out.println("TreeViewSettings: setTrimPaths");
		this.trimPaths = trimPaths;
	}

	public boolean getTrimPaths() {
		System.out.println("TreeViewSettings: getTrimPaths");
		return trimPaths;
	}
}
