package org.oboedit.gui.components.treeView;

import org.apache.log4j.Logger;
import org.bbop.framework.ComponentConfiguration;

/**
  * @author John Day-Richter, Jennifer Deegan, and Nicolas Rodriguez.<br>
  * Docs by Jennifer Deegan and Nicolas Rodriguez.
  * 
  * Holds the configuration settings for the component. This class is a <a href="http://en.wikipedia.org/wiki/JavaBeans">JavaBean</a>.
  * 
  *
 */
public class TreeViewSettings implements ComponentConfiguration {
	protected boolean multiSelect = true;

	protected boolean trimPaths = false;

	protected boolean showNonTransitive = false;

	// initialize logger
	protected final static Logger logger = Logger.getLogger(TreeViewConfigPanel.class);
	
	public TreeViewSettings() {

	//logger.debug("TreeViewSettings: entered this method to access getters and setters.");
	
	}

	public void setShowNonTransitive(boolean showNonTransitive) {
		//logger.debug("TreeViewSettings: setShowNonTransitive");
		this.showNonTransitive = showNonTransitive;
	}

	public boolean getShowNonTransitive() {
		//logger.debug("TreeViewSettings: getShowNonTransitive");
		return showNonTransitive;
	}

	/**
	 * @param multiSelect
	 */
	public void setMultiSelect(boolean multiSelect) {
		//logger.debug("TreeViewSettings: setMultiSelect");
		this.multiSelect = multiSelect;
	}

	/**
	 * Returns whether or not the component is set up to carry over multiple selection of terms from the Ontology Tree Editor.
	 * 
	 * @return whether or not the component is set up to carry over multiple selection of terms from the Ontology Tree Editor.
	 */
	public boolean getMultiSelect() {
		//logger.debug("TreeViewSettings: getMultiSelect");
		return multiSelect;
	}

	public void setTrimPaths(boolean trimPaths) {
		//logger.debug("TreeViewSettings: setTrimPaths");
		this.trimPaths = trimPaths;
	}

	public boolean getTrimPaths() {
		//logger.debug("TreeViewSettings: getTrimPaths");
		return trimPaths;
	}
}
