package org.oboedit.gui.components.treeView;

import org.apache.log4j.Logger;
import org.bbop.framework.ConfigurationPanel;
import org.bbop.framework.GUIComponent;



public class TreeViewConfigPanel  extends ConfigurationPanel {

	private static final long serialVersionUID = 1L;

	// initialize logger
	protected final static Logger logger = Logger.getLogger(TreeViewConfigPanel.class);

TreeViewSettings treeViewSettings;
TreeView treeViewInstance;


	public TreeViewConfigPanel(TreeView treeViewInstance) {

		this.treeViewInstance = treeViewInstance;

		this.treeViewSettings = treeViewInstance.treeViewSettingsInstance;


	}



	@Override
	public void commit() {

		logger.info("TreeViewConfigPanel: commit() run.");
		logger.info("TreeViewConfigPanel: treeViewSettings = " + treeViewSettings);

		//	treeViewSettings.setDotPath(appPathField.getText());
		//	treeViewSettings.setLabelFont(linkFontChooser.getChosenFont());
		//	treeViewSettings.setNodeFont(nodeFontChooser.getChosenFont());
		//	treeViewSettings.setTypeShape((String) typeShapeList.getSelectedItem());
		//	treeViewSettings.setNodeShape((String) nodeShapeList.getSelectedItem());
		//	treeViewSettings.setObsoleteShape((String) obsoleteShapeList.getSelectedItem());
		//	treeViewSettings.setFlipOver(flipoverBox.isSelected());
		//	treeViewSettings.setShowIDs(showIDsBox.isSelected());
		//	
		//	logger.info("TreeViewConfigPanel: commit() : " + treeViewSettings.getLabelFont());
		//	
		//	for (int i = 0; i < typeColorList.getData().size(); i++) {
		//		Object o = typeColorList.getData().get(i);
		//		if (o instanceof TypeColorPair) {
		//			TypeColorPair tc = (TypeColorPair) o;
		//			// logger.info("TreeViewConfigPanel: o is instance of TypeColorPair and o = " + o);
		//			treeViewSettings.getColorMap().put(tc.getTypeID(), tc.getPair());
		//
		//		
		//		} else if (o instanceof NamedColor) {
		//			//logger.info("TreeViewConfigPanel: o is instance of NamedColor and o = " + o);
		//			NamedColor nc = (NamedColor) o;
		//			//logger.info("TreeViewConfigPanel: NamedColor nc = " + nc);
		//			
		//			treeViewSettings.setNamedColor(nc.getName(), nc.getColor());
		//		}
		//	}
		//
		//	treeViewSettings.setViewerFormat((String) formatBox.getSelectedItem());
		//	
		//	TreeViewInstance.treeViewSettingsInstance = treeViewSettings;
		//	
		//	TreeViewInstance.reloadImage();

	}



	@Override
	public void init() {

		//	this.treeViewSettings = TreeViewInstance.treeViewSettingsInstance;
		//	
		//	logger.info("TreeViewConfigPanel: init() run.");
		//	logger.info("TreeViewConfigPanel, init method: variable TreeViewInstance = " + TreeViewInstance);
		//	//I'm not sure if any of the lines below are needed. 
		//	//The graphviz path works without them and they
		//	//don't make the background color work. 
		//	
		//	appPathField.setText(treeViewSettings.getDotPath());
		//	linkFontChooser.setChosenFont(treeViewSettings.getLabelFont());
		//	nodeFontChooser.setChosenFont(treeViewSettings.getNodeFont());
		//	typeShapeList.setSelectedItem(treeViewSettings.getTypeShape());
		//	nodeShapeList.setSelectedItem(treeViewSettings.getNodeShape());
		//	obsoleteShapeList.setSelectedItem(treeViewSettings.getObsoleteShape());
		//	flipoverBox.setSelected(treeViewSettings.getFlipOver());
		//	showIDsBox.setSelected(treeViewSettings.getShowIDs());
		//
		//	data = treeViewSettings.getNamedColorList();
		//	typeColorList.setData(data);
		//	
		//	
		//	formatBox.setSelectedItem(treeViewSettings.getViewerFormat());

	}


	@Override
	public GUIComponent getComponent() {
		logger.info("Config panel New : getComponent.");
		return treeViewInstance;
	}	

	@Override
	public void setComponent(GUIComponent comp) {
		if (comp instanceof TreeView) {
			treeViewInstance = (TreeView)comp;
			this.treeViewSettings = treeViewInstance.treeViewSettingsInstance;



			logger.info("TreeViewConfigPanel, setComponent method:  variable treeViewInstance = " + treeViewInstance);
			logger.info("TreeViewConfigPanel: treeViewSettings = " + treeViewSettings);
			logger.info("Config panel New : setComponent.");
		}
	}
}