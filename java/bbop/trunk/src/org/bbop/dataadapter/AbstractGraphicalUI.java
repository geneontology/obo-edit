package org.bbop.dataadapter;

import javax.swing.JPanel;

public abstract class AbstractGraphicalUI extends JPanel
    implements GraphicalUI {

    protected AdapterConfiguration config;
    protected UIConfiguration uiConfig;
    protected GraphicalUI advancedUI = null;
    protected GraphicalUI simpleUI = null;
    protected AdapterWidgetI widget;
    protected DataAdapter adapter;
    protected Object input;
    protected IOOperation op;

	{
	    setOpaque(false);
	}

    public void setUIConfiguration(UIConfiguration uiConfig) {
	this.uiConfig = uiConfig;
    }

    public UIConfiguration getUIConfiguration() {
	return uiConfig;
    }

    public void setConfiguration(AdapterConfiguration config) {
	this.config = config;
    }

    public void setAdvancedUI(GraphicalUI advancedUI) {
	this.advancedUI = advancedUI;
    }

    public void setSimpleUI(GraphicalUI simpleUI) {
	this.simpleUI = simpleUI;
    }

    public GraphicalUI getAdvancedUI() {
	return advancedUI;
    }

    public GraphicalUI getSimpleUI() {
	return simpleUI;
    }

    public void init(AdapterWidgetI widget, IOOperation op,
		     DataAdapter adapter, Object input) {
	this.widget = widget;
	this.op = op;
	this.adapter = adapter;
	this.input = input;
	this.config = createEmptyConfig();
    }

    public AdapterConfiguration getConfig(IOOperation op,
					  DataAdapter adapter,
					  Object input)
	throws DataAdapterUIException {
	return config;
    }

    public void cleanup() {}
}
