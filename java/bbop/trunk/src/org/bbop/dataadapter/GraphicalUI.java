package org.bbop.dataadapter;

public interface GraphicalUI extends DataAdapterUI {

    public void acceptComponentConfig(boolean storeonly)
	throws DataAdapterUIException;
    public GraphicalUI getAdvancedUI();
    public void setAdvancedUI(GraphicalUI ui);
    public GraphicalUI getSimpleUI();
    public void setSimpleUI(GraphicalUI ui);
}
