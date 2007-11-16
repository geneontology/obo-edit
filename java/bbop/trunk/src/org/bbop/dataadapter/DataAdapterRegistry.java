package org.bbop.dataadapter;

import java.util.Collection;

/**
 * A registry of available data adapters. A data adapter registry is used
 * to collect all the currently installed adapters, and most widgets that allow
 * a data adapter to be selected must be passed a DataAdapterRegistry.
 * 
 * @see GraphicalAdapterChooser
 */
public interface DataAdapterRegistry {

    public void addAdapter(DataAdapter adapter);
    public void removeAdapter(DataAdapter adapter);
    public void setUIFactory(DataAdapterUIFactory factory);

    public Collection<DataAdapter> getAdapters();
    public DataAdapter getAdapter(String id);
    public DataAdapterUIFactory getUIFactory();
}
