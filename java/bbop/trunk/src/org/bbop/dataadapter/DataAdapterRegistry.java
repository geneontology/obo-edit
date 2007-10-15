package org.bbop.dataadapter;

import java.util.Collection;

/*
 * A registry of available data adapters
 */
public interface DataAdapterRegistry {

    public void addAdapter(DataAdapter adapter);
    public void removeAdapter(DataAdapter adapter);
    public void setUIFactory(DataAdapterUIFactory factory);

    public Collection<DataAdapter> getAdapters();
    public DataAdapter getAdapter(String id);
    public DataAdapterUIFactory getUIFactory();
}
