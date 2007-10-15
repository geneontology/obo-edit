package org.bbop.dataadapter;

import java.util.Collection;

/*
 * Provides a collection of uis for an adapter.
 */
public interface DataAdapterUIFactory {

    public Collection getUIs(DataAdapter adapter);
}
