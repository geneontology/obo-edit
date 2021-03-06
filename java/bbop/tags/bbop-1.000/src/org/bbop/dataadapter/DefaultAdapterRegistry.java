package org.bbop.dataadapter;

import java.util.*;

/*
 * A registry of available data adapters
 */
public class DefaultAdapterRegistry implements DataAdapterRegistry {

	protected DataAdapterUIFactory factory = new DefaultUIFactory();
	protected Map<String, DataAdapter> adapters =
		new LinkedHashMap<String, DataAdapter>();

	public void addAdapter(DataAdapter adapter) {
		adapters.put(adapter.getID(), adapter);
	}

	public void removeAdapter(DataAdapter adapter) {
		adapters.remove(adapter.getID());
	}

	public DataAdapter getAdapter(String id) {
		return adapters.get(id);
	}

	public void setUIFactory(DataAdapterUIFactory factory) {
		this.factory = factory;
	}

	public Collection<DataAdapter> getAdapters() {
		return adapters.values();
	}

	public DataAdapterUIFactory getUIFactory() {
		return factory;
	}
}
