package org.bbop.dataadapter;

import java.util.*;

/*
 * A registry of available data adapters
 */
import org.apache.log4j.*;

public class DefaultAdapterRegistry implements DataAdapterRegistry {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefaultAdapterRegistry.class);

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
		List<DataAdapter> out = new ArrayList<DataAdapter>();
		for(String id : adapters.keySet()) {
			out.add(adapters.get(id));
		}
		return out;
	}

	public DataAdapterUIFactory getUIFactory() {
		return factory;
	}
}
