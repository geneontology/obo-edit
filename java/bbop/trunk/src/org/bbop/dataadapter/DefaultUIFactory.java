package org.bbop.dataadapter;

import java.util.*;

import org.bbop.util.MultiHashMap;
import org.bbop.util.MultiMap;

/**
 * A basic in-memory implementation of {@link DataAdapterUIFactory} that
 * always returns an adapters preferred ui, along with any additional UIs manually
 * added by calls to {@link #addUI(DataAdapter, DataAdapterUI)}
 * @author jrichter
 *
 */
public class DefaultUIFactory implements DataAdapterUIFactory {

	protected MultiMap<DataAdapter, DataAdapterUI> uiMap =
		new MultiHashMap<DataAdapter, DataAdapterUI>();

	public void addUI(DataAdapter adapter, DataAdapterUI ui) {
		uiMap.add(adapter, ui);
	}

	public void removeUI(DataAdapter adapter, DataAdapterUI ui) {
		uiMap.remove(adapter, ui);
	}

	public Collection<DataAdapterUI> getUIs(DataAdapter adapter) {
		Collection<DataAdapterUI> out = new HashSet<DataAdapterUI>();
		out.addAll(uiMap.get(adapter));
		if (adapter.getPreferredUI() != null)
			out.add(adapter.getPreferredUI());
		return out;
	}
}
