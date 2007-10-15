package org.bbop.dataadapter;

import java.util.*;

public class DefaultUIFactory implements DataAdapterUIFactory {

    protected Map uiMap = new HashMap();

    public void addUI(DataAdapter adapter, DataAdapterUI ui) {
	Collection s = (Collection) uiMap.get(adapter);
	if (s == null) {
	    s = new HashSet();
	    uiMap.put(adapter, s);
	}
	s.add(ui);
    }

    public void removeUI(DataAdapter adapter, DataAdapterUI ui) {
	Collection s = (Collection) uiMap.get(adapter);
	if (s != null) {
	    s.remove(ui);
	    if (s.size() == 0)
		uiMap.remove(adapter);
	}
    }

    public Collection getUIs(DataAdapter adapter) {
	Collection out = new HashSet();
	Collection s = (Collection) uiMap.get(adapter);
	if (s != null)
	    out.addAll(s);
	if (adapter.getPreferredUI() != null)
	    out.add(adapter.getPreferredUI());
	return out;
    }
}
