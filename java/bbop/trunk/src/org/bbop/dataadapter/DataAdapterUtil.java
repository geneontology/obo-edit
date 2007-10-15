package org.bbop.dataadapter;

import java.util.*;

public class DataAdapterUtil {

    public static DataAdapterUI getUI(DataAdapter adapter,
				      DataAdapterRegistry registry,
				      Class [] uiClasses) {
	Collection c = registry.getUIFactory().getUIs(adapter);
	Iterator it = c.iterator();
	while(it.hasNext()) {
	    DataAdapterUI ui = (DataAdapterUI) it.next();
	    boolean match = true;
	    for(int i=0; i < uiClasses.length; i++) {
		if (!uiClasses[i].isAssignableFrom(ui.getClass())) {
		    match = false;
		    break;
		}
	    }
	    if (match)
		return ui;
	}
	return null;
    }

    public static DataAdapter [] getAdapters(DataAdapterRegistry registry,
					     IOOperation op,
					     Class [] uiClasses) {
	Set out = new HashSet();
	Iterator it = registry.getAdapters().iterator();
	while(it.hasNext()) {
	    DataAdapter adapter = (DataAdapter) it.next();
	    boolean matched = false;
	    IOOperation [] ops = adapter.getSupportedOperations();
	    for(int i=0; i < ops.length; i++) {
		if (ops[i].equals(op)) {
		    matched = true;
		    break;
		}
	    }
	    if (matched && uiClasses != null) {
		Collection uis = registry.getUIFactory().getUIs(adapter);
		Iterator it2 = uis.iterator();
		matched = false;
		while(it2.hasNext()) {
		    DataAdapterUI ui = (DataAdapterUI) it2.next();
		    matched = true;
		    for(int i=0; i < uiClasses.length; i++) {
			if (!uiClasses[i].isAssignableFrom(ui.getClass())) {
			    matched = false;
			    break;
			}
		    }
		    if (matched)
			break;
		}
	    }
	    if (matched)
		out.add(adapter);
	}

	DataAdapter [] adapters = new DataAdapter[out.size()];
	it = out.iterator();
	for(int i=0; i < adapters.length; i++)
	    adapters[i] = (DataAdapter) it.next();
	return adapters;
    }
}
