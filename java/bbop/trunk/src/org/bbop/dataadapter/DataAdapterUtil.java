package org.bbop.dataadapter;

import java.util.*;

public class DataAdapterUtil {

	/**
	 * Returns the first {@link DataAdapterUI} for an adapter that is a subclass
	 * of ALL the provided classes. This is often used to find all the adapter
	 * uis that are appropriate to a particular data adapter widget. For
	 * example, {@link GraphicalAdapterChooser} calls this method to find adapter
	 * uis that are both {@link GraphicalUI}s and
	 * {@link java.awt.Component}s
	 * 
	 * @param adapter
	 * @param registry
	 * @param uiClasses
	 * @return
	 */
	public static DataAdapterUI getUI(DataAdapter adapter,
			DataAdapterRegistry registry, Class<?>[] uiClasses) {
		Collection<DataAdapterUI> c = registry.getUIFactory().getUIs(adapter);
		for (DataAdapterUI ui : c) {
			boolean match = true;
			for (int i = 0; i < uiClasses.length; i++) {
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

	/**
	 * Returns all the data adapters that support a given {@link IOOperation} and
	 * have a {@link DataAdapterUI} that is a subclass of all the provided classes. 
	 * @param registry
	 * @param op
	 * @param uiClasses
	 * @return
	 */
	public static DataAdapter[] getAdapters(DataAdapterRegistry registry,
			IOOperation<?,?> op, Class<?>[] uiClasses) {
		Collection<DataAdapter> out = new ArrayList<DataAdapter>();
		for (DataAdapter adapter : registry.getAdapters()) {
			boolean matched = false;
			IOOperation<?,?>[] ops = adapter.getSupportedOperations();
			for (int i = 0; i < ops.length; i++) {
				if (ops[i].equals(op)) {
					matched = true;
					break;
				}
			}
			if (matched && uiClasses != null) {
				Collection<DataAdapterUI> uis = registry.getUIFactory().getUIs(adapter);
				matched = false;
				for(DataAdapterUI ui : uis) {
					matched = true;
					for (int i = 0; i < uiClasses.length; i++) {
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
		DataAdapter[] adapters = out.toArray(new DataAdapter[0]);
		return adapters;
	}
}
