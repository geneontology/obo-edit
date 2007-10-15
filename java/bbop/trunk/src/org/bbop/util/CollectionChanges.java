package org.bbop.util;

import java.util.Collection;

public interface CollectionChanges<T> {

	public Collection<T> getAddedItems();
	public Collection<T> getDeletedItems();
}
