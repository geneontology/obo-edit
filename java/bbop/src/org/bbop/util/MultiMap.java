package org.bbop.util;

import java.util.Collection;
import java.util.Map;

public interface MultiMap<K, V> extends Map<K, Collection<V>> {
	
	public boolean containsSingleValue(Object value);
	public Collection<V> singleValues();
	public V add(K key, V value);
	public V remove(K key, V value);
	
}
