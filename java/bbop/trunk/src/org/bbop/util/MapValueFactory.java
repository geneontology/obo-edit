package org.bbop.util;

public interface MapValueFactory<K,V> {

	public V createObject(K key);
}
