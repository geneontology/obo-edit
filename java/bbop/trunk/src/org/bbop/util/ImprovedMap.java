package org.bbop.util;

import java.util.Map;

public interface ImprovedMap<K,V> extends Map<K,V> {

	public K getKey(Object key);
}
