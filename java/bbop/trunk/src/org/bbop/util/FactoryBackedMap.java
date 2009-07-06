package org.bbop.util;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/*
 * This map starts out with no values, just a set of keys and a factory. When a key
 * is requested, the factory creates the value, and then the value is cached. Future
 * calls to fetch the key will return the cached version.
 */
import org.apache.log4j.*;

public class FactoryBackedMap<K, V> implements Map<K, V> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FactoryBackedMap.class);

	protected Map<K, V> cacheMap;

	protected MapValueFactory<K, V> factory;

	protected Set<K> keySet;

	public FactoryBackedMap(MapValueFactory<K, V> factory, Collection<K> keySet) {
		this(null, factory, keySet);
	}

	public FactoryBackedMap(Map<K, V> cacheMap, MapValueFactory<K, V> factory,
			Collection<K> keySet) {
		if (cacheMap == null)
			this.cacheMap = new HashMap<K, V>();
		else {
			this.cacheMap = cacheMap;
			this.cacheMap.clear();
		}
		this.factory = factory;
		this.keySet = new HashSet<K>();
		for (K key : keySet) {
			this.keySet.add(key);
		}
	}

	public void clear() {
		cacheMap.clear();
		keySet.clear();
	}

	public boolean containsCachedKey(Object key) {
		return cacheMap.containsKey(key);
	}

	public boolean containsKey(Object key) {
		return keySet.contains(key);
	}

	/**
	 * Breaks the normal map contract here
	 */
	public boolean containsValue(Object value) {
		cacheAllKeys();
		return cacheMap.containsValue(value);
	}

	protected void cacheAllKeys() {
		for (K key : keySet()) {
			get(key);
		}
	}

	public Set<java.util.Map.Entry<K, V>> entrySet() {
		cacheAllKeys();
		return cacheMap.entrySet();
	}

	public V get(Object key) {
		if (cacheMap.containsKey(key)) {
			logger.info("CACHE: returned "+cacheMap.get(key)+" from "+key);
			return cacheMap.get(key);
		} else {
			V val =  factory.createObject((K) key);
			cacheMap.put((K) key, val);
			if (!keySet.contains(key))
				keySet.add((K) key);
			logger.info("FACTORY: returned "+val+" from "+key);
			return val;
		}
	}

	public boolean isEmpty() {
		return keySet.isEmpty();
	}

	public Set<K> keySet() {
		return keySet;
	}

	public V put(K key, V value) {
		keySet.add(key);
		return cacheMap.put(key, value);
	}

	public void putAll(Map<? extends K, ? extends V> m) {
		for (K key : m.keySet()) {
			put(key, m.get(key));
		}
	}

	public V remove(Object key) {
		keySet.remove(key);
		return cacheMap.remove(key);
	}

	public int size() {
		return keySet.size();
	}

	public Collection<V> values() {
		cacheAllKeys();
		return cacheMap.values();
	}
}
