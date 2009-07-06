package org.bbop.util;

import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.*;

public class ReferenceMap<K, V> implements Map<K, V> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ReferenceMap.class);

	public Map<K, MapSoftReference<K,V>> backingMap;
	
	protected ReferenceQueue<MapSoftReference.SoftPointer<V>> referenceQueue = new ReferenceQueue<MapSoftReference.SoftPointer<V>>();

	protected ReferenceQueueCleanupThread cleanupThread;

	protected Collection<ReferenceCleanupListener> cleanupListeners = new LinkedList<ReferenceCleanupListener>();
	
	public ReferenceMap() {
		this(new HashMap<K,MapSoftReference<K,V>>());
	}

	public ReferenceMap(Map<K, MapSoftReference<K,V>> backingMap) {
		this.backingMap = Collections.synchronizedMap(backingMap);
		// startCleanupThread();
	}

	public void startCleanupThread() {
		if (cleanupThread != null) {
			if (cleanupThread.isAlive()) {
				cleanupThread.halt();
			}
		}
			cleanupThread = new ReferenceQueueCleanupThread(referenceQueue);
			cleanupThread.addCleanupListener(new ReferenceCleanupListener() {
				public void cleanup(ReferenceCleanupEvent event) {
					MapSoftReference ref = (MapSoftReference) event
							.getReference();
					doCleanup(ref);
				}
			});
		cleanupThread.start();
	}
	
	protected synchronized void doCleanup(MapSoftReference ref) {
		fireCleanup(ref);
		backingMap.remove(ref.getKey());
		ref.reallyClear();
		logger.info("backingMap size = "+backingMap.size());
	}

	public void stopCleanupThread() {
		if (cleanupThread != null)
			cleanupThread.halt();
	}

	public void addCleanupListener(ReferenceCleanupListener listener) {
		cleanupListeners.add(listener);
	}

	public void removeCleanupListener(ReferenceCleanupListener<V> listener) {
		cleanupListeners.remove(listener);
	}

	protected void fireCleanup(Reference<Object> ref) {
		ReferenceCleanupEvent<Object> event = new ReferenceCleanupEvent<Object>(
				referenceQueue, ref);
		for (ReferenceCleanupListener<Object> listener : cleanupListeners) {
			listener.cleanup(event);
		}
	}

	public void clear() {
		backingMap.clear();
	}

	public boolean containsKey(Object key) {
		return backingMap.containsKey(key);
	}

	public boolean containsValue(Object value) {
		for (V v : values()) {
			if (v.equals(value))
				return true;
		}
		return false;
	}

	public Set<Map.Entry<K, V>> entrySet() {
		Set<Map.Entry<K, V>> out = new TinySet<Map.Entry<K, V>>();
		for (final Map.Entry<K, MapSoftReference<K,V>> entry : backingMap.entrySet()) {
			final V value = entry.getValue().getValue();
			if (value != null) {
				out.add(new Map.Entry<K, V>() {

					public K getKey() {
						return entry.getKey();
					}

					public V getValue() {
						return value;
					}

					public V setValue(V v) {
						MapSoftReference ref = createReference(entry.getKey(), v, false);
						entry.setValue(ref);
						return value;
					}
				});
			}
		}
		return out;
	}

	public V get(Object key) {
		MapSoftReference<K,V> r = backingMap.get(key);
		if (r != null)
			return r.getValue();
		else
			return null;
	}

	public boolean isEmpty() {
		return backingMap.isEmpty();
	}

	public Set<K> keySet() {
		return backingMap.keySet();
	}
	
	public V put(K key, V value) {
		return put(key, value, false);
	}

	public V put(K key, V value, boolean isDirty) {
		MapSoftReference<K,V> r = backingMap.put(key, createReference(key, value, isDirty));
		if (r != null)
			return r.getValue();
		else
			return null;
	}

	protected MapSoftReference<K,V> createReference(K key, V value, boolean isDirty) {
		return new MapSoftReference<K,V>(key, value, isDirty, referenceQueue);
	}
	
	public synchronized void markDirty(K key, boolean isDirty) {
		MapSoftReference<K,V> r = backingMap.get(key);	
		r.setDirty(isDirty);
	}
	
	public boolean isDirty(K key) {
		MapSoftReference<K,V> r = backingMap.get(key);
		return r.isDirty();
	}

	public void putAll(Map<? extends K, ? extends V> t) {
		Iterator<? extends Entry<? extends K, ? extends V>> i = t.entrySet()
				.iterator();
		while (i.hasNext()) {
			Entry<? extends K, ? extends V> e = i.next();
			put(e.getKey(), e.getValue());
		}
	}

	public V remove(Object key) {
		MapSoftReference<K,V> r = backingMap.remove(key);
		if (r != null)
			return r.getValue();
		else
			return null;
	}

	public int size() {
		return backingMap.size();
	}

	public Collection<V> values() {
		Collection<V> out = new LinkedList<V>();
		for (MapSoftReference<K,V> r : backingMap.values()) {
			V value = r.getValue();
			if (value != null)
				out.add(value);
		}
		return out;
	}

}
