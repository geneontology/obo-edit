package org.bbop.util;

import java.lang.ref.ReferenceQueue;
import java.lang.ref.SoftReference;

public class MapSoftReference<K,V> extends SoftReference<MapSoftReference.SoftPointer<V>> {
	
	public static class SoftPointer<T> {
		protected T object;
		
		public SoftPointer(T object) {
			this.object = object;
		}
		
		public T getObject() {
			return object;
		}		
	}
	
	protected K key;
	protected V value;
	protected boolean isDirty;

	public MapSoftReference(K key, V value, boolean isDirty, ReferenceQueue<SoftPointer<V>> referenceQueue) {
		super(new SoftPointer<V>(value), referenceQueue);
		this.key = key;
		this.value = value;
		this.isDirty = isDirty;
	}
	
	public void setDirty(boolean isDirty) {
		this.isDirty = isDirty;
	}
	
	public boolean isDirty() {
		return isDirty;
	}

	public K getKey() {
		return key;
	}

	public V getValue() {
		return value;
	}

	public void reallyClear() {
		key = null;
		value = null;
		isDirty = false;
	}
}
