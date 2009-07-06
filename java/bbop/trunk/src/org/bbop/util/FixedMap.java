package org.bbop.util;

import java.util.*;

/**
 * A map that contains a fixed number of mappings. If more mappings are
 * added than the maximum, the least recently accessed mappings are discared.
 * <p>
 * The Least Recently Used algorithm used here works as follows:<br>
 * An "access" of a key occurs when containsKey(), get(), or put() is called
 * on a key. When the key is accessed, it is moved to the front of the access
 * list. If a key is added such that the number of keys in the Map exceeds
 * the maxSize of the Map, the keys at the back of the access list are
 * discarded until the map is back within the maxSize.
 */
import org.apache.log4j.*;

public class FixedMap implements Map {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(FixedMap.class);

    /**
     * The backing store for the current map
     */
    protected HashMap map = new HashMap();

    /**
     * The ordered list of keys
     */
    protected LinkedList keyList = new LinkedList();

    /**
     * The maximum number of keys in this collection
     */
    protected int maxSize;

    /**
     * Creates a FixedMap with a maxSize of 20
     */
    public FixedMap() {
	this(20);
    }

    public FixedMap(int maxSize) {
	setMaxSize(maxSize);
    }

    /**
     * Creates a FixedMap with the given map values and a fixed size of 20.
     */
    public FixedMap(Map values) {
	this(values, 20);
    }

    public FixedMap(Map values, int maxSize) {
	setMaxSize(maxSize);
	putAll(values);	
    }

    /**
     * Changes the maximum size of this map. If the new size is smaller than
     * the previous size of this map, the least recently accessed keys will
     * be discarded until the map fits the new size.
     */
    public void setMaxSize(int newSize) {
	for(int i=0; i < maxSize - newSize && i < keyList.size(); i++) {
	    map.remove(keyList.removeLast());
	}
	maxSize = newSize;
    }

    public boolean containsKey(Object key) {
	keyList.remove(key);
	keyList.addFirst(key);
	return map.containsKey(key);
    }

    public Object get(Object key) {
	keyList.remove(key);
	keyList.addFirst(key);
	return map.get(key);
    }

    public Object put(Object key, Object value) {
	keyList.remove(key);
	keyList.addFirst(key);
	if (keyList.size() > maxSize) {
	    for(int i=0; i < keyList.size() - maxSize; i++) {
		Object discard = keyList.removeLast();
		map.remove(discard);
	    }
	}
	return map.put(key, value);
    }

    public Object remove(Object key) {
	keyList.remove(key);
	return map.remove(key);
    }

    public int size() {
	return map.size();
    }

    public boolean isEmpty() {
	return map.isEmpty();
    }

    public boolean containsValue(Object value) {
	return map.containsValue(value);
    }

    public void putAll(Map t) {
	Set s = t.keySet();
	Iterator it = s.iterator();
	while(it.hasNext()) {
	    Object key = it.next();
	    put(key, t.get(key));
	}
    }

    public void clear() {
	map.clear();
    }

    public Set keySet() {
	return map.keySet();
    }

    public Collection values() {
	return map.values();
    }

    public Set entrySet() {
	return map.entrySet();
    }

    @Override
	public boolean equals(Object o) {
	return map.equals(o);
    }

    @Override
	public int hashCode() {
	return map.hashCode();
    }
}




