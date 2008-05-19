package org.bbop.util;

import java.util.ArrayList;
import java.util.Collection;

import org.apache.log4j.*;

public class MultiArrayListMap<K,V> extends MultiHashMap<K,V> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(MultiArrayListMap.class);

	@Override
	protected Collection<V> createEmptyCollection() {
		return new ArrayList<V>();
	}
}
