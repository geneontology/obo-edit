package org.bbop.util;

import java.lang.reflect.*;
import java.util.*;

public class MemoryAllocator {

	protected Map mallocMap = new HashMap();
	protected Map constructorCache = new HashMap();
	protected boolean profile = false;
	protected boolean passThrough = false;
	protected int maxFree = 5;
	protected static final Object[] emptyArgs = new Object[0];
	protected static final Class[] emptySig = new Class[0];

	protected int standardAllocTime = 0;
	protected int cachedAllocTime = 0;
	protected int introspectionTime = 0;
	protected int freeTime = 0;

	protected static class MallocDataHolder {
		protected Object[] free;

		protected int maxAlloc = 0;
		protected int inUse = 0;
		protected int freeCount = 0;

		public MallocDataHolder(int max) {
			free = new Object[max];
		}
	}

	protected static class ObjectHolder {
		protected Object object;
		protected Exception creationException;

		public ObjectHolder(Object object) {
			this.object = object;
			creationException = new Exception();
		}
	}

	public MemoryAllocator() {
		this(false, false, 5);
	}

	public MemoryAllocator(boolean profile, boolean passThrough, int maxFree) {
		this.profile = profile;
		this.maxFree = maxFree;
		this.passThrough = passThrough;
	}

	public void setPassthrough(boolean passThrough) {
		this.passThrough = passThrough;
	}

	public Object malloc(Class mClass) {
		try {
			Constructor c = (Constructor) constructorCache.get(mClass);
			if (c == null) {
				c = mClass.getConstructor(emptySig);
				constructorCache.put(mClass, c);
			}
			return malloc(c);
		} catch (NoSuchMethodException ex) {
			throw new IllegalArgumentException("Can't do a class-based "
					+ "malloc on a class with no "
					+ "zero-argument constructor.");
		}
	}

	public Object malloc(Constructor constructor) {
		return malloc(constructor, emptyArgs);
	}

	public Object malloc(Constructor constructor, Object[] args) {
		if (passThrough) {
			try {
				return constructor.newInstance(args);
			} catch (Exception ex) {
				return null;
			}
		}
		Object out = null;

		MallocDataHolder mdh = (MallocDataHolder) mallocMap.get(constructor);
		if (mdh == null) {
			mdh = new MallocDataHolder(maxFree);
			mallocMap.put(constructor, mdh);
		}

		mdh.inUse++;
		if (mdh.inUse > mdh.maxAlloc)
			mdh.maxAlloc = mdh.inUse;

		if (mdh.freeCount > 0) {
			out = mdh.free[--mdh.freeCount];

			if (out != null) {
				return out;
			}
		}
		try {
			out = constructor.newInstance(args);
		} catch (Exception ex) {
			out = null;
			return out;
		}

		return out;
	}

	public void report() {
		Iterator it = mallocMap.keySet().iterator();
		while (it.hasNext()) {
			Constructor c = (Constructor) it.next();
			MallocDataHolder mdh = (MallocDataHolder) mallocMap.get(c);
			if (mdh.inUse > 0) {
				System.err.println("Data for " + c);
				System.err.println("    max = " + mdh.maxAlloc);
				System.err.println("    inUse = " + mdh.inUse);
				System.err.println("    cache = " + mdh.freeCount);
			}
		}
		/*
		 * if (profile) { System.err.println("standard alloc time =
		 * "+standardAllocTime); System.err.println("cached alloc time =
		 * "+cachedAllocTime+" (intro "+introspectionTime+")");
		 * System.err.println("free time = "+freeTime); if (standardAllocTime <
		 * cachedAllocTime) { System.err.println("!!!!!!!!!!!!!"); } }
		 */
	}

	public void finalize() {
		report();
	}

	public static Constructor getNoArgConstructor(Class c) {
		try {
			Constructor constructor = c.getConstructor(emptySig);
			return constructor;
		} catch (NoSuchMethodException ex) {
			return null;
		}
	}

	public void free(Object o) {
		free(o, o.getClass());
	}

	protected void free(Object o, Class fClass) {
		try {
			Constructor c = (Constructor) constructorCache.get(fClass);
			if (c == null) {
				c = fClass.getConstructor(emptySig);
				constructorCache.put(fClass, c);
			}
			free(o, c);
		} catch (NoSuchMethodException ex) {
			throw new IllegalArgumentException("Can't do a class-based "
					+ "free on a class with no " + "zero-argument constructor.");
		}
	}

	public void free(Object o, Constructor c) {
		if (passThrough)
			return;

		MallocDataHolder mdh = (MallocDataHolder) mallocMap.get(c);
		if (mdh == null)
			throw new IllegalArgumentException("Cannot free object " + o
					+ "that was never mallocked");
		mdh.inUse--;
		if (mdh.freeCount < maxFree) {
			mdh.free[mdh.freeCount++] = o;
		}
	}
}
