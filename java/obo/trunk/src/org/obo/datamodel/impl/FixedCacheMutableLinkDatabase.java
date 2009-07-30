package org.obo.datamodel.impl;

import java.util.Collection;
import java.util.LinkedList;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.IdentifiedObjectIndex;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.MutableLinkDatabase;

/**
 * This is a MutableLinkDatabase that wraps two other MutableLinkDatabases,
 * called the "cache" database and the "store" database. The cache database is
 * associated with three parameters: the preferred size < the flush size < the
 * maximum size. When the number of objects in the cache database reaches the
 * maximum size, the least recently used objects in the database are flushed to
 * the write database until the cache database returns to its preferred size.
 * 
 * If the caching thread is enabled and the number of items in the database
 * exceeds the flush size, a low priority thread will flush items to the write
 * database until the cache database returns to its preferred size.
 * 
 * The intention is that a FixedCacheMutableLinkDatabase would be used with an
 * in-memory cache database (like a {@link DefaultMutableLinkDatabase}) and the
 * write database would be an on disk mutable database (like a
 * {@link SQLBackedMutableLinkDatabase}).
 * 
 * @author jrichter
 * 
 */

import org.apache.log4j.*;

public class FixedCacheMutableLinkDatabase extends AbstractLinkDatabase implements MutableLinkDatabase {

	protected class FlushThread extends Thread {
		public static final long SLEEP_TIME = 100;
		protected boolean cancelled = false;
		//initialize logger
		protected final Logger logger = Logger.getLogger("");

		public void cancel() {
			cancelled = true;
			try {
				join();
			} catch (InterruptedException e) {
			}
			flushThread = null;
		}

		@Override
		public void run() {
			while (parentsCached > preferredSize) {
				if (cancelled)
					return;
				flushLRU();
				if (cancelled)
					return;
				try {
					sleep(SLEEP_TIME);
				} catch (InterruptedException e) {
				}
			}
		}
	}

	protected DefaultMutableLinkDatabase cacheDatabase;
	protected MutableLinkDatabase storeDatabase;

	protected int preferredSize = 1000;
	protected int flushSize = 2000;
	protected int maxSize = 3000;

	protected int parentsCached = 0;

	protected FlushThread flushThread;
	protected boolean useFlushThread;

	public FixedCacheMutableLinkDatabase(IdentifiedObjectIndex index,
			boolean useFlushThread) {
		this(new SQLBackedMutableLinkDatabase(index), useFlushThread);
	}

	public FixedCacheMutableLinkDatabase(MutableLinkDatabase storeDatabase,
			boolean useFlushThread) {
		this.cacheDatabase = new DefaultMutableLinkDatabase(true, true);
		this.storeDatabase = storeDatabase;
		setUseFlushThread(useFlushThread);
	}

	public void setUseFlushThread(boolean useFlushThread) {
		this.useFlushThread = useFlushThread;
	}

	public void addObject(IdentifiedObject lo) {
		cacheDatabase.addObject(lo);
		flushIfNecessary();
	}

	protected synchronized void flushLRU() {
		LinkedObject lo = cacheDatabase.getLeastRecentlyAccessedLinkKey();
		Collection<Link> parents = cacheDatabase.getParents(lo);
		int parentsCount = parents.size();
		storeDatabase.setParents(lo, parents);
		cacheDatabase.dropLinkKey(lo);
		parentsCached -= parentsCount;
		Logger.getLogger("").warning(
				"flushed " + lo + " out of memory to reduce cache size to "
						+ parentsCached);
	}

	protected synchronized void flushIfNecessary() {
		if (parentsCached > maxSize) {
			if (flushThread != null)
				flushThread.cancel();
			flushThread = null;
			/*
			 * LinkedList<Link> parentPile = new LinkedList<Link>();
			 * LinkedList<LinkedObject> clearMe = new LinkedList<LinkedObject>();
			 */
			while (parentsCached > preferredSize) {
				LinkedObject lo = cacheDatabase
						.getLeastRecentlyAccessedLinkKey();
				Collection<Link> parents = cacheDatabase.getParents(lo);
				int parentsCount = parents.size();
				/*
				 * clearMe.add(lo); parentPile.addAll(parents);
				 */
				storeDatabase.setParents(lo, parents);
				cacheDatabase.dropLinkKey(lo);
				Collection<Link> newParents = getParents(lo);
				parentsCached -= parentsCount;
			}
			/*
			 * Logger.getLogger("").warning("committing database write...");
			 * long time = System.currentTimeMillis();
			 * storeDatabase.clearObjectsAndAddParents(clearMe, parentPile);
			 * time = System.currentTimeMillis() - time;
			 * Logger.getLogger("").warning("...completed database commit in
			 * "+time);
			 */
		} else if (useFlushThread && parentsCached > flushSize
				&& flushThread == null) {
			flushThread = new FlushThread();
			flushThread.start();
		}
	}

	public synchronized void addParent(Link link) {
		cacheDatabase.addParent(link);
		parentsCached++;
		flushIfNecessary();
	}

	public synchronized void clear() {
		cacheDatabase.clear();
		storeDatabase.clear();
		parentsCached = 0;
	}

	public synchronized void clearParents(LinkedObject lo) {
		Collection<Link> parents = getParents(lo);
		cacheDatabase.clearParents(lo);
		storeDatabase.clearParents(lo);
		parentsCached -= parents.size();
	}

	public void removeObject(IdentifiedObject lo) {
		cacheDatabase.removeObject(lo);
		storeDatabase.removeObject(lo);
	}

	public synchronized void removeParent(Link link) {
		cacheDatabase.removeParent(link);
		storeDatabase.removeParent(link);
		parentsCached--;
	}

	public void setIdentifiedObjectIndex(IdentifiedObjectIndex index) {
		cacheDatabase.setIdentifiedObjectIndex(index);
		storeDatabase.setIdentifiedObjectIndex(index);
	}

	public synchronized void setParents(LinkedObject lo,
			Collection<Link> parents) {
		Collection<Link> oldparents = getParents(lo);
		cacheDatabase.setParents(lo, parents);
		parentsCached += parents.size() - oldparents.size();
		flushIfNecessary();
	}

	public synchronized Collection<Link> getChildren(LinkedObject lo) {
		Collection<Link> children = cacheDatabase.getChildren(lo);
		if (children == null) {
			children = storeDatabase.getChildren(lo);
			for (Link link : children) {
				cacheDatabase.addParent(link);
			}
			flushIfNecessary();
		}
		return children;
	}

	public synchronized Collection<IdentifiedObject> getObjects() {
		return cacheDatabase.getObjects();
	}

	public synchronized Collection<Link> getParents(LinkedObject lo) {
		Collection<Link> parents = cacheDatabase.getParents(lo);
		if (parents == null) {
			parents = storeDatabase.getParents(lo);
			for (Link link : parents) {
				cacheDatabase.addParent(link);
			}
			flushIfNecessary();
		}
		return parents;
	}

	public IdentifiedObject getObject(String id) {
		return cacheDatabase.getObject(id);
	}

	public int getPreferredSize() {
		return preferredSize;
	}

	public void setPreferredSize(int preferredSize) {
		this.preferredSize = preferredSize;
	}

	public int getFlushSize() {
		return flushSize;
	}

	public void setFlushSize(int flushSize) {
		this.flushSize = flushSize;
	}

	public int getMaxSize() {
		return maxSize;
	}

	public void setMaxSize(int maxSize) {
		this.maxSize = maxSize;
	}

	public boolean hasChildren(LinkedObject lo) {
		return cacheDatabase.hasChildren(lo) || storeDatabase.hasChildren(lo);
	}

	public boolean hasParents(LinkedObject lo) {
		return cacheDatabase.hasParents(lo) || storeDatabase.hasParents(lo);
	}
}
