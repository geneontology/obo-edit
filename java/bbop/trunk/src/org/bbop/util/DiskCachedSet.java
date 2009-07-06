package org.bbop.util;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.util.AbstractSet;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;

import org.bbop.io.IOUtil;

import org.apache.log4j.*;

public class DiskCachedSet<E> extends AbstractSet<E> implements Set<E> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DiskCachedSet.class);

	protected int cacheSize = 0;

	protected File diskPath;

	protected Hasher hasher;

	protected Set memCache;

	protected Exception deathException;

	protected boolean assumeNoClashes = true;

	public DiskCachedSet() {
		this(null, null, false, 0);
	}

	public DiskCachedSet(File file, Hasher hasher, boolean assumeNoClashes,
			int cacheSize) {
		if (hasher == null)
			this.hasher = DefaultHasher.getInstance();
		else
			this.hasher = hasher;
		if (file == null) {
			try {
				diskPath = IOUtil.createTempDir("diskcachedset", "");
			} catch (Exception ex) {
				die(ex);
			}
		} else {
			this.diskPath = file;
		}
		this.assumeNoClashes = assumeNoClashes;
		setCacheSize(cacheSize);
	}

	public void setCacheSize(int cacheSize2) {

	}

	protected void die(Exception ex) {
		// only set the death exception if it isn't already set
		if (deathException == null) {
			deathException = ex;
			setCacheSize(-1);
		}
	}

	protected Set createMemCache() {
		return new TreeSet();
	}

	protected File getPathForObject(E o) {
		return new File(diskPath, hasher.hashCode(o));
	}

	protected byte getClashPileSize(File file) throws IOException {
		if (!file.exists())
			return 0;
		InputStream istream = new BufferedInputStream(new FileInputStream(file));
		byte out = (byte) istream.read();
		istream.close();
		return out;
	}

	@SuppressWarnings("unchecked")
	protected Collection<E> getClashPile(File file) throws IOException,
			ClassNotFoundException {
		Collection<E> clashPile;

		if (!file.exists()) {
			clashPile = new LinkedList<E>();
		} else {
			InputStream istream = new BufferedInputStream(new FileInputStream(
					file));
			if (assumeNoClashes) {
				clashPile = new LinkedList<E>();
				ObjectInputStream oistream = new ObjectInputStream(istream);
				clashPile.add((E) oistream.readObject());
				
				oistream.close();				
			} else {
				// throw away the clash byte, we don't care
				istream.read();
				ObjectInputStream oistream = new ObjectInputStream(istream);
				clashPile = (Collection<E>) oistream.readObject();
				oistream.close();
			}
			istream.close();
		}
		return clashPile;
	}

	protected void writeClashPile(File file, Collection<E> clashPile)
			throws IOException {
		if (clashPile == null || clashPile.size() == 0) {
			file.delete();
		} else {
			if (clashPile.size() > 1) {
				logger.info("clashes found on keyfile " + file);
			}
			OutputStream stream = new BufferedOutputStream(
					new FileOutputStream(file));
			if (assumeNoClashes) {
				ObjectOutputStream ostream = new ObjectOutputStream(stream);
				ostream.writeObject(clashPile.iterator().next());
				ostream.close();
			} else {
				// the first byte should be set to the number of items in the
				// clashpile
				stream.write(clashPile.size());
				ObjectOutputStream ostream = new ObjectOutputStream(stream);
				ostream.writeObject(clashPile);
				ostream.close();
			}
			stream.close();
		}
	}

	protected void writeToDisk(E o) {
		File file = getPathForObject(o);
		try {
			Collection<E> clashPile = getClashPile(file);
			clashPile.add(o);
			writeClashPile(file, clashPile);
		} catch (Exception ex) {
			die(ex);
		}
	}

	protected E fetchFromDisk(E o) {
		File file = getPathForObject(o);
		try {
			Collection<E> clashPile = getClashPile(file);
			Iterator<E> it = clashPile.iterator();
			while (it.hasNext()) {
				E item = it.next();
				if (hasher.equals(item, o)) {
					return item;
				}
			}
		} catch (Exception ex) {
			die(ex);
		}

		return null;
	}

	protected void deleteFromDisk(E o) {
		File file = getPathForObject(o);
		try {
			Collection<E> clashPile = getClashPile(file);
			Iterator it = clashPile.iterator();
			while (it.hasNext()) {
				Object item = it.next();
				if (hasher.equals(item, o)) {
					it.remove();
				}
			}
			writeClashPile(file, clashPile);
		} catch (Exception ex) {
			die(ex);
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean contains(Object o) {
		if (assumeNoClashes) {
			return getPathForObject((E) o).exists();
		} else {
			return fetchFromDisk((E) o) != null;
		}
	}

	@Override
	public boolean add(E o) {
		if (contains(o))
			return false;
		writeToDisk(o);
		return true;
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean remove(Object o) {
		if (!contains(o))
			return false;
		deleteFromDisk((E) o);
		return true;
	}

	protected class DiskIterator implements Iterator<E> {
		protected File[] files;

		protected int currentIndex;

		protected Iterator<E> currentIterator;

		public DiskIterator(File diskPath) {
			files = diskPath.listFiles();
			currentIndex = 0;
			currentIterator = getCurrentCollection().iterator();
		}

		public E next() {
			E out = currentIterator.next();
			if (!currentIterator.hasNext()) {
				currentIndex++;
				if (currentIndex >= files.length) {
					currentIterator = null;
				} else {
					currentIterator = getCurrentCollection().iterator();
				}
			}
			return out;
		}

		public boolean hasNext() {
			return currentIterator != null;
		}

		protected Collection<E> getCurrentCollection() {
			try {
				return getClashPile(files[currentIndex]);
			} catch (Exception ex) {
				die(ex);
				return Collections.emptySet();
			}
		}

		public void remove() {
			throw new UnsupportedOperationException();
		}
	}

	@Override
	public Iterator<E> iterator() {
		return new DiskIterator(diskPath);
	}

	@Override
	public int size() {
		int out = 0;
		try {
			File[] files = diskPath.listFiles();
			for (File file : files) {
				out += getClashPileSize(file);
			}
		} catch (Exception ex) {
			die(ex);
		}
		return out;
	}

	private static long addToSet(Set s, Object o) {
		long time = System.nanoTime();
		s.add(o);
		return System.nanoTime() - time;
	}

	private static long fetchFromSet(Set s, Object o) {
		long time = System.nanoTime();
		s.contains(o);
		return System.nanoTime() - time;
	}
	
	private static class SetTest {
		public Set set;
		public long time = 0;
		public String name;
		public SetTest(String name, Set set) {
			this.name = name;
			this.set = set;
		}
	}
	
	private static void dumpResults(String name, Collection<SetTest> c) {
		logger.info(name+":");
		for(SetTest st: c) {
			logger.info("   "+st.name+" = "+st.time);
			st.time = 0;
		}
		
	}

	public static void main(String[] args) {
		LinkedList<SetTest> testSets = new LinkedList<SetTest>();
		testSets.add(new SetTest("DiskCachedSet(checkclashes)", new DiskCachedSet()));
		testSets.add(new SetTest("DiskCachedSet(noclashes)", new DiskCachedSet(null, null, true, 0)));
		testSets.add(new SetTest("HashSet", new HashSet()));
		testSets.add(new SetTest("TreeSet", new TreeSet()));
		
		Random random = new Random();
		LinkedList<String> stringKeeper = new LinkedList<String>();
		for (int i = 0; i < 10000; i++) {
			String s = random.nextInt() + "";
			for(SetTest st: testSets) {
				st.time += addToSet(st.set, s);
			}
			stringKeeper.add(s);
		}
		dumpResults("ADD", testSets);
		for (int i = 0; i < 10000; i++) {
			String s = random.nextInt() + "";
			for(SetTest st: testSets) {
				st.time += fetchFromSet(st.set, s);
			}
		}
		dumpResults("FETCH MISSES", testSets);
		for (String s : stringKeeper) {
			for(SetTest st: testSets) {
				st.time += fetchFromSet(st.set, s);
			}
		}
		dumpResults("FETCH HITS", testSets);
	}
}
