package org.bbop.util;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;

import org.bbop.framework.GUITask;

public class CollectionUtil {

	public final static EmptyIterator EMPTY_ITERATOR = EmptyIterator.EMPTY_ITERATOR;

	protected final static ComparableComparator comparableComparator = new ComparableComparator();

	private final static EqualsEqualityComparator equalityComparator = new EqualsEqualityComparator();

	private CollectionUtil() {
	}

	public static <T> T[] array(T... in) {
		return in;
	}

	public static <K, V> MultiMap<V, K> invertMap(MultiMap<K, V> map) {
		MultiMap<V, K> out = new MultiHashMap<V, K>();
		for (K key : map.keySet()) {
			Collection<V> vals = map.get(key);
			for (V val : vals) {
				out.add(val, key);
			}
		}
		return out;
	}

	/**
	 * Tests to see if two vectors contain the same data, perhaps in a different
	 * order
	 * 
	 * @param a
	 *            a vector
	 * @param b
	 *            a vector
	 * @return whether or not the vectors have the same elements
	 */
	public static boolean hasSameContents(Collection a, Collection b) {
		return hasSameContents(a, b, equalityComparator);
	}

	/**
	 * Tests to see if two vectors contain the same data, perhaps in a different
	 * order
	 * 
	 * @param a
	 *            a vector
	 * @param b
	 *            a vector
	 * @param cmp
	 *            the comparitor to use for elements of the vector
	 * @return whether or not the vectors have the same elements
	 */
	public static boolean hasSameContents(Collection a, Collection b,
			EqualityComparator cmp) {
		if (a.size() != b.size())
			return false;
		Iterator it = a.iterator();
		while (it.hasNext()) {
			if (!contains(b, it.next(), cmp))
				return false;
		}
		return true;
	}

	/**
	 * Tests to see if a vector contains a certain object, given a user
	 * specified equality comparator
	 * 
	 * @param vector
	 *            a vector
	 * @param o
	 *            the object to find
	 * @param cmp
	 *            the comparitor to use for elements of the vector
	 * @return whether vector contains an element that is equal to o
	 */
	public static boolean contains(Collection vector, Object o,
			EqualityComparator cmp) {
		Iterator it = vector.iterator();
		while (it.hasNext())
			if (cmp.equals(it.next(), o))
				return true;
		return false;
	}

	/**
	 * Creates a sorted list from an arbitrary collection.
	 * 
	 * @param v
	 *            the collection to sort
	 * @param c
	 *            the comparator to use to determine sort ordering
	 * @param duplicates
	 *            if false, discard duplicate items
	 */
	public static List createSortedList(Collection v, Comparator c,
			boolean duplicates) {
		List out = new ArrayList();
		Iterator it = v.iterator();
		while (it.hasNext()) {
			insertSorted(out, c, it.next(), duplicates);
		}
		return out;
	}

	/**
	 * Inserts an object into a sorted vector so that the vector remains sorted.
	 * The insert is done in log(n)+(vector insert cost) time.
	 * 
	 * @param v
	 *            the vector to insert into
	 * @param c
	 *            the comparator that defines the sort order on this vector
	 * @param o
	 *            the object to insert
	 * @param duplicates
	 *            whether duplicate items can be inserted into the list
	 * 
	 * @return whether the insert was successful
	 */
	public static boolean insertSorted(List v, Comparator c, Object o,
			boolean duplicates) {
		int pos = Collections.binarySearch(v, o, c);
		if (duplicates && pos >= 0) {
			if (pos >= v.size())
				pos = v.size() - 1;
			if (pos < 0)
				pos = 0;
			v.add(pos, o);
			return true;
		} else if (pos < 0) {
			v.add(-pos - 1, o);
			return true;
		} else
			return false;
	}

	/**
	 * Inserts an object into a sorted vector so that the vector remains sorted.
	 * The insert is done in log(n)+(vector insert cost) time. Duplicate items
	 * will not be inserted.
	 * 
	 * @param v
	 *            the vector to insert into
	 * @param c
	 *            the comparator that defines the sort order on this vector
	 * @param o
	 *            the object to insert
	 * 
	 * @return whether the insert was successful
	 */
	public static boolean insertSorted(List v, Comparator c, Object o) {
		return insertSorted(v, c, o, true);
	}

	/**
	 * Inserts an comparable object into a sorted vector so that the vector
	 * remains sorted. The insert is done in log(n)+(vector insert cost) time.
	 * 
	 * @param v
	 *            the vector to insert into
	 * @param o
	 *            the object to insert
	 */
	public static void insertSorted(List v, Object o) {
		insertSorted(v, comparableComparator, o);
	}

	/**
	 * Copies the contents of some collection into an iterator by doing sorted
	 * insertions. This may be faster than creating the list and then sorting
	 * it, depending on your list implementations.
	 */
	public static void copyIntoSorted(Iterator it, List dest,
			Comparator comparator) {
		while (it.hasNext()) {
			insertSorted(dest, comparator, it.next());
		}
	}

	/**
	 * Copies the contents of some collection into a list by doing sorted
	 * insertions. This should be slightly faster than creating the list and
	 * then sorting it, depending on your list implementations.
	 */
	public static void copyIntoSorted(Collection source, List dest,
			Comparator comparator) {
		copyIntoSorted(source.iterator(), dest, comparator);
	}

	public static void addAll(Collection c, Iterator it) {
		while (it.hasNext()) {
			c.add(it.next());
		}
	}

	public static boolean contains(Iterator it, Object o) {
		while (it.hasNext()) {
			if (ObjectUtil.equals(o, it.next()))
				return true;
		}
		return false;
	}

	public static Collection initialize(Collection c, Iterator it) {
		c.clear();
		addAll(c, it);
		return c;
	}

	public static <T> CollectionChanges<T> getChanges(Collection<T> cold,
			Collection<T> cnew) {
		final LinkedList<T> added = new LinkedList<T>();
		final LinkedList<T> deleted = new LinkedList<T>();
		for (T oldobj : cold) {
			if (!cnew.contains(oldobj))
				deleted.add(oldobj);
		}
		for (T newobj : cnew) {
			if (!cold.contains(newobj))
				added.add(newobj);
		}
		return new CollectionChanges<T>() {

			public Collection<T> getAddedItems() {
				return added;
			}

			public Collection<T> getDeletedItems() {
				return deleted;
			}
		};
	}

	public static <T> CollectionChanges<T> getChanges(Collection<T> cold,
			Collection<T> cnew, EqualityComparator eq) {
		if (eq == null)
			eq = equalityComparator;
		final LinkedList<T> added = new LinkedList<T>();
		final LinkedList<T> deleted = new LinkedList<T>();
		for (T oldobj : cold) {
			boolean found = false;
			for (T newobj : cnew) {
				if (eq.equals(oldobj, newobj)) {
					found = true;
					break;
				}
			}
			if (!found)
				deleted.add(oldobj);
		}
		for (T newobj : cnew) {
			boolean found = false;
			for (T oldobj : cnew) {
				if (eq.equals(oldobj, newobj)) {
					found = true;
					break;
				}
			}
			if (!found)
				added.add(newobj);
		}
		return new CollectionChanges<T>() {

			public Collection<T> getAddedItems() {
				return added;
			}

			public Collection<T> getDeletedItems() {
				return deleted;
			}
		};
	}

	public static <T> List<T> deepCopy(Collection<T> c) {
		if (c instanceof List) {
			return deepCopy((List<T>) c);
		} else
			return deepCopy(new LinkedList<T>(c));
	}

	public static <T> List<T> deepCopy(List<T> list) {
		return deepCopy(list, null);
	}

	public static <T> List<T> deepCopy(List<T> list, List<T> dest) {
		if (dest == null)
			dest = new ArrayList(list.size());
		else
			dest.clear();
		for (T t : list) {
			if (t instanceof List) {
				dest.add((T) deepCopy((List) t, null));
			} else {
				T newT = (T) ObjectUtil.cloneObject(t);
				if (newT == null)
					newT = t;
				dest.add(newT);
			}
		}
		return dest;
	}

	/**
	 * Creates a list by pulling off the head of a queue one item at a time and
	 * adding it to an output list. The input queue is emptied by this method.
	 * 
	 * @param <T>
	 * @param queue
	 * @param outputList
	 * @param reverse
	 *            whether or not to reverse the resulting list
	 * @return
	 */
	public static <T> List<T> createList(Queue<T> queue, List<T> outputList,
			boolean reverse) {
		if (outputList == null) {
			outputList = new ArrayList<T>(queue.size());
		}
		while (!queue.isEmpty()) {
			outputList.add(queue.poll());
		}
		if (reverse) {
			Collections.reverse(outputList);
		}
		return outputList;
	}

	public static <T> List<T> list(T... items) {
		ArrayList<T> out = new ArrayList<T>();

		for (T t : items) {
			out.add(t);
		}

		return out;
	}

	/**
	 * Creates a new empty instance of the provided collection
	 * 
	 * @param <T>
	 * @param in
	 * @return
	 */
	public static <T> Collection<T> createEmpty(Collection<?> in) {
		Class<?> originalClass = in.getClass();
		try {
			Constructor<?> originalConstructor = originalClass
					.getConstructor(new Class[0]);
			return (Collection<T>) originalConstructor
					.newInstance(new Object[0]);
		} catch (IllegalArgumentException e) {
		} catch (InstantiationException e) {
		} catch (IllegalAccessException e) {
		} catch (InvocationTargetException e) {
		} catch (SecurityException e) {
		} catch (NoSuchMethodException e) {
		} finally {
			return null;
		}
	}

	public static <T> Collection<T> getObjectsOfType(
			Collection<?> input, Class<T> type) {
		return getObjectsOfType(input, type, null);
	}

	public static <T> Collection<T> getObjectsOfType(
			Collection<?> input, Class<T> type, Collection<T> output) {
		if (output == null) {
			output = createEmpty(input);
			if (output == null)
				output = new ArrayList<T>();
		}
		for (Object o : input) {
			if (type.isAssignableFrom(o.getClass()))
				output.add((T) o);
		}
		return output;
	}
}
