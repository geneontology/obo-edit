package org.bbop.util;

import java.util.*;

/**
 * A collection of static methods for manipulating Vectors.
 * 
 * @see java.util.Vector
 */
public class VectorUtil {

	private final static ComparableComparator comparableComparator = new ComparableComparator();
	private final static EqualsEqualityComparator equalityComparator = new EqualsEqualityComparator();

	private VectorUtil() {
	}

	/**
	 * Adds all the elements from vector b into vector a. Equivalent to
	 * a.addAll(b); Note that a's contents are modified in place
	 * 
	 * @param a
	 *            the vector to merge into
	 * @param b
	 *            the vector to merge from
	 * @return the resulting vector (always the same as a)
	 */
	public static Vector mergeVectors(Vector a, Vector b) {
		for (int i = 0; i < b.size(); i++) {
			if (!a.contains(b.elementAt(i)))
				a.addElement(b.elementAt(i));
		}
		return a;
	}

	public static Vector condense(Vector vector, VectorCondenser condenser) {
		int oldCount = 0;
		do {
			oldCount = vector.size();
			vector = VectorUtil.doCondense(vector, condenser);
		} while (oldCount != vector.size());

		return vector;
	}

	private static Vector doCondense(Vector vector, VectorCondenser condenser) {
		vector = (Vector) vector.clone();
		Vector out = new Vector();

		int lastIndex = 0;
		while (lastIndex < vector.size()) {
			Object item = vector.elementAt(lastIndex);
			if (lastIndex + 1 >= vector.size()) {
				out.addElement(item);
				break;
			}
			boolean condensed = false;
			for (int i = lastIndex + 1; i < vector.size(); i++) {
				Object item2 = vector.elementAt(i);
				Pair pair = condenser.condense(item, item2);
				if (pair != null
						&& (pair.getA() != null || pair.getB() != null)) {
					condensed = true;
					if (pair.getB() == VectorCondenser.REMOVE_OP) {
						vector.removeElementAt(i);
					} else if (pair.getB() != null) {
						vector.setElementAt(pair.getB(), i);
					}

					if (pair.getA() == VectorCondenser.REMOVE_OP) {
						vector.removeElementAt(lastIndex);
					} else if (pair.getA() != null) {
						vector.setElementAt(pair.getA(), lastIndex);
					}
					break;
				}
			}
			if (!condensed) {
				out.addElement(item);
				lastIndex++;
			}
		}
		return out;
	}

	/**
	 * Creates a new Vector containing clones of all the elements of the
	 * original Vector. If an element cannot be cloned, it will be set to null.
	 * 
	 * @param values
	 *            the Vector to clone
	 * @return a vector containing clones of all the elements of values
	 */
	public static Vector trueClone(Vector values) {
		Vector out = new Vector(values.size());
		for (int i = 0; i < values.size(); i++) {
			out.addElement(ObjectUtil.cloneObject(values.elementAt(i)));
		}
		return out;
	}

	/**
	 * Creates a new List containing clones of all the elements of the original
	 * List. If an element cannot be cloned, it will be set to null.
	 * 
	 * @param values
	 *            the List to clone
	 * @return a List containing clones of all the elements of values
	 */
	public static List trueClone(List values) {
		Vector out = new Vector(values.size());
		for (int i = 0; i < values.size(); i++) {
			out.add(ObjectUtil.cloneObject(values.get(i)));
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
	public static boolean hasSameContents(Vector a, Vector b) {
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
	public static boolean hasSameContents(Vector a, Vector b,
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
	public static boolean contains(Vector vector, Object o,
			EqualityComparator cmp) {
		Iterator it = vector.iterator();
		while (it.hasNext())
			if (cmp.equals(it.next(), o))
				return true;
		return false;
	}

	/**
	 * Creates a transformed Vector by calling transformers transform method on
	 * each element in values. The original Vector is not modified
	 * 
	 * @param transformer
	 *            the VectorTransformer to apply
	 * @param values
	 *            the Vector to transform
	 * @return the transformed Vector
	 * @see org.bbop.util.VectorTransformer
	 */
	public static Vector transform(VectorTransformer transformer, Vector values) {
		return filter(null, transformer, values);
	}

	/**
	 * Creates a transformed Vector by calling transformers transform method on
	 * each element in values. The enumeration is emptied by calling this method
	 * 
	 * @param transformer
	 *            the VectorTransformer to apply
	 * @param values
	 *            the enumeration to transform
	 * @return the transformed Vector
	 * @see org.bbop.util.VectorTransformer
	 */
	public static Vector transform(VectorTransformer transformer,
			Enumeration values) {
		return filter(null, transformer, values);
	}

	/**
	 * Creates a filtered Vector by calling the satisfies() method of filter on
	 * each element of values. If the satisfies() method returns true, that
	 * element is added to the output vector. The original Vector is not
	 * modified.
	 * 
	 * @param filter
	 *            the VectorFilter to apply
	 * @param values
	 *            the Vector to filter
	 * @return the filtered Vector
	 * @see org.bbop.util.VectorFilter
	 */
	public static Vector filter(VectorFilter filter, Vector values) {
		return filter(filter, null, values);
	}

	/**
	 * Creates a filtered Vector by calling the satisfies() method of filter on
	 * each element of values. If the satisfies() method returns true, that
	 * element is added to the output vector. The enumeration is emptied by
	 * calling this method.
	 * 
	 * @param filter
	 *            the VectorFilter to apply
	 * @param values
	 *            the Enumeration to filter
	 * @return the filtered Vector
	 * @see org.bbop.util.VectorFilter
	 */
	public static Vector filter(VectorFilter filter, Enumeration values) {
		return filter(filter, null, values);
	}

	/**
	 * Filters and transforms a Vector. The equivalent of calling
	 * VectorUtil.transform(transformer, VectorUtil.filter(filter, values))
	 * 
	 * @param filter
	 *            the VectorFilter to apply
	 * @param values
	 *            the Vector to filter and transform
	 * @return the filtered Vector
	 */
	public static Vector filter(VectorFilter filter,
			VectorTransformer transformer, Vector values) {
		Vector out = new Vector(values.size());
		for (int i = 0; i < values.size(); i++) {
			Object o = values.get(i);
			if (filter == null || filter.satisfies(o)) {
				if (transformer != null)
					o = transformer.transform(o);
				out.addElement(o);
			}
		}
		out.trimToSize();
		return out;
	}

	/**
	 * Filters and transforms an Enumeration. The equivalent of calling
	 * VectorUtil.transform(transformer, VectorUtil.filter(filter, values))
	 * 
	 * @param filter
	 *            the VectorFilter to apply
	 * @param values
	 *            the Enumeration to filter and transform
	 * @return the filtered Vector
	 */
	public static Vector filter(VectorFilter filter,
			VectorTransformer transformer, Enumeration values) {
		Vector out = new Vector();
		while (values.hasMoreElements()) {
			Object o = values.nextElement();
			if (filter == null || filter.satisfies(o)) {
				if (transformer != null)
					o = transformer.transform(o);
				out.addElement(o);
			}
		}
		out.trimToSize();
		return out;
	}

	/**
	 * Creates a Vector containing all the elements of an Enumeration
	 * 
	 * @param e
	 *            an enumeration
	 * @return a vector containing the elements of the enumeration
	 */
	public static Vector getVector(Enumeration e) {
		Vector out = new Vector();
		while (e.hasMoreElements())
			out.addElement(e.nextElement());
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
		return CollectionUtil.insertSorted(v, c, o, duplicates);
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
		return CollectionUtil.insertSorted(v, c, o);
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
		CollectionUtil.insertSorted(v, o);
	}
}
