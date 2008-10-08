package org.obo.util;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;

import org.bbop.dataadapter.DataAdapterException;
import org.bbop.util.AbstractTaskDelegate;
import org.bbop.util.EmptyIterator;
import org.bbop.util.IteratorFactory;
import org.bbop.util.MultiHashMap;
import org.bbop.util.ObjectUtil;
import org.bbop.util.SuperIterator;
import org.obo.dataadapter.OBOAdapter;
import org.obo.dataadapter.OBOFileAdapter;
import org.obo.datamodel.DanglingObject;
import org.obo.datamodel.Datatype;
import org.obo.datamodel.DatatypeValue;
import org.obo.datamodel.IdentifiableObject;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Impliable;
import org.obo.datamodel.Instance;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.ObsoletableObject;
import org.obo.datamodel.PathCapable;
import org.obo.datamodel.RootAlgorithm;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.SynonymedObject;
import org.obo.datamodel.Value;
import org.obo.datamodel.impl.DanglingClassImpl;
import org.obo.datamodel.impl.DanglingInstanceImpl;
import org.obo.datamodel.impl.DanglingLinkImpl;
import org.obo.datamodel.impl.DanglingPropertyImpl;
import org.obo.datamodel.impl.DefaultLinkDatabase;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.filters.LinkFilter;
import org.obo.history.CompletesHistoryItem;
import org.obo.history.CreateLinkHistoryItem;
import org.obo.history.TermMacroHistoryItem;
import org.obo.reasoner.ReasonedLinkDatabase;

/**
 * A library of methods for working with {@link LinkedObject}s,
 * {@link OBOSession}s and {@link LinkDatabase}s.
 * 
 * @author jrichter
 * @see PathUtil
 * @see HistoryUtil
 * @see ReasonerUtil
 * 
 */
import org.apache.log4j.*;

public class TermUtil {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TermUtil.class);

	public static class AncestorTask extends
	AbstractTaskDelegate<Collection<LinkedObject>> {
		protected Map<LinkedObject, Collection<LinkedObject>> memoizeTable;

		protected LinkedObject term;

		protected LinkDatabase linkDatabase;

		protected LinkFilter linkFilter;

		public AncestorTask(LinkDatabase linkDatabase, LinkedObject term,
				Map<LinkedObject, Collection<LinkedObject>> memoizeTable) {
			this.linkDatabase = linkDatabase;
			this.term = term;
			this.memoizeTable = memoizeTable;
		}

		@Override
		public void execute() {
			setProgressString("Finding ancestors");
			if (memoizeTable == null)
				memoizeTable = new MultiHashMap<LinkedObject, LinkedObject>();
			progress = 0;
			setResults(getAncestors(100, linkDatabase, term, memoizeTable));
		}


		public LinkFilter getLinkFilter() {
			return linkFilter;
		}

		public void setLinkFilter(LinkFilter linkFilter) {
			this.linkFilter = linkFilter;
		}

		protected Collection<LinkedObject> getAncestors(double incSize,
				LinkDatabase linkDatabase, LinkedObject term,
				Map<LinkedObject, Collection<LinkedObject>> memoizeTable) {
			if (linkDatabase == null)
				linkDatabase = DefaultLinkDatabase.getDefault();

			Collection<LinkedObject> out = memoizeTable.get(term);
			if (memoizeTable.containsKey(term)) {
				progress += (int) incSize;
				return out;
			}
			out = new HashSet<LinkedObject>();
			memoizeTable.put(term, out);
			for (Link tr : linkDatabase.getParents(term)) {
				if (linkFilter != null && !linkFilter.satisfies(tr))
					continue;
				out.add(tr.getParent());
				out.addAll(getAncestors(incSize / term.getParents().size(),
						linkDatabase, tr.getParent(), memoizeTable));
			}
			if (term.getParents().size() == 0) {
				progress += (int) incSize;
			}
			return out;
		}
	}

	public static class DescendantTask extends
	AbstractTaskDelegate<Collection<LinkedObject>> {
		protected Map<LinkedObject, Collection<LinkedObject>> memoizeTable;

		protected LinkedObject term;

		protected LinkDatabase linkDatabase;

		public DescendantTask(LinkDatabase linkDatabase, LinkedObject term,
				Map<LinkedObject, Collection<LinkedObject>> memoizeTable) {
			this.linkDatabase = linkDatabase;
			this.term = term;
			this.memoizeTable = memoizeTable;
		}

		@Override
		public void execute() {
			setProgressString("Finding descendants");
			if (memoizeTable == null)
				memoizeTable = new MultiHashMap<LinkedObject, LinkedObject>();
			progress = 0;
			setResults(getDescendants(100, linkDatabase, term, memoizeTable));
		}

		protected Collection<LinkedObject> getDescendants(double incSize,
				LinkDatabase linkDatabase, LinkedObject term,
				Map<LinkedObject, Collection<LinkedObject>> memoizeTable) {
			if (linkDatabase == null)
				linkDatabase = DefaultLinkDatabase.getDefault();

			Collection<LinkedObject> out = memoizeTable.get(term);
			if (memoizeTable.containsKey(term)) {
				progress += (int) incSize;
				return out;
			}
			out = new HashSet<LinkedObject>();
			memoizeTable.put(term, out);

			Iterator it = linkDatabase.getChildren(term).iterator();
			while (it.hasNext()) {
				Link tr = (Link) it.next();
				out.add(tr.getChild());
			}

			it = linkDatabase.getChildren(term).iterator();
			while (it.hasNext()) {
				Link tr = (Link) it.next();
				out.addAll(getDescendants(incSize / term.getChildren().size(),
						linkDatabase, tr.getChild(), memoizeTable));
			}
			if (term.getChildren().size() == 0) {
				progress += (int) incSize;
			}
			return out;
		}
	}

	private TermUtil() {
	}

	/**
	 * Creates a clone of all the nodes down to the given. The leaves of the
	 * cloned tree are term, NOT a clone of term. This means that even though
	 * the cloned parents of the term refer to the original term, the original
	 * term is not modified, and thus does not contain a reference to the cloned
	 * parents.
	 * 
	 * @param term
	 *            the term whose parents should be cloned
	 * @return returns a root of the cloned parent tree
	 */
	public static LinkedObject cloneParentTree(LinkedObject term) {
		if (term.getParents().size() == 0)
			return term;
		OBOClass newRoot = null;
		Iterator it = getAncestors(term).iterator();
		Map<LinkedObject, LinkedObject> newTermHash = new HashMap<LinkedObject, LinkedObject>();
		Map<LinkedObject, LinkedObject> oldTermHash = new HashMap<LinkedObject, LinkedObject>();
		while (it.hasNext()) {
			OBOClass originalTerm = (OBOClass) it.next();
			OBOClass newTerm = (OBOClass) originalTerm.clone();

			if (originalTerm.getID().equals(term.getID())) {
				continue;
			}
			if (originalTerm.getParents().size() == 0)
				newRoot = newTerm;
			newTermHash.put(newTerm, originalTerm);
			oldTermHash.put(originalTerm, newTerm);
		}
		// This is NOT redundant. This has to be done in two passes to
		// guarantee that the newTermHash is fully populated
		it = newTermHash.keySet().iterator();
		while (it.hasNext()) {
			OBOClass newTerm = (OBOClass) it.next();

			OBOClass originalTerm = (OBOClass) newTermHash.get(newTerm);

			newTerm.getParents().clear();
			newTerm.getChildren().clear();
			Iterator it2 = originalTerm.getChildren().iterator();
			while (it2.hasNext()) {
				Link tr = (Link) it2.next();
				OBOClass newChild = (OBOClass) oldTermHash.get(tr.getChild());
				if (newChild != null) {
					Link trNew = new OBORestrictionImpl(newChild, newTerm, tr
							.getType());
					newTerm.atomicAddChild(trNew);
				} else if (tr.getChild() == term) {
					Link trNew = new OBORestrictionImpl(term, newTerm, tr
							.getType());
					newTerm.atomicAddChild(trNew);
				}
			}
			it2 = originalTerm.getParents().iterator();
			while (it2.hasNext()) {
				Link tr = (Link) it2.next();
				OBOClass newParent = (OBOClass) oldTermHash.get(tr.getParent());
				Link trNew = new OBORestrictionImpl(newTerm, newParent, tr
						.getType());
				newTerm.atomicAddParent(trNew);
			}
		}
		return newRoot;
	}

	public static OBOSession getSession(String path)
	throws DataAdapterException {
		OBOFileAdapter adapter = new OBOFileAdapter();
		OBOFileAdapter.OBOAdapterConfiguration config = new OBOFileAdapter.OBOAdapterConfiguration();
		config.getReadPaths().add(path);
		OBOSession session = (OBOSession) adapter.doOperation(
				OBOAdapter.READ_ONTOLOGY, config, null);
		return session;
	}

	/**
	 * Creates a map of id strings to {@link IdentifiedObject}s from the given
	 * collection of {@link IdentifiedObject}s.
	 * 
	 * @param c
	 *            the collection from which the map should be built
	 * @return a map of id strings to IdentifiedObjects
	 */
	public static Map<String, IdentifiedObject> createIDMap(
			Collection<? extends IdentifiedObject> c) {
		Map<String, IdentifiedObject> out = new HashMap<String, IdentifiedObject>();
		for (IdentifiedObject o : c) {
			out.put(o.getID(), o);
		}
		return out;
	}

	/**
	 * Detects the roots in a given collection of {@link IdentifiedObject}s,
	 * using a given {@link LinkDatabase} and {@link RootAlgorithm} to determine
	 * which terms are roots. The results are stored in a collection passed
	 * through the first parameter.
	 * 
	 * @param outSet
	 *            The collection where detected roots will be stored
	 * @param linkDatabase
	 *            provides the parents
	 * @param objects
	 *            the objects to check
	 * @param algorithm
	 *            the {@link RootAlgorithm} to use for root detection
	 */
	public static void detectRoots(Collection<LinkedObject> outSet,
			LinkDatabase linkDatabase, Collection<IdentifiedObject> objects,
			RootAlgorithm algorithm) {
		algorithm.setLinkDatabase(linkDatabase);

		for (IdentifiedObject io : objects) {
			if (io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) io;
				if (algorithm.isRoot(lo)) {
					outSet.add((LinkedObject) io);
				}
			}
		}
	}

	/**
	 * Detects the roots in a given {@link LinkDatabase}, using a the given
	 * {@link RootAlgorithm}. The results are stored in the collection passed
	 * via the first parameter.
	 * 
	 * @param outSet
	 *            The collection where detected roots will be stored
	 * @param linkDatabase
	 *            The linkDatabase to check
	 * @param algorithm
	 *            the {@link RootAlgorithm} to use for root detection
	 */
	public static void detectRoots(Set<LinkedObject> outSet,
			LinkDatabase linkDatabase, RootAlgorithm rootAlgorithm) {
		detectRoots(outSet, linkDatabase, linkDatabase.getObjects(),
				rootAlgorithm);
	}

	/**
	 * Returns all ancestors of the given term.
	 */
	public static Collection<LinkedObject> getAncestors(LinkedObject term) {
		return getAncestors(term, false);
	}

	/**
	 * Returns all ancestors of the given term. If the includeSelf parameter is
	 * true, the given term itself is also included in the returned collection.
	 * 
	 * @param term
	 *            the term whose ancestors most be found
	 * @param includeSelf
	 *            whether or not the given term should be included in the output
	 *            collection
	 */
	public static Collection<LinkedObject> getAncestors(LinkedObject term,
			boolean includeSelf) {
		return getAncestors(term, null, includeSelf);
	}

	/**
	 * Returns all ancestors of the given term.
	 * 
	 * @param term
	 *            the term whose ancestors most be found
	 * @param linkDatabase
	 *            provides the parent links
	 * @param includeSelf
	 *            whether or not the given term itself should be included in the
	 *            output collection
	 */
	public static Collection<LinkedObject> getAncestors(LinkedObject term,
			LinkDatabase linkDatabase, boolean includeSelf) {
		AncestorTask task = getAncestors(term, linkDatabase);
		task.execute();
		Collection<LinkedObject> out = task.getResults();

		if (includeSelf)
			out.add(term);
		return out;
	}

	/**
	 * Returns an {@link AncestorThread} that can be used to calculate all the
	 * ancestors of a term. To get the results, the AncestorThread must be
	 * launched using the {@link Thread#start()} method.
	 * 
	 * @param term
	 *            the term whose ancestors should be searched
	 * @param linkDatabase
	 *            provides the parent links
	 * @return
	 */
	public static AncestorTask getAncestors(LinkedObject term,
			LinkDatabase linkDatabase) {
		return new AncestorTask(linkDatabase, term, null);
	}

	public static boolean isAncestor(LinkedObject term, LinkedObject ancestor,
			LinkDatabase linkDatabase) {
		return getAncestors(term, linkDatabase, true).contains(ancestor);
	}

	protected static AncestorTask getAncestors(LinkedObject term,
			LinkDatabase linkDatabase,
			Map<LinkedObject, Collection<LinkedObject>> memoizeTable) {
		return new AncestorTask(linkDatabase, term, memoizeTable);
	}

	/**
	 * Returns the number of children for the given.
	 * 
	 * @param linkDatabase
	 *            provides the child links
	 * @param lo
	 *            the term to search
	 * @return the number of children
	 */
	public static int getChildCount(LinkDatabase linkDatabase, LinkedObject lo) {
		return linkDatabase.getChildren(lo).size();
	}

	/**
	 * Returns the descendants of the given term.
	 */
	public static Collection<LinkedObject> getDescendants(LinkedObject term) {
		return getDescendants(term, false);
	}

	/**
	 * Returns the descendants of the given term.
	 * 
	 * @param term
	 *            the term to search
	 * @param includeSelf
	 *            whether to include the term itself in the results
	 */
	public static Collection<LinkedObject> getDescendants(LinkedObject term,
			boolean includeSelf) {
		return getDescendants(term, null, includeSelf);
	}

	/**
	 * Returns the descendants of the given term.
	 * 
	 * @param term
	 *            the term to search
	 * @param linkDatabase
	 *            provides the child links
	 * @param includeSelf
	 *            whether to include the term itself in the results
	 */
	public static Collection<LinkedObject> getDescendants(LinkedObject term,
			LinkDatabase linkDatabase, boolean includeSelf) {
		DescendantTask task = new DescendantTask(linkDatabase, term, null);
		task.execute();
		Collection<LinkedObject> out = task.getResults();
		if (includeSelf)
			out.add(term);
		return out;
	}

	protected static Object getFirst(Collection c) {
		Iterator it = c.iterator();
		if (it.hasNext())
			return it.next();
		else
			return null;
	}

	/**
	 * Returns the number of objects in a {@link LinkDatabase}
	 */
	public static int getObjectCount(LinkDatabase linkDatabase) {
		return linkDatabase.getObjects().size();
	}

	/**
	 * Returns the obsolete terms in the given {@link LinkDatabase}
	 */
	public static Collection<ObsoletableObject> getObsoletes(
			LinkDatabase session) {
		Collection<ObsoletableObject> out = new LinkedList<ObsoletableObject>();
		for (IdentifiedObject io : session.getObjects()) {
			if (isObsolete(io)) {
				out.add((ObsoletableObject) io);
			}
		}
		return out;
	}

	/**
	 * Returns the obsolete terms in the given {@link OBOSession}
	 */
	public static Collection<ObsoletableObject> getObsoletes(OBOSession session) {
		return getObsoletes(session.getLinkDatabase());
	}

	/**
	 * Returns the dangling terms in the given {@link LinkDatabase}
	 */
	public static Collection<DanglingObject> getDanglingObjects(
			LinkDatabase session) {
		Collection<DanglingObject> out = new LinkedList<DanglingObject>();
		for (IdentifiedObject io : session.getObjects()) {
			if (isDangling(io)) {
				out.add((DanglingObject) io);
			}
		}
		return out;
	}


	/**
	 * Returns the number of parents for a term in the given
	 * {@link LinkDatabase}
	 */
	public static int getParentCount(LinkDatabase linkDatabase, LinkedObject lo) {
		return linkDatabase.getParents(lo).size();
	}

	public static Collection<LinkedObject> getParentsByType(LinkedObject lo, OBOProperty type) {
		HashSet<LinkedObject> parents = new HashSet<LinkedObject>();
		for (Link link : lo.getParents()) {
			if (link.getType().equals(type)) {
				parents.add(link.getParent());
			}
		}
		return parents;
	}
	
	public static Collection<LinkedObject> getParents(LinkedObject lo) {
		HashSet<LinkedObject> parents = new HashSet<LinkedObject>();
		for (Link link : lo.getParents()) {
				parents.add(link.getParent());
		}
		return parents;
	}


	/**
	 * Returns a single value for a given property from a given instance. If the
	 * instance has no values for that property, null is returned. If the
	 * instance has multiple values for the property, any of the values may be
	 * returned.
	 */
	public static Object getPropValue(Instance instance, OBOProperty property) {
		Collection c = getPropValues(instance, property);
		return getFirst(c);
	}

	/**
	 * Returns a single value for a given property from a given instance. If the
	 * instance has no values for that property, null is returned.
	 * 
	 * If the failOnMultipleValues parameter is true, a runtime exception will
	 * be thrown if more than one value is defined.
	 * 
	 * @param <T>
	 *            the type to which the result should be cast
	 * @param instance
	 *            the instance to query
	 * @param property
	 *            the property to fetch
	 * @param expectedClass
	 *            the expected type of the fetched value
	 * @param failOnMultipleValues
	 *            whether to throw an exception if multiple values are found
	 * 
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static <T> T getPropValue(Instance instance, OBOProperty property,
			Class<T> expectedClass, boolean failOnMultipleValues) {
		Collection values = getPropValues(instance, property);
		if (values.size() == 0)
			return null;
		else if (failOnMultipleValues && values.size() > 1)
			throw new RuntimeException("Too many values for "
					+ property.getID() + " relationship for instance "
					+ instance.getID());
		Object value = getFirst(values);
		if (!expectedClass.isAssignableFrom(value.getClass()))
			throw new RuntimeException("Got object " + value + " of type "
					+ value.getClass() + " for property " + property.getID()
					+ " of instance " + instance.getID()
					+ ", but expected object of type " + expectedClass);
		return (T) value;
	}

	/**
	 * Returns the decoded values of a given property for a given instance.
	 * {@link DatatypeValue}s are converted into the equivalent Java objects.
	 * Non- datatype values retain their original type.
	 * 
	 * @param instance
	 * @param property
	 * @return
	 */
	public static Collection getPropValues(Instance instance,
			OBOProperty property) {
		Collection<Value<?>> values = instance.getValues(property);
		LinkedList<Object> list = new LinkedList<Object>();
		for (Value<?> v : values) {
			if (v.getType() instanceof Datatype && v instanceof DatatypeValue) {
				Object o = ((Datatype) v.getType())
				.getValue(((DatatypeValue) v).getValue());
				list.add(o);
			} else
				list.add(v);
		}
		return list;
	}



	/**
	 * Returns all non-obsolete {@link OBOProperty OBOProperties} in a
	 * {@link LinkDatabase}
	 */
	public static Collection<OBOProperty> getRelationshipTypes(
			LinkDatabase linkDatabase) {
//		logger.debug("TermUtil.getRelationshipTypes");
		Collection<OBOProperty> out = new LinkedList<OBOProperty>();
		for (IdentifiedObject io : linkDatabase.getObjects()) {
			if (isProperty(io) && !isObsolete(io) && !io.isBuiltIn()){
				out.add((OBOProperty) io);
			}
		}
		return out;
	}

	/**
	 * Returns all non-obsolete {@link OBOProperty OBOProperties} in an
	 * {@link OBOSession}
	 */
	public static Collection<OBOProperty> getRelationshipTypes(
			OBOSession session) {
		return getRelationshipTypes(session.getLinkDatabase());
	}

	/**
	 * Returns a single root ancestor for a {@link LinkedObject}. If the given
	 * {@link LinkedObject} has multiple roots, only the first one encountered
	 * is returned. This method uses the {@link RootAlgorithm#GREEDY GREEDY}
	 * root algorithm to detect roots.
	 */
	public static LinkedObject getRoot(LinkedObject obj) {
		return getRoot(obj, DefaultLinkDatabase.getDefault(),
				RootAlgorithm.GREEDY);
	}

	/**
	 * Returns a single root ancestor for a {@link LinkedObject}. If the given
	 * {@link LinkedObject} has multiple roots, only the first one encountered
	 * is returned.
	 * 
	 * @param obj
	 *            the object for which a root should be found
	 * @param linkDatabase
	 *            provides the parent links
	 * @param rootAlgorithm
	 *            determines which ancestors are roots
	 */
	public static LinkedObject getRoot(LinkedObject obj,
			LinkDatabase linkDatabase, RootAlgorithm rootAlgorithm) {
		rootAlgorithm.setLinkDatabase(linkDatabase);
		if (rootAlgorithm.isRoot(obj))
			return obj;
		Iterator it = linkDatabase.getParents(obj).iterator();
		while (it.hasNext()) {
			Object o = it.next();
			Link parent = (Link) o;
			LinkedObject root = getRoot(parent.getParent(), linkDatabase,
					rootAlgorithm);
			if (root != null)
				return root;
		}
		return null;
	}

	/**
	 * Returns all non-obsolete root terms in a given {@link LinkDatabase}.
	 * Equivalent to {@link #getRoots(LinkDatabase, true, false, false, false)}.
	 */
	@SuppressWarnings("unchecked")
	public static Collection<OBOClass> getRoots(LinkDatabase linkDatabase) {
		Collection c = getRoots(RootAlgorithm.GREEDY, linkDatabase, true,
				false, false, false);
		return (Collection<OBOClass>) c;
	}

	/**
	 * Returns all roots in a given {@link LinkDatabase}, using the
	 * {@link RootAlgorithm#GREEDY GREEDY} root algorithm.
	 * 
	 * @param linkDatabase
	 *            the linkDatabase to check
	 * @param includeTerms
	 *            whether to include root terms
	 * @param includeProperties
	 *            whether to include root properties
	 * @param includeObsoletes
	 *            whether to include obsolete terms & properties
	 * @param includeInstances
	 *            whether to include instances
	 */
	public static Collection<LinkedObject> getRoots(LinkDatabase linkDatabase,
			boolean includeTerms, boolean includeProperties,
			boolean includeObsoletes, boolean includeInstances) {
		return getRoots(RootAlgorithm.GREEDY, linkDatabase, includeTerms,
				includeProperties, includeObsoletes, includeInstances);
	}

	/**
	 * Returns all non-obsolete term roots in the given session.
	 */
	public static Collection<OBOClass> getRoots(OBOSession session) {
		return getRoots(session.getLinkDatabase());
	}

	/**
	 * Returns all non-obsolete term roots in the given {@link LinkDatabase}.
	 * 
	 * @param rootAlgorithm
	 *            determines which terms are roots
	 * @param linkDatabase
	 *            the linkDatabase to check
	 * @return
	 */
	public static Collection<LinkedObject> getRoots(
			RootAlgorithm rootAlgorithm, LinkDatabase linkDatabase) {
		return getRoots(rootAlgorithm, linkDatabase, true, false, false, false);
	}

	/**
	 * Returns all roots in a given {@link LinkDatabase}, using the given root
	 * algorithm.
	 * 
	 * @param rootAlgorithm
	 *            determines which terms are roots
	 * @param linkDatabase
	 *            the linkDatabase to check
	 * @param includeTerms
	 *            whether to include root terms
	 * @param includeProperties
	 *            whether to include root properties
	 * @param includeObsoletes
	 *            whether to include obsolete terms & properties
	 * @param includeInstances
	 *            whether to include instances
	 */
	public static Collection<LinkedObject> getRoots(
			RootAlgorithm rootAlgorithm, LinkDatabase linkDatabase,
			boolean includeTerms, boolean includeProperties,
			boolean includeObsoletes, boolean includeInstances) {
		rootAlgorithm.setLinkDatabase(linkDatabase);
		Collection<LinkedObject> out = new LinkedList<LinkedObject>();
		Iterator it = linkDatabase.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			if (!(io instanceof LinkedObject))
				continue;
			if (!includeProperties && TermUtil.isProperty(io))
				continue;
			if (!includeObsoletes && TermUtil.isObsolete(io))
				continue;
			if (!includeInstances && TermUtil.isInstance(io))
				continue;
			if (!includeTerms && TermUtil.isClass(io))
				continue;
			LinkedObject lo = (LinkedObject) io;
			if (rootAlgorithm.isRoot(lo))
				out.add(lo);
		}
		return out;
	}

	/**
	 * Returns all non-obsolete terms in the given {@link LinkDatabase}
	 */
	public static Collection<OBOClass> getTerms(LinkDatabase linkDatabase) {
		Collection<OBOClass> out = new LinkedList<OBOClass>();
		for (IdentifiedObject io : linkDatabase.getObjects()) {
			if (isClass(io) && !isObsolete(io)) {
				out.add((OBOClass) io);
			}
		}
		return out;
	}

	/**
	 * Returns all non-obsolete terms in the given {@link OBOSession}
	 */
	public static Collection<OBOClass> getTerms(OBOSession session) {
		return getTerms(session.getLinkDatabase());
	}

	public static Collection<Instance> getInstances(LinkDatabase linkDatabase) {
		Collection<Instance> out = new LinkedList<Instance>();
		for (IdentifiedObject io : linkDatabase.getObjects()) {
			if (isInstance(io)) {
				out.add((Instance) io);
			}
		}
		return out;
	}

	public static Collection<Instance> getInstances(OBOSession session) {
		return getInstances(session.getLinkDatabase());
	}

	/**
	 * Returns whether a {@link LinkedObject} has a given ancestor.
	 */
	public static boolean hasAncestor(LinkedObject child, LinkedObject ancestor) {
		boolean out = hasAncestor(new DefaultLinkDatabase(null), child,
				ancestor, new HashSet<LinkedObject>());
		return out;
	}

	protected static boolean hasAncestor(LinkDatabase linkDatabase,
			LinkedObject child, LinkedObject ancestor,
			Collection<LinkedObject> lookedAt) {
		if (lookedAt.contains(child))
			return false;
		else
			lookedAt.add(child);
		for (Link tr : linkDatabase.getParents(child)) {
			if (tr.getParent().equals(ancestor))
				return true;
			else if (hasAncestor(linkDatabase, tr.getParent(), ancestor,
					lookedAt))
				return true;
		}
		return false;
	}

	/**
	 * Returns whether a {@link LinkedObject} has a given ancestor.
	 */
	public static boolean hasIsAAncestor(LinkedObject child, LinkedObject ancestor) {
		boolean out = hasIsAAncestor(new DefaultLinkDatabase(null), child,
				ancestor, new HashSet<LinkedObject>());
		return out;
	}
	protected static boolean hasIsAAncestor(LinkDatabase linkDatabase,
			LinkedObject child, LinkedObject ancestor,
			Collection<LinkedObject> lookedAt) {
		if (lookedAt.contains(child))
			return false;
		else
			lookedAt.add(child);
		for (Link tr : linkDatabase.getParents(child)) {
			if (tr.getType().equals(OBOProperty.IS_A)) {
				if (tr.getParent().equals(ancestor))
					return true;
				else if (hasIsAAncestor(linkDatabase, tr.getParent(), ancestor,
						lookedAt))
					return true;
			}
		}
		return false;
	}

	public static Link getLink(LinkDatabase linkDatabase, Link link) {
		for(Link l : linkDatabase.getParents(link.getChild()))
			if (l.equals(link))
				return l;
		return null;
	}

	public static boolean containsLink(LinkDatabase linkDatabase, Link link) {
		Collection<Link> parents = linkDatabase.getParents(link.getChild());
		return parents != null && parents.contains(link);
	}

	/**
	 * Returns whether or not the given object is an OBO class
	 */
	public static boolean isClass(IdentifiedObject io) {
		if (io.getType() == null) {
			// instances can have dangling type
			return false;
		} else {
			return io.getType().equals(OBOClass.OBO_CLASS);
		}
	}

	/**
	 * If the object is an OBOClass, it casts it to an OBOClass and returns it.
	 * Otherwise, returns null.
	 */
	public static OBOClass getClass(IdentifiedObject io) {
		if (io instanceof OBOClass)
			return (OBOClass) io;
		else
			return null;
	}

	/**
	 * If the object is an Instance, it casts it to an Instance and returns it.
	 * Otherwise, returns null.
	 */
	public static Instance getInstance(IdentifiedObject io) {
		if (io instanceof Instance)
			return (Instance) io;
		else
			return null;
	}

	/**
	 * If the object is an OBOProperty, it casts it to an OBOProperty and
	 * returns it. Otherwise, returns null.
	 */
	public static OBOProperty getProperty(IdentifiedObject io) {
		if (io instanceof OBOProperty)
			return (OBOProperty) io;
		else
			return null;
	}

	/**
	 * Checks whether the given {@link LinkedObject} lo has a particular
	 * relationship to itself. This is checked using the provided
	 * {@link LinkDatabase}. If the provided {@link LinkDatabase} is a
	 * {@link ReasonedLinkDatabase}, this method can use the reasoner data to
	 * run very quickly, without recursion. If the provided {@link LinkDatabase}
	 * is not a {@link ReasonedLinkDatabase}, this method runs slowly,
	 * requiring many recursive steps, and will not find implied cycles.
	 */
	public static boolean isCycle(LinkDatabase linkDatabase,
			OBOProperty property, LinkedObject lo) {
		if (linkDatabase instanceof ReasonedLinkDatabase) {
			Collection<OBOProperty> properties = new LinkedList<OBOProperty>();
			if (property != null)
				properties.add(property);
			else {
				Iterator it = linkDatabase.getObjects().iterator();
				while (it.hasNext()) {
					Object o = it.next();
					if (o instanceof OBOProperty)
						properties.add((OBOProperty) o);
				}
			}
			Iterator it = properties.iterator();
			while (it.hasNext()) {
				property = (OBOProperty) it.next();
				if (((ReasonedLinkDatabase) linkDatabase).hasRelationship(lo,
						property, lo) != null)
					return true;
			}
			return false;
		} else
			return isCycle(linkDatabase, property, lo, lo, true,
					new HashSet<LinkedObject>());
	}

	protected static boolean isCycle(LinkDatabase linkDatabase,
			OBOProperty property, LinkedObject findme, LinkedObject current,
			boolean skipIt, Set<LinkedObject> seenem) {
		if (!skipIt && findme.equals(current)) {
			return true;
		} else {
			if (seenem.contains(current)) {
				return false;
			} else
				seenem.add(current);
			Iterator it = linkDatabase.getParents(current).iterator();
			while (it.hasNext()) {
				Link link = (Link) it.next();
				if (property == null
						|| ReasonerUtil.isSubclass(linkDatabase,
								link.getType(), property)) {
					boolean isCycle = isCycle(linkDatabase, property, findme,
							link.getParent(), false, seenem);

					if (isCycle)
						return true;
				}
			}

			return false;
		}
	}

	/**
	 * Returns whether the given object is a dangling object
	 */
	public static boolean isDangling(IdentifiedObject io) {
		return io instanceof DanglingObject;
	}

	// TODO - Make this much more efficient by NOT calling a DescendantThread
	/**
	 * Returns whether a {@link LinkedObject} has a given descendant
	 * 
	 * @param parent
	 * @param desc
	 * @return
	 */
	public static boolean isDescendant(LinkedObject parent, LinkedObject desc) {
		return getDescendants(parent).contains(desc);
	}

	/**
	 * Returns whether the given object was generated by a reasoner. (Currently,
	 * no OBO api reasoner will ever generate new objects, but an external
	 * reasoner may).
	 */
	public static boolean isImplied(IdentifiedObject io) {
		if (io instanceof Impliable)
			return ((Impliable) io).isImplied();
		else
			return false;
	}

	/**
	 * Returns whether the given link was generated by a reasoner.
	 */
	public static boolean isImplied(PathCapable link) {
		if (link instanceof Impliable)
			return ((Impliable) link).isImplied();
		else
			return false;
	}

	/**
	 * Returns whether the given object is an instance
	 */
	public static boolean isInstance(IdentifiedObject io) {
		return io instanceof Instance;
	}

	/**
	 * Returns whether the given link is an intersection link. That is, whether
	 * this link provides part of a genus or differentia definition.
	 */
	public static boolean isIntersection(Link link) {
		if (link instanceof OBORestriction)
			return ((OBORestriction) link).completes();
		else
			return false;
	}

	/**
	 * Returns whether the given object has an intersection (genus/differentia)
	 * definition.
	 */
	public static boolean isIntersection(LinkedObject lo) {
		Iterator it = lo.getParents().iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (isIntersection(link))
				return true;
		}
		return false;
	}

	/**
	 * Returns whether the given link is a union link. That is, whether
	 * this link provides part of a union definition.
	 */
	public static boolean isUnion(Link link) {
		if (link instanceof OBORestriction)
			return link.getType().equals(OBOProperty.UNION_OF);
		else
			return false;
	}	/**
	 * Returns whether the given object has union definition
	 */
	public static boolean isUnion(LinkedObject lo) {
		Iterator it = lo.getParents().iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (isUnion(link))
				return true;
		}
		return false;
	}

	/**
	 * Highly incomplete check to see if a relationship is legal. Currently,
	 * this method only checks to be sure that {@link OBOProperty#INVERSE_OF}
	 * only links properties, and that {@link OBOProperty#DISJOINT_FROM} only
	 * links terms.
	 */
	public static boolean isLegalRelationship(LinkedObject child,
			OBOProperty type, LinkedObject parent) {
		if (ReasonerUtil.isSubclass(type, OBOProperty.INVERSE_OF)
				&& !(TermUtil.isProperty(child) && TermUtil.isProperty(parent)))
			return false;
		if (ReasonerUtil.isSubclass(type, OBOProperty.DISJOINT_FROM)
				&& (TermUtil.isProperty(child) || TermUtil.isProperty(parent)))
			return false;
		return true;
	}

	/**
	 * Returns whether the given object is obsolete.
	 */
	public static boolean isObsolete(IdentifiableObject o) {
		return o instanceof ObsoletableObject
		&& ((ObsoletableObject) o).isObsolete();
	}

	/**
	 * Returns whether the given object is a relationship type.
	 */
	public static boolean isProperty(IdentifiedObject io) {
		if (io.getType() == null) {
			return false;
		} else {
			return io.getType().equals(OBOClass.OBO_PROPERTY);
		}
	}

	/**
	 * Returns whether the given {@link OBOProperty} is used in any link in the
	 * given {@link OBOSession}
	 */
	public static boolean isUsed(OBOSession session, OBOProperty prop) {
		Iterator it = session.getObjects().iterator();
		while (it.hasNext()) {
			IdentifiedObject io = (IdentifiedObject) it.next();
			if (!io.isBuiltIn() && io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) io;
				Iterator it2 = lo.getParents().iterator();
				while (it2.hasNext()) {
					Link link = (Link) it2.next();
					if (!link.getParent().isBuiltIn()
							&& link.getType().equals(prop)) {
						return true;
					}
				}
			}
		}
		return false;
	}

	/**
	 * Resolves dangling links. If the given link contains dangling objects,
	 * return a new link where the dangling objects have been replaced by real
	 * objects (if possible).
	 */
	public static Link resolve(OBOSession session, Link link) {
		if (link.getParent() instanceof DanglingObject
				|| link.getChild() instanceof DanglingObject
				|| link instanceof DanglingLinkImpl) {
			LinkedObject parent = resolve(session, link.getParent());
			LinkedObject child = resolve(session, link.getChild());
			Link out;
			if (link instanceof DanglingLinkImpl && !isDangling(parent)
					&& !isDangling(child)) {
				out = new OBORestrictionImpl();
				out.setType(link.getType());
			} else
				out = (Link) link.clone();
			out.setParent(parent);
			out.setChild(child);
			return out;
		} else
			return link;

	}

	/**
	 * Resolves a dangling object into a real object, if possible. If the
	 * dangling object cannot be resolved, the original object is returned.
	 */
	public static LinkedObject resolve(OBOSession session, LinkedObject pc) {
		if (pc instanceof DanglingObject) {
			IdentifiedObject newObject = session.getObject(pc.getID());
			if (newObject == null || !(newObject instanceof LinkedObject))
				return pc;
			else
				return (LinkedObject) newObject;
		} else
			return pc;
	}

	/**
	 * Resolves a dangling object or link into a real object or link, if
	 * possible. If the dangling object cannot be resolved, the original object
	 * is returned.
	 */
	public static PathCapable resolve(OBOSession session, PathCapable pc) {
		if (pc instanceof Link)
			return resolve(session, (Link) pc);
		else if (pc instanceof LinkedObject)
			return resolve(session, (LinkedObject) pc);
		else
			return null;
	}

	/**
	 * Returns whether the given object has any parent links with the given
	 * relationship type.
	 */
	public static boolean usesType(LinkedObject lo, OBOProperty prop) {
		Iterator it = lo.getParents().iterator();
		while (it.hasNext()) {
			Link l = (Link) it.next();
			if (l.getType().equals(prop))
				return true;
		}
		return false;
	}

	/**
	 * maps enum int to string
	 * (lifted from OBO_1_2_Serializer by cjm)
	 * TODO: DRY
	 * @param scope
	 * @return
	 */
	public static String getScopeLabel(int scope) {
		if (scope == Synonym.UNKNOWN_SCOPE || scope == Synonym.RELATED_SYNONYM)
			return "RELATED";
		else if (scope == Synonym.EXACT_SYNONYM)
			return "EXACT";
		else if (scope == Synonym.BROAD_SYNONYM)
			return "BROAD";
		else if (scope == Synonym.NARROW_SYNONYM)
			return "NARROW";
		else
			return null;
	}

	public static int getScopeEnum(String label) {
		if (label == null)
			return Synonym.RELATED_SYNONYM;
		String code = (String) label.toLowerCase().subSequence(0, 1);
		if (code.equals("e"))
			return Synonym.EXACT_SYNONYM;
		else if (code.equals("b"))
			return Synonym.BROAD_SYNONYM;
		else if (code.equals("n"))
			return Synonym.NARROW_SYNONYM;
		else
			return Synonym.RELATED_SYNONYM;
	}

	public static OBOClass castToClass(LinkedObject lo) {
		if (lo instanceof OBOClass) {
			return (OBOClass) lo;
		} else if (isDangling(lo)) {
			return new DanglingClassImpl(lo.getID());
		} else
			return null;
	}

	public static Instance castToInstance(LinkedObject lo) {
		if (lo instanceof Instance) {
			return (Instance) lo;
		} else if (isDangling(lo)) {
			return new DanglingInstanceImpl(lo.getID(),null); 
		} else
			return null;
	}

	public static OBOProperty castToProperty(IdentifiedObject lo) {
		if (lo instanceof OBOProperty) {
			return (OBOProperty) lo;
		} else if (isDangling(lo)) {
			return new DanglingPropertyImpl(lo.getID());
		} else
			return null;
	}

	public static Link castParentToClass(Link link) {
		if (link.getParent() instanceof OBOClass) {
			return link;
		} else if (isDangling(link.getParent())) {
			Link out = new OBORestrictionImpl(link);
			out.setParent(new DanglingClassImpl(link.getParent().getID()));
			return out;
		} else
			return null;
	}

	public static boolean isDangling(Link link) {
		return isDangling(link.getParent()) || isDangling(link.getType());
	}

	public static boolean hasDanglingIntersections(LinkedObject lo) {
		for (Link link : lo.getParents()) {
			if (isIntersection(link) && isDangling(link)) {
				return true;
			}
		}
		return false;
	}

	public static boolean resolveDanglingLink(OBOSession session, Link link) {
		if (isDangling(link)) {
			boolean resolved = true;
			LinkedObject p = link.getParent();
			if (isDangling(p)) {
				// the non-dangling object may have been created
				// after the link; in this case reset the link
				p = (LinkedObject)session.getObject(p.getID());
				if (p == null)
					resolved = false; // leave as dangling
				else {
					// reset link
					link.setParent(p);
					p.addChild(link); // do reciprocal link 
				}
			}
			OBOProperty t = link.getType();
			if (isDangling(t)) {
				t = (OBOProperty)session.getObject(t.getID());
				if (t == null)
					resolved = false;
				else
					link.setType(t);
			}
			return resolved;
		}
		return true;
	}

	public static boolean resolveDanglingLinks(OBOSession session) {
		boolean allResolved = true;
		for (IdentifiedObject io : session.getObjects()) {
			if (io instanceof LinkedObject) {
				for (Link link : ((LinkedObject)io).getParents()) {
					boolean resolved = resolveDanglingLink(session, link);
					if (!resolved)
						allResolved = true;
				}
			}
		}
		return allResolved;
	}


	public static Iterator<Link> getAllLinks(final LinkDatabase linkDatabase) {
		IteratorFactory<LinkDatabase, IdentifiedObject> objIteratorFactory = new IteratorFactory<LinkDatabase, IdentifiedObject>() {

			public Iterator<IdentifiedObject> getIterator(LinkDatabase object) {
				return object.getObjects().iterator();
			}

		};
		IteratorFactory<IdentifiedObject, Link> linkIteratorFactory = new IteratorFactory<IdentifiedObject, Link>() {

			public Iterator<Link> getIterator(IdentifiedObject object) {
				if (object instanceof LinkedObject) {
					return linkDatabase.getParents((LinkedObject) object)
					.iterator();
				} else
					return EmptyIterator.emptyIterator();
			}

		};
		return new SuperIterator<LinkDatabase, Link>(linkDatabase,
				objIteratorFactory, linkIteratorFactory);
	}

	/**
	 * Returns whether two links should be considered equal. This method takes
	 * the links' intersection property into consideration.
	 */
	public static boolean equals(Link a, Link b) {
		return TermUtil.equalsWithoutIntersection(a, b)
		&& (isIntersection(a) == isIntersection(b));
	}

	/**
	 * Returns whether two links should be considered equal. This method ignores
	 * the links' intersection property.
	 */
	public static boolean equalsWithoutIntersection(Link a, Link b) {
		return ObjectUtil.equals(a.getChild(), b.getChild())
		&& ObjectUtil.equals(a.getType(), b.getType())
		&& ObjectUtil.equals(a.getParent(), b.getParent());
	}

	public static TermMacroHistoryItem createGenusDifferentiaHistoryItem(LinkedObject lo, LinkedObject genus, String relID, LinkedObject diffClass) {
		TermMacroHistoryItem item = new TermMacroHistoryItem("Created new xp term");
		String id = lo.getID();
		item.addItem(new CreateLinkHistoryItem(id, relID, diffClass.getID()));
		item.addItem(new CreateLinkHistoryItem(id, "OBO_REL:is_a", genus.getID()));
		item.addItem(new CompletesHistoryItem(id, relID, diffClass.getID(), false));
		item.addItem(new CompletesHistoryItem(id, "OBO_REL:is_a", genus.getID(), false));
		// we have to recreate existing links that were transformed:
		// where we have two links, one is defining, we still want to keep the other
		for (Link link : lo.getParents()) {
			if (link.getType().equals(OBOProperty.IS_A) &&
					link.getParent().equals(genus)) {
				item.addItem(new CreateLinkHistoryItem(id, "OBO_REL:is_a", genus.getID()));

			}
			if (link.getType().getID().equals(relID) &&
					link.getParent().equals(diffClass)) {
				item.addItem(new CreateLinkHistoryItem(id, relID, diffClass.getID()));

			}
		}
		return item;
	}

	public static Collection<String> getLabels(IdentifiedObject lo) {
		LinkedList<String> labels = new LinkedList<String>();
		if (lo.getName() != null)
			labels.add(lo.getName());
		if (lo instanceof SynonymedObject) {
			for (Synonym syn : ((SynonymedObject)lo).getSynonyms())
				labels.add(syn.toString());
		}
		return labels;
	}

	public static Collection<String> getExactLabels(IdentifiedObject lo) {
		LinkedList<String> labels = new LinkedList<String>();
		if (lo.getName() != null)
			labels.add(lo.getName());
		if (lo instanceof SynonymedObject) {
			for (Synonym syn : ((SynonymedObject)lo).getSynonyms())
				if (syn.getScope() == Synonym.EXACT_SYNONYM)
					labels.add(syn.toString());
		}
		return labels;
	}




	/**
	 * this utility method is used to get all the OBOObjects references in an OBOSession.
	 * Note that OBOSession currently tracks all IdentifiedObjects, which is a superclass of
	 * OBOOBject. Eventually IdentifiedObject, LinkedObject will be deprecated and
	 * everything will be an OBOObject
	 * @param session
	 * @return all OBOObjects in session
	 */
	public static Collection<OBOObject> getOBOObjects(OBOSession session) {
		Collection<OBOObject> objs  = new HashSet<OBOObject>();
		for (IdentifiedObject io : session.getObjects()) {
			if (io.isBuiltIn())
				continue;
			if (io instanceof OBOObject)
				objs.add((OBOObject)io);
		}
		return objs;
	}

	/**
	 * @param session
	 * @return all properties (aka relations) defined in this session
	 */
	public static Collection<OBOProperty> getProperties(OBOSession session) {
		Collection<OBOProperty> props  = new HashSet<OBOProperty>();
		if (session.getObjects() == null) {
			return null;
		}
		for (IdentifiedObject io : session.getObjects()) {
			if (io.isBuiltIn())
				continue;
			if (io instanceof OBOProperty)
				props.add((OBOProperty)io);
		}
		return props;
	}

	public static Collection<OBOObject> getSubclasses(OBOObject obj) {
		Collection<OBOObject> subclasses = new HashSet<OBOObject>();
		for (Link link : obj.getChildren())
			if (link.getType().equals(OBOProperty.IS_A))
				subclasses.add((OBOObject) link.getChild());
		return subclasses;

	}

	public static TermMacroHistoryItem makeAllSubclassesMutuallyDisjointHistoryItem(OBOObject obj) {
		Collection<OBOObject> subclasses = getSubclasses(obj);
		TermMacroHistoryItem item = new TermMacroHistoryItem("Created new disjoint set");
		for (OBOObject c1 : subclasses) {
			for (OBOObject c2 : subclasses) {
				if (c1.equals(c2))
					continue;
				item.addItem(new CreateLinkHistoryItem(c1, 
						OBOProperty.DISJOINT_FROM, c2));

			}
		}
		return item;
	}

	public static String getNameSafe(LinkedObject lo) {
		String name = lo.getName();
		if (name != null)
			return name;
		return lo.getID();
	}

	public static Collection<OBOProperty> getSuperProperties(OBOProperty prop) {
		Collection<OBOProperty> props = new HashSet<OBOProperty> ();
		for (Link link : prop.getParents()) {
			if (link.getType().equals(OBOProperty.IS_A)) {
				props.add((OBOProperty) link.getParent());
			}
		}
		return props;
	}

}
