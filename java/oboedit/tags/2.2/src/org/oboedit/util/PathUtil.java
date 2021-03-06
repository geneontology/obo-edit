package org.oboedit.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Vector;

import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.PathCapable;
import org.obo.datamodel.RootAlgorithm;
import org.obo.datamodel.impl.DefaultLinkDatabase;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.history.StringRelationship;
import org.obo.util.HistoryUtil;
import org.obo.util.TermUtil;
import org.oboedit.gui.ObjectSelector;

import org.apache.log4j.*;

public class PathUtil {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(PathUtil.class);

	public static TreePath[] getPaths(Collection<PathCapable> items,
			ObjectSelector displayer, boolean shortestPathOnly) {
		if (displayer == null) {
			return PathUtil.getPaths(items, RootAlgorithm.GREEDY,
					DefaultLinkDatabase.getDefault(), shortestPathOnly);
		} else
			return PathUtil.getPaths(items, displayer.getRootAlgorithm(),
					displayer.getLinkDatabase(), shortestPathOnly);
	}

	public static TreePath[] getPaths(Collection<? extends PathCapable> items,
			RootAlgorithm rootAlgorithm, LinkDatabase linkDatabase,
			boolean bestPathOnly) {
		if (items == null)
			return new TreePath[0];

		Set<TreePath> pathVector = new HashSet<TreePath>();
		Iterator<? extends PathCapable> it = items.iterator();
		while (it.hasNext()) {
			PathCapable lo = it.next();
			if (bestPathOnly) {
				TreePath path = PathUtil.getBestPath(lo, rootAlgorithm,
						linkDatabase);
				pathVector.add(path);
			} else {
				pathVector.addAll(PathUtil.getPathsAsVector(lo, rootAlgorithm,
						linkDatabase));
			}
		}

		TreePath[] selectPaths = new TreePath[pathVector.size()];
		Iterator<TreePath> itp = pathVector.iterator();
		for (int i = 0; itp.hasNext(); i++) {
			selectPaths[i] = itp.next();
		}

		return selectPaths;
	}

	public static boolean pathIsValid(TreePath current, TreeModel model) {
		Object[] objects = current.getPath();

		// only expand paths that are actually in the model
		// otherwise some JTree implementations will become corrupted
		Object previous = objects[0];

		for (int j = 1; j < objects.length; j++) {
			int count = model.getChildCount(previous);
			boolean found = false;
			for (int i = 0; i < count; i++) {
				Object child = model.getChild(previous, i);
				
				if (child.equals(objects[j])) {
					found = true;
					break;
				}
			}
			if (!found)
				return false;
			previous = objects[j];
		}
		return true;
	}

	public static TreePath reconstruct(TreePath current, TreeModel model) {
		Object[] objects = current.getPath();
		Object[] newObjects = new Object[objects.length];
		Object previous = objects[0];
		newObjects[0] = objects[0];
		for (int j = 1; j < objects.length; j++) {
			int count = model.getChildCount(previous);
			boolean found = false;
			for (int i = 0; i < count; i++) {
				Object child = model.getChild(previous, i);
				if (child.equals(objects[j])) {
					found = true;
					newObjects[j] = child;
					break;
				}
			}
			if (!found)
				return null;
			previous = objects[j];
		}
		return new TreePath(newObjects);
	}

	public static TreePath[] getPaths(IdentifiedObject io) {
		return PathUtil.getPaths(io, RootAlgorithm.GREEDY, DefaultLinkDatabase
				.getDefault());
	}

	public static TreePath[] getPaths(LinkedObject term) {
		return PathUtil.getPaths(term, RootAlgorithm.GREEDY,
				DefaultLinkDatabase.getDefault());
	}

	public static TreePath[] getPaths(Link link) {
		return PathUtil.getPaths(link, RootAlgorithm.GREEDY,
				DefaultLinkDatabase.getDefault());
	}

	public static List<TreePath> getPathsAsVector(Link link) {
		return PathUtil.getPathsAsVector(link, RootAlgorithm.GREEDY,
				DefaultLinkDatabase.getDefault());
	}

	public static List<TreePath> getPathsAsVector(LinkedObject term) {
		return PathUtil.getPathsAsVector(term, RootAlgorithm.GREEDY,
				DefaultLinkDatabase.getDefault());
	}

	public static TreePath[] getPaths(IdentifiedObject io,
			RootAlgorithm rootAlgorithm, LinkDatabase linkDatabase) {
		if (io instanceof LinkedObject)
			return PathUtil.getPaths((LinkedObject) io, rootAlgorithm,
					linkDatabase);
		else
			return new TreePath[0];
	}

	public static TreePath[] getPaths(LinkedObject term,
			RootAlgorithm rootAlgorithm, LinkDatabase linkDatabase) {
		List<TreePath> v = PathUtil.getPathsAsVector(term, rootAlgorithm, linkDatabase);
		Iterator<TreePath> it = v.iterator();
		TreePath[] out = new TreePath[v.size()];
		for (int i = 0; it.hasNext(); i++) {
			out[i] = it.next();
		}
		return out;
	}

	public static TreePath[] getPaths(Link link, RootAlgorithm rootAlgorithm,
			LinkDatabase linkDatabase) {
		List<TreePath> paths = PathUtil.getPathsAsVector(link, rootAlgorithm,
				linkDatabase);
		TreePath[] out = new TreePath[paths.size()];
		for (int i = 0; i < paths.size(); i++) {
			out[i] = paths.get(i);
		}
		return out;
	}

	public static List<TreePath> getPathsAsVector(Link link, RootAlgorithm rootAlgorithm,
			LinkDatabase linkDatabase, int nBest) {
		Map<Link, Vector<TreePath>> lookedAt = new HashMap<Link, Vector<TreePath>>();
		Vector<TreePath> out = PathUtil.getPathsAsVector(link, rootAlgorithm,
				linkDatabase, lookedAt, nBest);
		return out;
	}

	public static int getPathRating(TreePath path) {
		int rating = 0;
		Object[] objs = path.getPath();
		for (int i = 0; i < objs.length; i++) {
			if (objs[i] instanceof Link) {
				Link link = (Link) objs[i];
				rating += PathUtil.getLinkRating(link);
			}
		}
		return rating;
	}

	public static int getLinkRating(Link link) {
		int val;
		if (link.getType() == null)
			val = 1;
		else {
			if (link.getType().equals(OBOProperty.IS_A)) {
				if (TermUtil.isImplied(link))
					val = 2;
				else
					val = 1;
			} else if (link.getType().isTransitive())
				if (TermUtil.isImplied(link))
					val = 4;
				else
					val = 3;
			else
				val = 10;
		}
		if (TermUtil.isIntersection(link)) {
			val *= 1000;
		}
		return val;
	}

	public static TreePath[] getBestPaths(Collection<PathCapable> c,
			RootAlgorithm rootAlgorithm, LinkDatabase linkDatabase) {
		LinkedList<TreePath> out = new LinkedList<TreePath>();
		Iterator<PathCapable> it = c.iterator();
		while (it.hasNext()) {
			PathCapable pc = it.next();
			TreePath best = PathUtil.getBestPath(pc, rootAlgorithm,
					linkDatabase);
			if (best != null)
				out.add(best);
		}
		TreePath[] paths = new TreePath[out.size()];
		Iterator<TreePath> itp = out.iterator();
		for (int i = 0; itp.hasNext(); i++) {
			paths[i] = itp.next();
		}
		return paths;
	}

	public static TreePath getBestPath(PathCapable lo,
			RootAlgorithm rootAlgorithm, LinkDatabase linkDatabase) {
		java.util.List<TreePath> v = PathUtil.getPathsAsVector(lo, rootAlgorithm,
				linkDatabase, 2);
		TreePath bestPath = null;
		int minRating = Integer.MAX_VALUE;
		Iterator<TreePath> it = v.iterator();
		while (it.hasNext()) {
			TreePath path = it.next();
			int rating = getPathRating(path);
			if (rating < minRating) {
				bestPath = path;
				minRating = rating;
			}
		}
		return bestPath;
	}

	public static TreePath getShortestPath(LinkedObject lo,
			RootAlgorithm rootAlgorithm, LinkDatabase linkDatabase) {
		if (TermUtil.isDangling(lo))
			return null;
		rootAlgorithm.setLinkDatabase(linkDatabase);

		if (rootAlgorithm.isRoot(lo)) {
			Object[] os = new Object[3];
			os[0] = PathUtil.ROOT;
			if (TermUtil.isObsolete(lo))
				os[1] = PathUtil.OBSOLETE;
			else if (TermUtil.isProperty(lo))
				os[1] = PathUtil.TYPES;
			else if (TermUtil.isClass(lo))
				os[1] = PathUtil.CLASSES;
			else if (TermUtil.isInstance(lo))
				os[1] = PathUtil.INSTANCES;
			else
				throw new RuntimeException(
						"Could not determine type of object " + lo
						+ ", class = " + lo.getClass());
			os[2] = new OBORestrictionImpl(lo);
			TreePath path = new TreePath(os);
			return path;
		} else if (lo.getParents().size() == 0 && !rootAlgorithm.isRoot(lo)) {
			return null;
		} else {
			Map<Link, TreePath> lookedAt = new HashMap<Link, TreePath>();
			Iterator<Link> it = linkDatabase.getParents(lo).iterator();
			TreePath shortest = null;
			while (it.hasNext()) {
				Link parent = it.next();
				TreePath path = PathUtil.getShortestPath(parent, rootAlgorithm,
						linkDatabase, lookedAt, null, 0);
				if (path == null)
					continue;
				if (shortest == null)
					shortest = path;
				else {
					if (path.getPathCount() < shortest.getPathCount())
						shortest = path;
					// can't get shorter than this!
					if (shortest.getPathCount() <= 3)
						break;
				}
			}
			return shortest;
		}
	}

	public static List<TreePath> getPathsAsVector(PathCapable pc,
			RootAlgorithm rootAlgorithm, LinkDatabase linkDatabase) {
		return PathUtil.getPathsAsVector(pc, rootAlgorithm, linkDatabase, -1);
	}

	public static List<TreePath> getPathsAsVector(PathCapable pc,
			RootAlgorithm rootAlgorithm, LinkDatabase linkDatabase, int nBest) {
		if (pc instanceof Link)
			return getPathsAsVector((Link) pc, rootAlgorithm, linkDatabase,
					nBest);
		else if (pc instanceof LinkedObject)
			return PathUtil.getPathsAsVector((LinkedObject) pc, rootAlgorithm,
					linkDatabase, nBest);
		else
			throw new IllegalArgumentException();
	}

	public static TreePath getShortestPath(PathCapable pc,
			RootAlgorithm rootAlgorithm, LinkDatabase linkDatabase) {
		if (pc instanceof Link)
			return PathUtil.getShortestPath((Link) pc, rootAlgorithm,
					linkDatabase);
		else if (pc instanceof LinkedObject)
			return getShortestPath((LinkedObject) pc, rootAlgorithm,
					linkDatabase);
		else
			throw new IllegalArgumentException("Can't get shortest path to "
					+ pc);
	}

	public static TreePath getShortestPath(Link link,
			RootAlgorithm rootAlgorithm, LinkDatabase linkDatabase) {
		TreePath out = PathUtil.getShortestPath(link, rootAlgorithm,
				linkDatabase, new HashMap<Link, TreePath>(), null, 0);
		return out;
	}

	public static TreePath getShortestPath(Link link,
			RootAlgorithm rootAlgorithm, LinkDatabase linkDatabase,
			Map<Link, TreePath> lookedAt, TreePath best, int currentDepth) {
		if (lookedAt.containsKey(link))
			return lookedAt.get(link);

		rootAlgorithm.setLinkDatabase(linkDatabase);

		if (link.getParent() == null) {
			Object[] os = new Object[3];
			os[0] = PathUtil.ROOT;
			if (TermUtil.isObsolete(link.getChild()))
				os[1] = PathUtil.OBSOLETE;
			else if (TermUtil.isProperty(link.getChild()))
				os[1] = PathUtil.TYPES;
			else if (TermUtil.isClass(link.getChild()))
				os[1] = PathUtil.CLASSES;
			else if (TermUtil.isInstance(link.getChild()))
				os[1] = PathUtil.INSTANCES;
			else
				throw new RuntimeException(
						"Could not determine type of link child "
						+ link.getChild() + ", class = "
						+ link.getChild().getClass());
			os[2] = link;
			TreePath out = new TreePath(os);
			lookedAt.put(link, out);
			return out;
		}

		if (rootAlgorithm.isRoot(link.getParent())) {
			TreePath shortest = getShortestPath(link.getParent(),
					rootAlgorithm, linkDatabase);
			if (shortest == null)
				return null;
			TreePath path = shortest.pathByAddingChild(link);
			lookedAt.put(link, path);
			return path;
		}

		if (best != null && currentDepth + 4 >= best.getPathCount()) {
			lookedAt.put(link, best);
			return best;
		}

		// I can't believe we'd need to do this AGAIN.
//		rootAlgorithm.setLinkDatabase(linkDatabase);
		TreePath shortestPath = null;

		Iterator<Link> it = linkDatabase.getParents(link.getParent()).iterator();
		while (it.hasNext()) {
			Link parentRel = it.next();
			TreePath parentPath = getShortestPath(parentRel, rootAlgorithm,
					linkDatabase, lookedAt, shortestPath, currentDepth + 1);
			if (parentPath == null)
				continue;

			if (shortestPath != null
					&& parentPath.getPathCount() + 1 >= shortestPath
					.getPathCount())
				continue;

			// else, this is the new shortest path
			shortestPath = parentPath.pathByAddingChild(link);
			// no path can ever be shorter than 4
			if (shortestPath.getPathCount() <= 4)
				break;
		}
		lookedAt.put(link, shortestPath);
		return shortestPath;
	}

	public static Comparator<TreePath> pathComparator = new Comparator<TreePath>() {
		public int compare(TreePath a, TreePath b) {
			int paVal = getPathRating(a);
			int pbVal = getPathRating(b);
			return pbVal - paVal;
		}
	};

	public static Comparator<Link> linkRatingComparator = new Comparator<Link>() {

		public int compare(Link l1, Link l2) {
			return getLinkRating(l1) - getLinkRating(l2);
		}

	};

	public static Vector<TreePath> getPathsAsVector(Link link,
			RootAlgorithm rootAlgorithm, LinkDatabase linkDatabase,
			Map<Link, Vector<TreePath>> lookedAt, int nBest) {
		Vector<TreePath> out = new Vector<TreePath>();

		// make the change here!
		// Make WHAT change here??? --NH
		if (lookedAt.containsKey(link)) {
			out = lookedAt.get(link);
			return (Vector<TreePath>) out.clone();
		}

		lookedAt.put(link, out);

		if (link.getParent() == null) {
			Object[] os = new Object[3];
			os[0] = PathUtil.ROOT;
			if (TermUtil.isObsolete(link.getChild()))
				os[1] = PathUtil.OBSOLETE;
			else if (TermUtil.isProperty(link.getChild())){
				os[1] = PathUtil.TYPES;
			}
			else if (TermUtil.isClass(link.getChild()))
				os[1] = PathUtil.CLASSES;
			else
				os[1] = PathUtil.INSTANCES;
			os[2] = link;
			out.add(new TreePath(os));
			return out;
		}

		rootAlgorithm.setLinkDatabase(linkDatabase);

		if (rootAlgorithm.isRoot(link.getParent())) {
			List<TreePath> v = getPathsAsVector(link.getParent(), rootAlgorithm,
					linkDatabase);
			Iterator<TreePath> it = v.iterator();
			while (it.hasNext()) {
				TreePath path = it.next().pathByAddingChild(link);
				out.add(path);
			}
			return out;
		}
		/*
		 * if (linkDatabase.getParents(link.getParent()).size() == 0) { Object []
		 * os = new Object[3]; os[0] = OBOSession.ROOT; if
		 * (TermUtil.isObsolete(link.getChild())) os[1] = OBOSession.OBSOLETE;
		 * else if (TermUtil.isProperty(link.getChild())) os[1] =
		 * OBOSession.TYPES; else if (TermUtil.isClass(link.getChild())) os[1] =
		 * OBOSession.CLASSES; else os[1] = OBOSession.INSTANCES; os[2] = new
		 * OBORestrictionImpl(link.getParent()); TreePath path = new
		 * TreePath(os); out.add(path); return out; }
		 */
		Collection<Link> parents;
		if (nBest < 0) {
			parents = linkDatabase.getParents(link.getParent());
		} else {
			parents = new ArrayList<Link>();
			parents.addAll(linkDatabase.getParents(link.getParent()));
			Collections.sort((List<Link>) parents, linkRatingComparator);
			int count = parents.size() - nBest;
			for (int i = 0; i < count; i++)
				((List<Link>) parents).remove(parents.size() - 1);
		}

		Iterator<Link> it = parents.iterator();
		while (it.hasNext()) {
			Link parentRel = it.next();
			Vector<TreePath> parentVector = getPathsAsVector(parentRel, rootAlgorithm,
					linkDatabase, lookedAt, nBest);
			for (int j = 0; j < parentVector.size(); j++) {
				TreePath path = parentVector.get(j);
				TreePath pathByAddingChild = path.pathByAddingChild(link);
				if (!out.contains(pathByAddingChild))
					out.add(pathByAddingChild);
			}
		}

		return out;
	}

	public static Vector<TreePath> getPathsAsVector(LinkedObject term,
			RootAlgorithm rootAlgorithm, LinkDatabase linkDatabase, int nBest) {
		Vector<TreePath> out = new Vector<TreePath>();
		rootAlgorithm.setLinkDatabase(linkDatabase);
		if (rootAlgorithm.isRoot(term)) {
			Object[] os = new Object[3];
			os[0] = PathUtil.ROOT;
			if (TermUtil.isObsolete(term))
				os[1] = PathUtil.OBSOLETE;
			else if (TermUtil.isProperty(term))
				os[1] = PathUtil.TYPES;
			else if (TermUtil.isClass(term))
				os[1] = PathUtil.CLASSES;
			else
				os[1] = PathUtil.INSTANCES;
			os[2] = new OBORestrictionImpl(term);
			out.add(new TreePath(os));
		} else {
			Collection<Link> parents;
			if (nBest < 0) {
				parents = linkDatabase.getParents(term);
			} else {
				parents = new ArrayList<Link>();
				parents.addAll(linkDatabase.getParents(term));
				Collections.sort((List<Link>) parents, linkRatingComparator);
				int count = parents.size() - nBest;
				for (int i = 0; i < count; i++)
					((List<Link>) parents).remove(parents.size() - 1);
			}

			Iterator<Link> it = parents.iterator();
			while (it.hasNext()) {
				Link tr = it.next();
				List<TreePath> paths = getPathsAsVector(tr, rootAlgorithm, linkDatabase,
						nBest);
				for (int j = 0; j < paths.size(); j++) {
					TreePath path = paths.get(j);
					if (!out.contains(path))
						out.add(path);
				}
			}
		}
		Collections.sort(out, pathComparator);
		return out;
	}

	public static TreePath convertPathToIDs(TreePath path) {
		Object[] pathobs = path.getPath();
		if (pathobs.length == 0)
			return path;
		Object[] obj = new Object[pathobs.length - 1];
		for (int i = 1; i < pathobs.length; i++) {
			if (pathobs[i] instanceof OBORestriction) {
				OBORestriction tr = (OBORestriction) pathobs[i];
				obj[i - 1] = new StringRelationship(tr);
			} else if (pathobs[i] instanceof OBOClass) {
				OBOClass term = (OBOClass) pathobs[i];
				obj[i - 1] = term.getID();
			} else
				obj[i - 1] = pathobs[i];
		}
		TreePath out = new TreePath(obj);
		return out;
	}

	public static TreePath convertPathToObjects(TreePath path,
			OBOSession history) {
		Object[] pathobs = path.getPath();
		if (pathobs.length == 0)
			return path;
		Object[] obj = new Object[pathobs.length + 1];
		obj[0] = PathUtil.ROOT;
		for (int i = 0; i < pathobs.length; i++) {
			if (pathobs[i] == null)
				return null;
			else if (pathobs[i] instanceof StringRelationship) {
				StringRelationship sr = (StringRelationship) pathobs[i];

				LinkedObject child = (LinkedObject) history.getObject(sr
						.getChild());
				LinkedObject parent = (LinkedObject) history.getObject(sr
						.getParent());
				OBOProperty type = (OBOProperty) history
				.getObject(sr.getType());

				if (child == null && sr.getChild() != null)
					return null;
				if (parent == null && sr.getParent() != null)
					return null;
				if (type == null && sr.getType() != null)
					return null;

				Link tr = new OBORestrictionImpl(child, parent, type);

				if (tr.getParent() != null)
					tr = HistoryUtil.findChildRel(tr, tr.getParent());
				if (tr == null)
					return null;
				obj[i + 1] = tr;
			} else
				obj[i + 1] = pathobs[i];
		}
		TreePath out = new TreePath(obj);
		return out;
	}

	public static TreePath[] convertPathsToIDs(TreePath[] path) {
		if (path == null)
			return null;
		TreePath[] out = new TreePath[path.length];
		for (int i = 0; i < out.length; i++)
			out[i] = convertPathToIDs(path[i]);
		return out;
	}

	public static TreePath[] convertPathsToObjects(TreePath[] paths,
			OBOSession history) {
		if (paths == null)
			return null;
		List<TreePath> temp = new LinkedList<TreePath>();
		for (int i = 0; i < paths.length; i++) {
			TreePath path = convertPathToObjects(paths[i], history);
			if (path != null)
				temp.add(path);
		}
		return temp.toArray(new TreePath[temp.size()]);
	}

	public static boolean pathContainsNonTransitive(TreePath path) {
		do {
			Object o = path.getLastPathComponent();
			path = path.getParentPath();
			if (o instanceof Link) {
				Link link = (Link) o;
				if (link.getType() == null)
					continue;
				if (!link.getType().isTransitive()) {
					return true;
				}
			}
		} while (path != null && path.getPathCount() > 0);
		return false;
	}

	public static boolean pathIsCircular(TreePath path) {
		boolean out = PathUtil
		.pathIsCircular(path, new HashSet<LinkedObject>());
		return out;
	}

	public static boolean pathIsCircular(TreePath path,
			Set<LinkedObject> scratchSet) {
		Object[] obs = path.getPath();
		for (int i = 0; i < obs.length; i++) {
			if (obs[i] instanceof Link) {
				Link tr = (Link) obs[i];
				// If we've already visited this child...
				if (scratchSet.contains(tr.getChild())
						// ...and it's not a symmetric relation  [added by NH, 9/5/2008]
						&& !(tr.getType().isSymmetric())) {
					//logger.debug("PathUtil: pathIsCircular: " + tr.getType().isSymmetric());
					return true;
				}
				scratchSet.add(tr.getChild());
			}
		}
		return false;
	}

	/**
	 * pathIsOneWayDisjoint - one way disjoint path exists already
	 * TRUE - one of the disjoint paths is added and isContained in existingLinks
	 * FLASE - no paths for this disjoint relation have been added 
	 * so go ahead and add this transitive path
	 * @param path
	 * @return
	 */
	public static boolean pathIsOneWayDisjoint(TreePath path) {
		boolean out = PathUtil
		.pathIsOneWayDisjoint(path, new HashSet<LinkedObject>());
		return out;
	}

	public static boolean pathIsOneWayDisjoint(TreePath path,
			Set<LinkedObject> disjointLinks) {
		Object o = path.getParentPath();
//		Object o = path.getLastPathComponent();
		Link link = (Link) o;
		logger.debug("\nlink: " + link);
		logger.debug("link.getType(): " + link.getType());

		if (link.getType().getID().equals("disjoint_from")){
			logger.debug("disjoint link -- return True");
			return true;
		}
		
		return false;
	}

	public static boolean isFake(Link link) {
		return (link.getType() == null || link.getParent() == null
				|| link.getType().equals(OBOProperty.REPLACES)
				|| link.getType().equals(OBOProperty.CONSIDER)
				|| TermUtil.isObsolete(link.getParent()) || TermUtil
				.isObsolete(link.getChild()));
	}

	public static TreePath resolve(OBOSession session, TreePath path) {
		Object[] objects = path.getPath();
		for (int i = 0; i < objects.length; i++) {
			if (objects[i] instanceof PathCapable) {
				objects[i] = TermUtil
				.resolve(session, (PathCapable) objects[i]);
			}
		}
		TreePath newpath = new TreePath(objects);
		return newpath;
	}

	public static final Object CLASSES = "CLASSES";

	public static final Object OBSOLETE = "OBSOLETE";

	public static final Object ROOT = "ROOT";

	public static final Object TYPES = "PROPERTIES";

	public static final Object INSTANCES = "INSTANCES";

}
