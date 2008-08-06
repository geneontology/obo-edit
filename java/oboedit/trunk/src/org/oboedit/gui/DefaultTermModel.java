package org.oboedit.gui;

import javax.swing.event.*;
import javax.swing.tree.*;

import org.bbop.util.*;

import java.util.*;

import org.obo.datamodel.*;
import org.obo.datamodel.impl.FilteredLinkDatabase;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.datamodel.impl.TrivialLink;
import org.obo.filters.*;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.impl.TrimmedLinkDatabase;
import org.obo.util.FilterUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.FilterManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.*;
import org.oboedit.gui.event.*;
import org.oboedit.util.GUIUtil;
import org.oboedit.util.PathUtil;

import org.apache.log4j.*;

public class DefaultTermModel implements TermModel {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefaultTermModel.class);

        protected boolean SHOW_DISJOINTS_AS_LEAVES = true;  // ! Make this settable

	protected Vector listeners = new Vector();

	protected boolean showTerms = true;

	protected boolean showTypes = true;

	protected boolean showObsoletes = true;

	protected boolean showInstances = false;

	// protected static final int MAX_CACHE_SIZE = 100;

	protected Map childCache;

	protected Map leafCache;

	protected List<Object> topLevel = new LinkedList<Object>();

	public LinkDatabase getLinkDatabase() {
		return linkDatabase;
	}

	protected List classRoots = new Vector();

	protected List typeRoots = new Vector();

	protected List instanceRoots = new Vector();

	protected List obsoleteRoots = new Vector();

	protected LinkDatabase linkDatabase;

	protected TrimmedLinkDatabase trimmedLinkDB;

	protected Filter userTermFilter;

	protected Filter userLinkFilter;

	protected CompoundFilter linkFilter = new CompoundFilterImpl();

	protected CompoundFilter termFilter = new CompoundFilterImpl();

	protected RootAlgorithm rootAlgorithm = RootAlgorithm.GREEDY;

	protected TermSorter comparator = new TermSorter(true);

	protected FilterManager manager = FilterManager.getManager();

	// JTree does some unnecessary checks when expanding paths to make sure that
	// you are only expanding a valid path. We don't have time for this,
	// especially
	// when we have a model with a bunch of filter layers, so we say that there
	// are
	// no leaves in the model unless we're painting
	protected boolean answerLeafHonestly = false;

	public void setPainting(boolean isPainting) {
		answerLeafHonestly = isPainting;
	}

	public RootAlgorithm getRootAlgorithm() {
		return rootAlgorithm;
	}

	public void setRootAlgorithm(RootAlgorithm rootAlgorithm) {
		this.rootAlgorithm = rootAlgorithm;
	}

	protected ReconfigListener reconfigListener = new ReconfigListener() {
		public void configReloaded(ReconfigEvent e) {
			setSortMode(true);
		}
	};

	// The problem with this listener is that history events were triggering fireTreeStructureChanged
	// which was making the OTE close its open nodes in response to, say, a text edit--very
	// annoying.  But for other history events we *do* need the OTE to reset--for example,
	// on initial load.  So I've gotten rid of this reloadListener and gone back to having
	// OBOTermPanel call upon this class to reload when appropriate.  There's a new method
	// reloadFilters for that.
//  	protected ReloadListener reloadListener = new ReloadListener() {
//  	        public void reload(ReloadEvent e) {
// //			if (e.isHistory()) // Don't need to reload for history events
// //				return;
//  			linkFilter.clear();
//  			termFilter.clear();
//  			initializeFilters();
// 			DefaultTermModel.this.reload();
//  		} };

	protected static class TermSorter implements Comparator {
		protected boolean ignoreCase = true;

		public TermSorter(boolean ignoreCase) {
			setIgnoreCase(ignoreCase);
		}

		public void setIgnoreCase(boolean ignoreCase) {
			this.ignoreCase = ignoreCase;
		}

		public int compare(Object a, Object b) {
			int rankinga = getObjectRanking(a);
			int rankingb = getObjectRanking(b);

			if (rankinga == 0 && rankingb == 0) {
				Link la = (Link) a;
				Link lb = (Link) b;
				IdentifiedObject terma = la.getChild();
				IdentifiedObject termb = lb.getChild();
				String namea = terma.getName();
				String nameb = termb.getName();
				if (namea == null && nameb == null)
				    return 0;
				if (namea == null)
				    return -1;
				if (nameb == null)
				    return 1;

				int compval;
				if (ignoreCase)
				    compval = namea.compareToIgnoreCase(nameb);
				else
				    compval = namea.compareTo(nameb);

				if (compval == 0) {
					if (la.getType() != lb.getType()) {
						if (la.getType() == null)
							compval = -1;
						else if (lb.getType() == null)
							compval = 1;
						else {
							if (ignoreCase)
								compval = la.getType().getID()
										.compareToIgnoreCase(
												lb.getType().getID());
							else
								compval = la.getType().getID().compareTo(
										lb.getType().getID());
						}
					}
				}

				return compval;
			} else {
				if (rankinga < rankingb) {
					return -1;
				} else if (rankinga > rankingb) {
					return 1;
				} else
					return 0;
			}
		}

		protected static int getObjectRanking(Object a) {
			if (a instanceof Link)
				return 0;
			else if (a.equals(PathUtil.ROOT))
				return 1;
			else if (a.equals(PathUtil.TYPES))
				return 2;
			else if (a.equals(PathUtil.OBSOLETE))
				return 3;
			else if (a.equals(PathUtil.INSTANCES))
				return 4;
			else
				return -1;
		}
	}

	public void reloadFilters() {
		linkFilter.clear();
		termFilter.clear();
		initializeFilters();
		DefaultTermModel.this.reload();
	}

	protected void initCaches() {
		childCache = new LinkedHashMap() {
			/**
			 * 
			 */
			private static final long serialVersionUID = -9192215743438642387L;

			@Override
			protected boolean removeEldestEntry(Map.Entry eldest) {
				boolean doRemove = size() >= getViewCacheSize();
				return doRemove;
			}
		};

		leafCache = new LinkedHashMap() {
			/**
			 * 
			 */
			private static final long serialVersionUID = -9192215743438642387L;

			@Override
			protected boolean removeEldestEntry(Map.Entry eldest) {
				boolean doRemove = size() >= getViewCacheSize();
				return doRemove;
			}
		};
	}

	protected int getViewCacheSize() {
		return 1000;
	}

	protected void buildTopLevel() {
		topLevel.clear();
		if (showTerms)
			topLevel.add(PathUtil.CLASSES);
		if (showTypes)
			topLevel.add(PathUtil.TYPES);
		if (showInstances)
			topLevel.add(PathUtil.INSTANCES);
		if (showObsoletes)
			topLevel.add(PathUtil.OBSOLETE);
	}

	public DefaultTermModel() {
		this.listeners = new Vector();
		initCaches();
		buildTopLevel();
		initializeFilters();
		setSortMode(false);
	}

	protected void initializeFilters() {
		if (userTermFilter != null)
			termFilter.addFilter(userTermFilter);

		termFilter.addFilter(manager.getGlobalTermFilter());

		if (userLinkFilter != null) {
			linkFilter.addFilter(userLinkFilter);
		}

		linkFilter.addFilter(manager.getGlobalLinkFilter());
	}

	public void setLinkFilter(Filter filter) {
		if ((filter == null && userLinkFilter == null) ||
		    filter.equals(userLinkFilter)) {
			// Avoid unnecessary reload
			return;
		}

		if (userLinkFilter != null)
			linkFilter.removeFilter(userLinkFilter);
		this.userLinkFilter = filter;
		if (userLinkFilter != null)
			linkFilter.addFilter(userLinkFilter);
//		logger.info("DefaultTermModel.setLinkFilter: after adding new filter, now linkfilter = " + linkFilter); // DEL
		reload();
	}

	public void setTermFilter(Filter filter) {
		if ((filter == null && userTermFilter == null) ||
		    filter.equals(userTermFilter))
			// Avoid unnecessary reload
			return;
		if (userTermFilter != null)
			termFilter.removeFilter(userTermFilter);
		this.userTermFilter = filter;
		if (userTermFilter != null)
			termFilter.addFilter(userTermFilter);
		reload();
	}

	public Filter getTermFilter() {
		return userTermFilter;
	}

	public Filter getLinkFilter() {
		return userLinkFilter;
	}

	public void setShowTerms(boolean showTerms) {
		this.showTerms = showTerms;
	}

	public void setShowTypes(boolean showTypes) {
		this.showTypes = showTypes;
	}

	public void setShowObsoletes(boolean showObsoletes) {
		this.showObsoletes = showObsoletes;
	}

	public void setShowInstances(boolean showInstances) {
		this.showInstances = showInstances;
	}

	public boolean getShowTerms() {
		return showTerms;
	}

	public boolean getShowTypes() {
		return showTypes;
	}

	public boolean getShowObsoletes() {
		return showObsoletes;
	}

	public void init() {
		Preferences.getPreferences().addReconfigListener(reconfigListener);
//		GUIUtil.addReloadListener(reloadListener);
		setSortMode(!Preferences.getPreferences().getCaseSensitiveSort());
	}

	public void cleanup() {
		Preferences.getPreferences().removeReconfigListener(reconfigListener);
//		GUIUtil.removeReloadListener(reloadListener);
	}

	protected void setSortMode(boolean sortMode) {
		comparator.setIgnoreCase(sortMode);
	}

	public Object getChild(Object parent, int index) {
		return getChildren(parent).get(index);
	}

	public int getChildCount(Object parent) {
		return getChildren(parent).size();
	}

	public int getIndexOfChild(Object parent, Object child) {
		return getChildren(parent).indexOf(child);
	}

	public Object getRoot() {
		return PathUtil.ROOT;
	}

	public boolean isLeaf(Object parent) {
		if (answerLeafHonestly) {
			if (parent instanceof Relationship) {
				OBOProperty relationshipType = ((Relationship)parent).getType();
				if (SHOW_DISJOINTS_AS_LEAVES && relationshipType != null && relationshipType.equals(OBOProperty.DISJOINT_FROM)) {
					return true;
				}

				LinkedObject lo = ((Relationship) parent).getChild();
				if (TermUtil.isObsolete(lo)) {
					return getReplacements((ObsoletableObject) lo).isEmpty();
				} else {
					Collection children = (Collection) childCache.get(lo);
					if (children != null)
						return children.isEmpty();
					Boolean val = (Boolean) leafCache.get(lo);
					if (val == null) {
						val = linkDatabase.getChildren(lo).isEmpty() ? Boolean.TRUE
								: Boolean.FALSE;
						leafCache.put(lo, val);
					}
					return val.booleanValue();
				}
			} else
				return getChildren(parent).size() == 0;
		} else
			return false;
	}

	public void addTreeModelListener(TreeModelListener l) {
		listeners.addElement(l);
	}

	protected void buildFilteredDatabase() {
		// I think this may be the reason that filters that are in place at load time don't go away
		// when removed by the user:  it is going back to the session to get the original link database.
		FilteredLinkDatabase filteredLinkDatabase = new FilteredLinkDatabase(
//				SessionManager.getManager().getSession().getLinkDatabase());
				SessionManager.getManager().getCurrentLinkDatabase());
		if (SessionManager.getManager().getUseReasoner()) {
			filteredLinkDatabase = new FilteredLinkDatabase(SessionManager
					.getManager().getReasoner());
			filteredLinkDatabase.setFilters(getMergedTermFilter(),
							getMergedLinkFilter());
			trimmedLinkDB = new TrimmedLinkDatabase(filteredLinkDatabase);
			linkDatabase = trimmedLinkDB;
			// if (filterProperty == null)
			// trimmedLinkDB = new TrimmedLinkDatabase(SessionManager
			// .getManager().getReasoner());
			// else {
			// final ReasonedLinkDatabase ldb = SessionManager.getManager()
			// .getReasoner();
			// final FilteredLinkDatabase propertyFiltered = new
			// FilteredLinkDatabase(
			// ldb);
			// /*
			// * propertyFiltered
			// * .setFilterMethod(FilteredLinkDatabase.REUSABLE_ITERATOR);
			// */
			// propertyFiltered.setLinkFilter(getPropertyLinkFilter());
			//
			// trimmedLinkDB = new TrimmedLinkDatabase(propertyFiltered);
			// }
			// filteredLinkDatabase = new FilteredLinkDatabase(trimmedLinkDB);
			// linkDatabase = filteredLinkDatabase;
		} else {
			trimmedLinkDB = null;
			filteredLinkDatabase = new FilteredLinkDatabase(SessionManager
					.getManager().getSession().getLinkDatabase());
			filteredLinkDatabase.setFilters(getMergedTermFilter(),
							getMergedLinkFilter());
			linkDatabase = filteredLinkDatabase;
		}
	}

	protected Filter getMergedTermFilter() {
		return FilterUtil.mergeFilters(manager
				.getGlobalTermFilter(), termFilter);
	}

	protected Filter getMergedLinkFilter() {
		return FilterUtil.mergeFilters(manager
				.getGlobalLinkFilter(), linkFilter);
	}

	public void reload() {
		initCaches();
		buildTopLevel();
		buildFilteredDatabase();
		classRoots = new Vector();
		typeRoots = new Vector();
		instanceRoots = new Vector();
		obsoleteRoots = new Vector();

		Set<LinkedObject> temp = new HashSet<LinkedObject>();
		try {
			long time = System.currentTimeMillis();
			if (trimmedLinkDB != null)
				trimmedLinkDB.setEnableTrimming(false);
			TermUtil.detectRoots(temp, linkDatabase, getRootAlgorithm());
			if (trimmedLinkDB != null)
				trimmedLinkDB.setEnableTrimming(true);
//			logger.info("   detected roots in "
//					+ (System.currentTimeMillis() - time));
		} catch (Throwable t) {
			t.printStackTrace();
		}
		Iterator it = temp.iterator();
		while (it.hasNext()) {
			IdentifiedObject identified = (IdentifiedObject) it.next();
			if (identified instanceof Instance) {
				Instance instance = (Instance) identified;
				Link link = new TrivialLink(instance);
				VectorUtil.insertSorted(instanceRoots, comparator, link);
			} else if (identified instanceof LinkedObject) {
				LinkedObject io = (LinkedObject) identified;
				Link rootLink = new OBORestrictionImpl(io);

				if (TermUtil.isObsolete(io))
					VectorUtil
							.insertSorted(obsoleteRoots, comparator, rootLink);
				else if (io instanceof OBOClass)
					VectorUtil.insertSorted(classRoots, comparator, rootLink);
				else if (io instanceof OBOProperty)
					VectorUtil.insertSorted(typeRoots, comparator, rootLink);
			}
		}
		// There are times when we don't want this to fire, because it collapses the tree.
		logger.debug("DefaultTermModel.reload: fireTreeStructureChanged"); // DEL
		fireTreeStructureChanged(new TreeModelEvent(this, new TreePath(PathUtil.ROOT)));
	}

	protected void fireTreeStructureChanged(TreeModelEvent e) {
		for (int i = 0; i < listeners.size(); i++) {
			TreeModelListener tml = (TreeModelListener) listeners.elementAt(i);
			tml.treeStructureChanged(e);
		}
	}

	protected void fireNodesInserted(TreeModelEvent e) {
		for (int i = 0; i < listeners.size(); i++) {
			TreeModelListener tml = (TreeModelListener) listeners.elementAt(i);
			tml.treeNodesInserted(e);
		}
	}

	protected void fireNodesRemoved(TreeModelEvent e) {
		for (int i = 0; i < listeners.size(); i++) {
			TreeModelListener tml = (TreeModelListener) listeners.elementAt(i);
			tml.treeNodesRemoved(e);
		}
	}

	protected void fireNodeChanged(TreeModelEvent e) {
		for (int i = 0; i < listeners.size(); i++) {
			TreeModelListener tml = (TreeModelListener) listeners.elementAt(i);
			tml.treeNodesChanged(e);
		}
	}

	public void removeTreeModelListener(TreeModelListener l) {
		listeners.removeElement(l);
	}

	public void valueForPathChanged(TreePath path, Object newVal) {
	}

	public List wrapSet(Collection c) {
		return wrapSet(c.iterator());
	}

	public List wrapSet(Iterator it) {
		boolean HIDE_DISJOINT = true;
		List out = new ArrayList();
		while (it.hasNext()) {
			Object o = it.next();
			Link link = null;
			if (o instanceof LinkedObject) {
				LinkedObject io = (LinkedObject) o;
				link = new OBORestrictionImpl(io);
			} else if (o instanceof Link) {
				// We can keep the disjoint relations out of the OBOTermPanel this way...
//				if (HIDE_DISJOINT && ((Link)o).getType() != null && ((Link)o).getType().equals(OBOProperty.DISJOINT_FROM)) {
//					logger.debug("wrapSet: not adding " + o); // DEL
//					continue;
//				}
				link = (Link) o;
			}

//			logger.debug("wrapSet: link = " + link); // DEL
			VectorUtil.insertSorted(out, comparator, link);
		}
		return out;
	}

	protected Set getReplacements(ObsoletableObject oo) {
		Set out = new HashSet();

		Iterator it;
		it = oo.getReplacedBy().iterator();
		while (it.hasNext()) {
			ObsoletableObject replacedBy = (ObsoletableObject) it.next();
			out.add(new OBORestrictionImpl((LinkedObject) replacedBy,
					OBOProperty.REPLACES, (LinkedObject) oo));
		}
		/*
		 * if (oo instanceof LinkedObject && oo.getReplacedBy() &&
		 * oo.getReplacedBy() instanceof LinkedObject) out.add(new
		 * OBORestrictionImpl((LinkedObject) oo.getReplacedBy(),
		 * OBOProperty.REPLACES, (LinkedObject) oo)); }
		 */
		it = oo.getConsiderReplacements().iterator();
		while (it.hasNext()) {
			ObsoletableObject consider = (ObsoletableObject) it.next();
			if (oo instanceof LinkedObject && consider instanceof LinkedObject)
				out.add(new OBORestrictionImpl((LinkedObject) consider,
						OBOProperty.CONSIDER, (LinkedObject) oo));
		}
		return out;
	}

	public List getChildren(Object parent) {
		if (parent.equals(PathUtil.ROOT))
			return topLevel;
		else if (parent.equals(PathUtil.CLASSES)) {
			return classRoots;
		} else if (parent.equals(PathUtil.TYPES))
			return typeRoots;
		else if (parent.equals(PathUtil.OBSOLETE))
			return obsoleteRoots;
		else if (parent instanceof Relationship) {
			LinkedObject lo = (LinkedObject) SessionManager.getManager().getSession()
					.getObject(((Relationship) parent).getChild().getID());
			List out = (List) childCache.get(lo);

			if (out == null) {
				if (TermUtil.isObsolete(lo)) {
					out = wrapSet(getReplacements((ObsoletableObject) lo));
				} else {
					Collection<Link> children = linkDatabase.getChildren(lo);
					// Not used?!
//					Collection<Link> realChildren = SessionManager.getManager()
//							.getSession().getLinkDatabase().getChildren(lo);
					out = wrapSet(children);
				}
				childCache.put(lo, out);
				leafCache.remove(lo);
			}
			return out;
		} else {
			logger.info("requested children of unknown object " + parent);
			return null;
		}
	}

	public String toString() { // For debugging
		return "DefaultTermModel: linkFilter = " + linkFilter; 
	}
}
