package org.oboedit.controller;

import java.awt.event.MouseEvent;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.LinkedList;

import javax.swing.JComponent;
import javax.swing.tree.TreePath;

import org.bbop.util.ObjectUtil;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.PathCapable;
import org.obo.datamodel.RootAlgorithm;
import org.obo.datamodel.impl.DefaultLinkDatabase;
import org.obo.util.TermUtil;
import org.oboedit.gui.DefaultGestureTarget;
import org.oboedit.gui.DefaultSelection;
import org.oboedit.gui.GestureTarget;
import org.oboedit.gui.ObjectSelector;
import org.oboedit.gui.Selection;
import org.oboedit.gui.Selection.PathCalcMode;
import org.oboedit.gui.event.ExpandCollapseListener;
import org.oboedit.gui.event.PreSelectionEvent;
import org.oboedit.gui.event.PreSelectionListener;
import org.oboedit.gui.event.RootChangeEvent;
import org.oboedit.gui.event.RootChangeListener;
import org.oboedit.gui.event.SelectionEvent;
import org.oboedit.gui.event.SelectionListener;
import org.oboedit.util.PathUtil;

import org.apache.log4j.*;

public class SelectionManager implements ObjectSelector {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SelectionManager.class);

	protected static SelectionManager selectionManager;

	protected List<SelectionListener> selectionListeners = new LinkedList<SelectionListener>();

	protected List<PreSelectionListener> preSelectionListeners = new LinkedList<PreSelectionListener>();

	protected LinkedList<Selection> selectionStack = new LinkedList<Selection>();

	protected LinkedList<Selection> forwardSelections = new LinkedList<Selection>();

	protected int maxSelectionStackSize = 10;
	
	protected Selection checkedPreSelection = null;

	protected SelectionManager() {
	}

	public void setMaxSelectionStackSize(int maxSelectionStackSize) {
		this.maxSelectionStackSize = maxSelectionStackSize;
	}

	public static SelectionManager getManager() {
		if (selectionManager == null) {
			selectionManager = new SelectionManager();
			SessionManager.getManager().addRootChangeListener(
					new RootChangeListener() {
						public void changeRoot(RootChangeEvent e) {
							try {
								selectNone();
							} catch (Exception ex) {
								ex.printStackTrace();
							}
						}
					});
		}
		return selectionManager;
	}

	public static Selection resolveSelectionDanglers(OBOSession session,
			Selection postSelection) {
		Collection<LinkedObject> terms = new LinkedList<LinkedObject>();
		for (LinkedObject lo : postSelection.getTerms()) {
			terms.add(TermUtil.resolve(session, lo));
		}

		Collection<Link> links = new LinkedList<Link>();
		for (Link link : postSelection.getLinks()) {
			links.add(TermUtil.resolve(session, link));
		}

		Link linkSubSelection = null;
		if (postSelection.getLinkSubSelection() != null) {
			linkSubSelection = TermUtil.resolve(session, postSelection
					.getLinkSubSelection());
		}
		LinkedObject termSubSelection = null;
		if (postSelection.getTermSubSelection() != null)
			termSubSelection = TermUtil.resolve(session, postSelection
					.getTermSubSelection());
		TreePath[] paths = null;
		if (postSelection.getMode().equals(PathCalcMode.DONT_CALCULATE)) {
			paths = new TreePath[postSelection.getPaths().length];
			int i = 0;
			for (TreePath path : postSelection.getPaths()) {
				paths[i++] = PathUtil.resolve(session, path);
			}
		}
		return SelectionManager.createSelection(postSelection.getComponent(),
				links, terms, paths, postSelection.getRootAlgorithm(),
				postSelection.getLinkDatabase(), postSelection.getMode(),
				linkSubSelection, termSubSelection);
	}
	
	public static Selection createEmptySelection() {
		return createEmptySelection(null);
	}

	public static Selection createEmptySelection(JComponent component) {
		Collection<Link> emptyLinkSet = Collections.emptySet();
		Collection<LinkedObject> emptyTermSet = Collections.emptySet();
		return new DefaultSelection(component, emptyLinkSet, emptyTermSet,
				new TreePath[0], null, null,
				DefaultSelection.PathCalcMode.DONT_CALCULATE, null, null);
	}

	public static Selection createSelectionFromLinks(JComponent component,
			Collection<Link> links, Link linkSubSelection, boolean fillInTerms) {
		return createSelectionFromLinks(component, links, RootAlgorithm.GREEDY,
				SessionManager.getManager().getSession().getLinkDatabase(),
				linkSubSelection, fillInTerms);
	}

	public static Selection createSelectionFromLinks(JComponent component,
			Collection<Link> links, RootAlgorithm rootAlgorithm,
			LinkDatabase linkDatabase, Link linkSubSelection,
			boolean fillInTerms) {
		Collection<LinkedObject> terms = new HashSet<LinkedObject>();
		LinkedObject subSelection = null;
		if (fillInTerms) {
			for (Link link : links)
				terms.add(link.getChild());
			if (terms.size() > 0)
				subSelection = terms.iterator().next();
		}
		if (linkSubSelection == null && links.size() > 0)
			linkSubSelection = links.iterator().next();
		return new DefaultSelection(component, links, terms, null,
				rootAlgorithm, linkDatabase,
				DefaultSelection.PathCalcMode.CALCULATE_FROM_LINKS,
				linkSubSelection, subSelection);
	}

	public static Selection createSelectionFromTerms(JComponent component,
			Collection<? extends LinkedObject> terms,
			LinkedObject subSelection, boolean fillInLinks) {
		return createSelectionFromTerms(component, terms, RootAlgorithm.GREEDY,
				SessionManager.getManager().getSession().getLinkDatabase(),
				subSelection, fillInLinks);
	}

	public static Selection createSelectionFromTerms(JComponent component,
			Collection<? extends LinkedObject> terms,
			RootAlgorithm rootAlgorithm, LinkDatabase linkDatabase,
			LinkedObject subSelection, boolean fillInLinks) {
		Collection<Link> links = new HashSet<Link>();
		Link linkSubSelection = null;
		if (fillInLinks) {
			for (LinkedObject lo : terms) {
				links.addAll(linkDatabase.getParents(lo));
			}
			if (links.size() > 0)
				linkSubSelection = links.iterator().next();
		}
		if (subSelection == null && terms.size() > 0)
			subSelection = terms.iterator().next();

		return new DefaultSelection(component, links, terms, null,
				rootAlgorithm, linkDatabase,
				DefaultSelection.PathCalcMode.CALCULATE_FROM_TERMS,
				linkSubSelection, subSelection);
	}

	public static Selection createSelectionFromPaths(JComponent component,
			TreePath[] paths, Link linkSubSelection, LinkDatabase linkDatabase,
			RootAlgorithm rootAlgorithm, boolean fillInTerms) {
//		Collection<Link> links = new HashSet<Link>();
//		Collection<LinkedObject> terms = new HashSet<LinkedObject>();
		// Want terms to be kept in order (which HashSet doesn't do).
		// However, keep in mind that LinkedHashSets are slower.
		Collection<Link> links = new LinkedHashSet<Link>();
		Collection<LinkedObject> terms = new LinkedHashSet<LinkedObject>();

		LinkedObject subSelection = null;
		for (TreePath path : paths) {
			if (path.getLastPathComponent() instanceof Link) {
				Link link = (Link) path.getLastPathComponent();
				links.add(link);
				if (fillInTerms)
					terms.add(link.getChild());
			}
		}
		if (fillInTerms)
			if (terms.size() > 0)
				subSelection = terms.iterator().next();
		if (linkSubSelection == null && links.size() > 0)
			linkSubSelection = links.iterator().next();
		return new DefaultSelection(component, links, terms, paths,
				rootAlgorithm, linkDatabase,
				DefaultSelection.PathCalcMode.DONT_CALCULATE, linkSubSelection,
				subSelection);
	}

	public static Selection createSelectionFromTarget(GestureTarget target) {
		TreePath[] paths = null;
		if (target.getPath() != null) {
			paths = new TreePath[1];
			paths[0] = target.getPath();
		}
		return new DefaultSelection(target.getComponent(), Collections
				.singleton(target.getLink()), Collections.singleton(target
				.getTerm()), paths, null, null,
				Selection.PathCalcMode.DONT_CALCULATE, null, null);
	}

	public static Selection addToSelection(Selection selection, PathCapable pc) {
		if (selection.getComponent() instanceof ObjectSelector) {
			ObjectSelector os = (ObjectSelector) selection.getComponent();
			return addToSelection(selection, pc, os.getRootAlgorithm(), os
					.getLinkDatabase());
		} else
			throw new IllegalArgumentException(
					"The selection must originate from an ObjectSelector");
	}

	public static Selection removeFromSelection(Selection selection,
			PathCapable pc) {
		Collection<PathCapable> pcs = new LinkedList<PathCapable>();
		pcs.add(pc);

		return removeFromSelection(selection, pcs);
	}

	public static Selection removeFromSelection(Selection selection,
			Collection<? extends PathCapable> pcs) {
		Collection<TreePath> pathCollection = new HashSet();
		if (selection.getPaths() != null) {
			for (TreePath path : selection.getPaths()) {
				pathCollection.add(path);
			}
		}
		Collection<Link> links = new HashSet<Link>(selection.getLinks());
		Collection<LinkedObject> terms = new LinkedHashSet<LinkedObject>(
				selection.getTerms());
		for (PathCapable pc : pcs) {
			if (pc instanceof Link) {
				links.remove((Link) pc);
				Iterator<TreePath> it = pathCollection.iterator();
				while (it.hasNext()) {
					TreePath path = it.next();
					if (path.getLastPathComponent().equals(pc)) {
						it.remove();
					}
				}
			}
			if (pc instanceof LinkedObject) {
				terms.remove((LinkedObject) pc);
				Iterator<TreePath> it = pathCollection.iterator();
				while (it.hasNext()) {
					TreePath path = it.next();
					Link lastLink = (Link) path.getLastPathComponent();
					if (lastLink.getChild().equals(pc)) {
						it.remove();
					}
				}
			}
		}
		Link subLink = null;
		LinkedObject subTerm = null;
		if (links.size() > 0)
			subLink = links.iterator().next();
		if (terms.size() > 0)
			subTerm = terms.iterator().next();
		TreePath[] paths = new TreePath[pathCollection.size()];
		Iterator<TreePath> it = pathCollection.iterator();
		for (int i = 0; it.hasNext(); i++) {
			paths[i] = it.next();
		}
		return SelectionManager.createSelection(selection.getComponent(),
				links, terms, paths, null, null, PathCalcMode.DONT_CALCULATE,
				subLink, subTerm);
	}

	public static Selection addToSelection(Selection selection, PathCapable pc,
			RootAlgorithm rootAlgorithm, LinkDatabase linkDatabase) {
		Collection<PathCapable> pcs = new LinkedList<PathCapable>();
		pcs.add(pc);
		Link subLink = selection.getLinkSubSelection();
		LinkedObject subTerm = selection.getTermSubSelection();

		if (pc instanceof Link)
			subLink = (Link) pc;
		else if (pc instanceof LinkedObject)
			subTerm = (LinkedObject) pc;

		return addToSelection(selection, pcs, subLink, subTerm, rootAlgorithm,
				linkDatabase);
	}

	public static Selection addToSelection(Selection selection,
			Collection<? extends PathCapable> pcs, Link subLink,
			LinkedObject subTerm, RootAlgorithm rootAlgorithm,
			LinkDatabase linkDatabase) {
		Collection<Link> links = new HashSet<Link>(selection.getLinks());
		Collection<LinkedObject> terms = new HashSet<LinkedObject>(selection
				.getTerms());
		if (subLink == null)
			subLink = selection.getLinkSubSelection();
		if (subTerm == null)
			subTerm = selection.getTermSubSelection();
		for (PathCapable pc : pcs) {
			if (pc instanceof Link) {
				links.add((Link) pc);
				if (subLink == null)
					subLink = (Link) pc;
			}
			if (pc instanceof LinkedObject) {
				terms.add((LinkedObject) pc);
				if (subTerm == null)
					subTerm = (LinkedObject) pc;
			}
		}
		return SelectionManager.createSelection(selection.getComponent(),
				links, terms, null, rootAlgorithm, linkDatabase,
				PathCalcMode.CALCULATE_FROM_TERMS_AND_LINKS, subLink, subTerm);
	}

	public static Selection createSelection(JComponent component,
			Collection<Link> links, Collection<? extends LinkedObject> terms,
			TreePath[] paths, RootAlgorithm rootAlgorithm,
			LinkDatabase linkDatabase, Selection.PathCalcMode mode,
			Link linkSubSelection, LinkedObject subSelection) {
		return new DefaultSelection(component, links, terms, paths,
				rootAlgorithm, linkDatabase, mode, linkSubSelection,
				subSelection);
	}

	public static Selection changeSubSelection(Selection selection,
			LinkedObject newSubSelection) {
		return new DefaultSelection(selection.getComponent(), selection
				.getLinks(), selection.getTerms(), selection.getPaths(), null,
				null, DefaultSelection.PathCalcMode.DONT_CALCULATE, selection
						.getLinkSubSelection(), newSubSelection);
	}

	public static void setGlobalSelection(Selection selection) {
		logger.debug("SelectionManager -- setGlobalSelection -- selection: " + selection);
		if (selection == null)
			selectNone(null);
		else
			getManager().select(selection);
	}

	public static void selectNone() {
		selectNone(null);
	}

	public static void selectNone(JComponent source) {
		getManager().select(createEmptySelection(source));
	}

	public void select(Selection selection) {
		if (doPreSelectValidation(selection)) {
			selectionStack.add(selection);
			if (selectionStack.size() > maxSelectionStackSize)
				selectionStack.removeFirst();
			forwardSelections.clear();
			notifyListeners();
			checkedPreSelection = null;
		}
	}

	public boolean hasPreviousSelection() {
		return selectionStack.size() > 0;
	}

	public boolean hasNextSelection() {
		return forwardSelections.size() > 0;
	}

	public void previousSelection() {
		if (selectionStack.size() > 0) {
			Selection s = selectionStack.removeLast();
			forwardSelections.add(s);
		}
		notifyListeners();
	}

	public void nextSelection() {
		if (forwardSelections.size() > 0) {
			Selection s = forwardSelections.removeLast();
			selectionStack.add(s);
			notifyListeners();
		}
	}

	public void addPreSelectionListener(PreSelectionListener listener) {
		preSelectionListeners.add(listener);
	}

	public void removePreSelectionListener(PreSelectionListener listener) {
		preSelectionListeners.remove(listener);
	}

	public void addSelectionListener(SelectionListener listener) {
		selectionListeners.add(listener);
	}

	public void removeSelectionListener(SelectionListener listener) {
		selectionListeners.remove(listener);
	}

	protected void notifyListeners() {
		Selection current = getSelection();
		Object source;
		if (current.getComponent() == null)
			source = this;
		else
			source = current.getComponent();
		fireSelectionEvent(new SelectionEvent(source, current));
	}

	public Selection getSelection() {
		if (selectionStack.size() == 0)
			return createEmptySelection(null);
		else
			return selectionStack.getLast();
	}

	public static Selection getGlobalSelection() {
		return getManager().getSelection();
	}

	protected void fireSelectionEvent(SelectionEvent event) {
		Collection<SelectionListener> selectionListeners = new LinkedList<SelectionListener>(
				this.selectionListeners);
		for (SelectionListener listener : selectionListeners) {
			listener.selectionChanged(event);
		}
	}

	public boolean doPreSelectValidation(Selection source) {
		if (ObjectUtil.equals(source, checkedPreSelection))
			return true;
		PreSelectionEvent event = new PreSelectionEvent(source);
		Iterator it = new LinkedList<PreSelectionListener>(
				preSelectionListeners).iterator();
		while (it.hasNext()) {
			PreSelectionListener listener = (PreSelectionListener) it.next();
			if (!listener.isPreSelectOkay(event)) {
				checkedPreSelection = null;
				return false;
			}
		}
		checkedPreSelection = source;
		return true;
	}

	public static GestureTarget createEmptyTarget(JComponent source) {
		return new DefaultGestureTarget(source, null, null, null, null, null,
				Selection.PathCalcMode.DONT_CALCULATE);
	}

	public static GestureTarget createGestureTarget(JComponent source,
			LinkDatabase linkDatabase, RootAlgorithm rootAlgorithm,
			PathCapable pc) {
		if (pc instanceof Link) {
			return createGestureTarget(source, linkDatabase, rootAlgorithm,
					(Link) pc, false);
		} else if (pc instanceof LinkedObject) {
			return createGestureTarget(source, linkDatabase, rootAlgorithm,
					(LinkedObject) pc);
		} else
			throw new IllegalArgumentException("path capable argument " + pc
					+ " is not a linkedObject or a link");
	}

	public static GestureTarget createGestureTarget(JComponent source,
			LinkDatabase linkDatabase, RootAlgorithm rootAlgorithm, Link link,
			boolean fillInTerm) {
		return new DefaultGestureTarget(source, link, (fillInTerm ? link
				.getChild() : null), null, rootAlgorithm, linkDatabase,
				Selection.PathCalcMode.CALCULATE_FROM_LINKS);
	}

	public static GestureTarget createGestureTarget(JComponent source,
			TreePath path, boolean fillInTerm) {
		if (path == null)
			return createEmptyTarget();
		if (!(path.getLastPathComponent() instanceof Link))
			return createEmptyTarget();
		Link link = (Link) path.getLastPathComponent();
		return new DefaultGestureTarget(source, link, (fillInTerm ? link
				.getChild() : null), path, null, null,
				Selection.PathCalcMode.DONT_CALCULATE);
	}

	public static GestureTarget createGestureTarget(JComponent source,
			LinkDatabase linkDatabase, RootAlgorithm rootAlgorithm,
			LinkedObject term) {
		return new DefaultGestureTarget(source, null, term, null,
				rootAlgorithm, linkDatabase,
				Selection.PathCalcMode.CALCULATE_FROM_TERMS);
	}

	public static GestureTarget createGestureTarget(JComponent source,
			LinkedObject term) {
		RootAlgorithm rootAlgorithm = null;
		LinkDatabase linkDatabase = null;
		if (source instanceof ObjectSelector) {
			rootAlgorithm = ((ObjectSelector) source).getRootAlgorithm();
			linkDatabase = ((ObjectSelector) source).getLinkDatabase();
		}
		return createGestureTarget(source, linkDatabase, rootAlgorithm, term);
	}

	public static void selectTerm(JComponent component, LinkedObject lo) {
		getManager().select(component, lo);
	}
	
	public void select(JComponent component, LinkedObject lo) {
		select(createSelectionFromTerms(component, Collections
				.singleton(lo), lo, false));
	}
	
	public void select(JComponent component, Link link, boolean fillInTerms) {
		select(createSelectionFromLinks(component, Collections
				.singleton(link), link, fillInTerms));
	}

	public static void selectLink(JComponent component, Link link,
			boolean fillInTerms) {
		getManager().select(component, link, fillInTerms);
	}

	public static void selectTerms(JComponent component,
			Collection<? extends LinkedObject> terms) {
		setGlobalSelection(createSelectionFromTerms(component, terms, null,
				false));
	}

	public static GestureTarget createEmptyTarget() {
		return createEmptyTarget(null);
	}

	public static Selection createSelection(JComponent component,
			Collection<? extends PathCapable> pcs, Link subLink,
			LinkedObject subTerm, RootAlgorithm rootAlgorithm,
			LinkDatabase linkDatabase) {
		Collection<Link> links = new LinkedList<Link>();
		Collection<LinkedObject> terms = new LinkedList<LinkedObject>();
		for (PathCapable pc : pcs) {
			if (pc instanceof Link) {
				links.add((Link) pc);
				if (subLink == null)
					subLink = (Link) pc;
			}
			if (pc instanceof LinkedObject) {
				terms.add((LinkedObject) pc);
				if (subTerm == null)
					subTerm = (LinkedObject) pc;
			}
		}
		return SelectionManager.createSelection(component, links, terms, null,
				rootAlgorithm, linkDatabase,
				PathCalcMode.CALCULATE_FROM_TERMS_AND_LINKS, subLink, subTerm);
	}

	public static Selection createSelection(JComponent component,
			Collection<? extends PathCapable> pcs) {
		return createSelection(component, pcs, null, null,
				RootAlgorithm.GREEDY, DefaultLinkDatabase.getDefault());
	}

	public static Selection createSelection(JComponent component,
			PathCapable pc, RootAlgorithm rootAlgorithm,
			LinkDatabase linkDatabase) {
		Collection<PathCapable> pcs = new LinkedList<PathCapable>();
		pcs.add(pc);
		Link subLink = null;
		LinkedObject subTerm = null;

		if (pc instanceof Link)
			subLink = (Link) pc;
		else if (pc instanceof LinkedObject)
			subTerm = (LinkedObject) pc;

		return createSelection(component, pcs, subLink, subTerm, rootAlgorithm,
				linkDatabase);
	}

	public static void selectTerm(JComponent component, LinkedObject lo,
			boolean fillInLinks) {
		setGlobalSelection(createSelectionFromTerms(component, Collections
				.singleton(lo), lo, fillInLinks));
	}

	public LinkDatabase getLinkDatabase() {
		return SessionManager.getManager().getSession().getLinkDatabase();
	}

	public RootAlgorithm getRootAlgorithm() {
		return RootAlgorithm.GREEDY;
	}

	public Selection getSelection(MouseEvent e) {
		return getSelection();
	}

	public boolean hasCombinedTermsAndLinks() {
		return false;
	}

	public boolean isLive() {
		return this.equals(getManager());
	}

	/**
	 * The global SelectionManager cannot be made "unlive", since the definition
	 * of a "live" SelectionManager is one that is linked to the global
	 * selection. Any attempt to make the global selection manager unlive will
	 * result in a runtime exception.
	 */
	public void setLive(boolean isLive) {
		if (isLive != isLive())
			throw new IllegalArgumentException(
					"The 'live' status of a selection "
							+ "manager cannot be controlled in this way.");
	}

	public void addExpansionListener(ExpandCollapseListener listener) {
		// do nothing
	}

	public Collection<PathCapable> getVisibleObjects() {
		return Collections.emptyList();
	}

	public void removeExpansionListener(ExpandCollapseListener listener) {
		// do nothing		
	}
}
