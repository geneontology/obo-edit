package org.obo.util;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

import org.bbop.util.VectorFilter;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.SynonymCategory;
import org.obo.datamodel.SynonymedObject;
import org.obo.datamodel.TermCategory;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.history.HistoryItem;
import org.obo.history.HistoryList;
import org.obo.history.StringRelationship;
import org.obo.history.TermMacroHistoryItem;

public class HistoryUtil {

	/**
	 * Does a depth-first search of a {@link TermMacroHistoryItem} to find
	 * {@link HistoryItem}s that match the provided {@link VectorFilter}.
	 * 
	 * @param mitem
	 * @param filter
	 * @return a list of {@link HistoryItem}s that match the filter
	 */
	public static Collection findMatchingItems(TermMacroHistoryItem mitem,
			VectorFilter filter) {
		LinkedList out = new LinkedList();
		for (int i = 0; i < mitem.size(); i++) {
			HistoryItem item = mitem.getItemAt(i);
			if (filter.satisfies(item))
				out.add(item);
			if (item instanceof TermMacroHistoryItem)
				out.addAll(findMatchingItems((TermMacroHistoryItem) item,
						filter));
		}
		return out;
	}

	/**
	 * Does a depth-first search of a {@link HistoryList} to find
	 * {@link HistoryItem}s that match the provided {@link VectorFilter}.
	 * 
	 * @param list
	 * @param filter
	 * @return a list of {@link HistoryItem}s that match the filter
	 */
	public static Collection findMatchingItems(HistoryList list,
			VectorFilter filter) {
		LinkedList out = new LinkedList();
		Iterator it = list.getHistoryItems();
		while (it.hasNext()) {
			HistoryItem item = (HistoryItem) it.next();
			if (filter.satisfies(item))
				out.add(item);
			if (item instanceof TermMacroHistoryItem) {
				out.addAll(findMatchingItems((TermMacroHistoryItem) item,
						filter));
			}
		}
		return out;
	}

	public static Link createRealRel(OBOSession session,
			StringRelationship tr) {
		if (tr == null)
			return null;
		LinkedObject parent = (LinkedObject) session.getObject(tr.getParent());
		LinkedObject child = (LinkedObject) session.getObject(tr.getChild());
		OBOProperty type = (OBOProperty) session.getObject(tr.getType());
	
		Namespace ns = session.getNamespace(tr.getNamespace());
		/*
		OBORestriction realtr = new OBORestrictionImpl(child, parent, type);
		*/
		OBORestriction realtr = session.getObjectFactory().createOBORestriction(child, type, parent, false);
		realtr.setNamespace(ns);
		realtr.setNecessarilyTrue(tr.isNecessary());
		realtr.setInverseNecessarilyTrue(tr.isInverseNecessary());
		realtr.setCompletes(tr.completes());
		realtr.setMinCardinality(tr.getMinCardinality());
		realtr.setMaxCardinality(tr.getMaxCardinality());
		realtr.setCardinality(tr.getCardinality());
		
		return realtr;
	}

	public static Link getRealRel(OBOSession session,
			StringRelationship tr) {
		if (tr == null)
			return null;
		LinkedObject child = (LinkedObject) session.getObject(tr.getChild());
		OBOProperty type = (OBOProperty) session.getObject(tr.getType());
		LinkedObject parent = (LinkedObject) session.getObject(tr.getParent());
	
		if (child == null)
			return null;
	
		Iterator it = child.getParents().iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (link.getType().equals(type) && link.getParent().equals(parent)) {
				return link;
			}
		}
		return null;
	}

	public static Link findChildRel(Link tr, LinkedObject t) {
		if (t == null)
			if (tr.getParent() == null)
				return tr;
			else {
				return null;
			}
		Iterator it = t.getChildren().iterator();
		while (it.hasNext()) {
			Link ptr = (Link) it.next();
			if (TermUtil.equals(ptr, tr))
				return ptr;
		}
		return null;
	}

	/**
	 * Returns the {@link TermCategory} with the given name in the given
	 * {@link OBOSession}, or null if no such category exists.
	 */
	public static TermCategory findCategory(String name, OBOSession session) {
		Iterator it = session.getCategories().iterator();
		while (it.hasNext()) {
			TermCategory cat = (TermCategory) it.next();
			if (cat.getName().equals(name))
				return cat;
		}
		return null;
	}

	/**
	 * Finds the {@link TermCategory} in the given {@link OBOSession} that has
	 * the same name as the given {@link TermCategory}. If no matching category
	 * can be found, null is returned.
	 */
	public static TermCategory findCategory(TermCategory cat, OBOSession session) {
		return findCategory(cat.getName(), session);
	}

	public static Link findChildRelNoIntersection(Link tr, LinkedObject t) {
		if (t == null)
			if (tr.getParent() == null)
				return tr;
			else
				return null;
		Iterator it = t.getChildren().iterator();
		while (it.hasNext()) {
			Link ptr = (Link) it.next();
			if (TermUtil.equalsWithoutIntersection(ptr, tr))
				return ptr;
		}
		return null;
	}

	public static Link findParentRelNoIntersection(Link tr, LinkedObject t) {
		Iterator it = t.getParents().iterator();
		while (it.hasNext()) {
			Link ctr = (Link) it.next();
			if (TermUtil.equalsWithoutIntersection(ctr, tr))
				return ctr;
		}
		return null;
	}

	public static Link findParentRel(Link tr, LinkedObject t) {
		Iterator it = t.getParents().iterator();
		while (it.hasNext()) {
			Link ctr = (Link) it.next();
			if (TermUtil.equals(ctr, tr))
				return ctr;
		}
		return null;
	}

	public static Namespace findNamespace(Namespace ns, OBOSession session) {
		return HistoryUtil.findNamespace(ns.getID(), session);
	}

	public static Namespace findNamespace(String nsid, OBOSession session) {
		Iterator it = session.getNamespaces().iterator();
		while (it.hasNext()) {
			Namespace ns = (Namespace) it.next();
			if (ns.getID().equals(nsid))
				return ns;
		}
		return null;
	}

	public static Synonym findSynonym(SynonymedObject t, String stext) {
		Iterator it = t.getSynonyms().iterator();
		while (it.hasNext()) {
			Synonym s = (Synonym) it.next();
			if (s.getText().equals(stext))
				return s;
		}
		return null;
	}

	public static Synonym findSynonym(SynonymedObject t, Synonym s) {
		return findSynonym(t, s.getText());
	}

	public static SynonymCategory findSynonymCategory(String cat_id,
			OBOSession session) {
		Iterator it = session.getSynonymCategories().iterator();
		while (it.hasNext()) {
			SynonymCategory cat = (SynonymCategory) it.next();
			if (cat.getID().equals(cat_id))
				return cat;
		}
		return null;
	}

	public static SynonymCategory findSynonymCategory(SynonymCategory cat,
			OBOSession session) {
		return findSynonymCategory(cat.getID(), session);
	}

	public static boolean hasChild(LinkedObject t, Link tr) {
		if (tr.getType() == null)
			return false;
		Iterator it = t.getChildren().iterator();
		while (it.hasNext()) {
			Link tra = (Link) it.next();
			if (tr.getChild().equals(tra.getChild())
					&& tr.getType().equals(tra.getType()))
				return true;
		}
		return false;
	}

}
