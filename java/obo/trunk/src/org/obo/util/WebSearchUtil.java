package org.obo.util;

import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.Set;

import org.bbop.util.StringUtil;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBOSession;
import org.obo.filters.EqualsComparison;
import org.obo.filters.ObjectFilter;
import org.obo.filters.ObjectFilterFactory;
import org.obo.filters.SearchComparison;

/**
 * @author cjm
 *
 */
public class WebSearchUtil {
	public enum QueryStyle {NCBI,GOOGLE};
	
	public static String createQueryString(QueryStyle style, Collection<String> terms) {
		StringBuffer sb = new StringBuffer();
		if (style.equals(QueryStyle.NCBI))
			sb.append("(");
		int i = 0;
		for (String term : terms) {
			if (i>0) {
				sb.append(" OR ");
			}
			sb.append("\""+term+"\"");
			if (style.equals(QueryStyle.NCBI))
				sb.append("[All Fields]");
			i++;
		}
		if (style.equals(QueryStyle.NCBI))
			sb.append(")");
		return sb.toString();
	}
	/**
	 * 
	 * creates a list of search terms from ontologies in memory to be used in
	 * non-ontology aware search tools in a disjunctive (OR) query
	 * 
	 * for example "programmed cell death" should expand to "apoptosis"...
	 * "programmed cell death, B-cells" would NOT be included, as anything in
	 * this set is already included in the PCD set (assuming the external search engine does
	 * not do its own expansion of course)
	 * 
	 * @param session
	 * @param label
	 * @return list of labels to be used in searching
	 */
	public static Set<String> expandSearchTerm(OBOSession session, String label) {
		ObjectFilterFactory off = new ObjectFilterFactory();
		ObjectFilter filter = (ObjectFilter)off.createNewFilter();

		SearchComparison comparison = new EqualsComparison();
		filter.setComparison(comparison);
		filter.setValue(label); // TODO: escape

		Collection<IdentifiedObject> objs = new LinkedList<IdentifiedObject>();
		for (IdentifiedObject io : session.getObjects()) {
			if (filter.satisfies(io))
				objs.add(io);
		}
		return expandSearchTerm(objs);
	}
	
	public static Set<String> expandSearchTerm(Collection<IdentifiedObject> objs) {
		Set<String> terms = new HashSet<String>();
		expandSearchTerm(objs,terms,0);
		return mostGeneral(terms);
	}
	public static Set<String> expandSearchTerm(IdentifiedObject obj) {
		return expandSearchTerm(Collections.singleton(obj));
	}
	private static void expandSearchTerm(Collection<IdentifiedObject> objs, Set<String> terms, int depth) {
		if (objs.size() == 0)
			return;
		Collection<IdentifiedObject> nextObjs = new LinkedList<IdentifiedObject>();
		for (IdentifiedObject io : objs) {
			
			if (io instanceof LinkedObject) {
				Collection<String> labels = TermUtil.getLabels(io);
				terms.addAll(labels);
				for (Link link : ((LinkedObject)io).getChildren()) {
					if (link.getType().equals(OBOProperty.IS_A))
						nextObjs.add(link.getChild());
				}
			}
		}
		expandSearchTerm(nextObjs, terms,depth+1); // not loop safe - TODO
	}
	

	/*
	 * TODO: improve algorithm
	 * Should tRNA subsume stRNA? Use token based rather than character based sumsumption?
	 */
	private static Set<String> mostGeneral(Set<String> terms) {
		HashSet<String> gterms = new HashSet<String>();
		// O(N^2) -- however, we do not expect massive term lists
		for (String s1 : terms) {
			boolean isGeneral = true;
			for (String s2 : terms) {
				if (s1.equals(s2))
					continue;
				if (s1.contains(s2)) {
					isGeneral = false;
					//System.out.println("excl:"+s1);
					break;
				}
			}
			if (isGeneral)
				gterms.add(s1);
		}
		return gterms;
	}
}
