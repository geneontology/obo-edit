package org.obo.web;

import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLEncoder;
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
import org.obo.util.TermUtil;

/**
 * @author cjm
 *
 */
import org.apache.log4j.*;

public class DatabaseSearchWrapper {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DatabaseSearchWrapper.class);
	public enum SearchableDatabase {NCBI,GOOGLE,CLINICAL_TRIALS_GOV};
	public enum SearchResultFormat {HTML,XML};
	
	protected SearchableDatabase searchableDatabase;
	protected SearchResultFormat resultFormat;
	protected OBOSession session;

	
	public DatabaseSearchWrapper() {
		super();
		// TODO Auto-generated constructor stub
	}
	
	
	public DatabaseSearchWrapper(OBOSession session) {
		super();
		this.session = session;
	}

	

	public SearchableDatabase getSearchableDatabase() {
		return searchableDatabase;
	}


	public void setSearchableDatabase(SearchableDatabase database) {
		this.searchableDatabase = database;
	}


	public SearchResultFormat getResultFormat() {
		return resultFormat;
	}


	public void setResultFormat(SearchResultFormat resultFormat) {
		this.resultFormat = resultFormat;
	}


	public OBOSession getSession() {
		return session;
	}


	public void setSession(OBOSession session) {
		this.session = session;
	}


	/**
	 * @param term    a word or phrase to be expanded and used; eg "lung cancer"
	 * @return
	 */
	public String expandToSearchURL(String term) {
		Set<String> terms = expandSearchTerm(term);
		String expandedTerm = createDisjunctiveQueryString(terms);
		return createSearchURL(expandedTerm);
	}
	
	/**
	 * @param obj     term to be expanded and used in search
	 * @return
	 */
	public String expandToSearchURL(IdentifiedObject obj) {
		Set<String> terms = expandSearchTerm(obj);
		String expandedTerm = createDisjunctiveQueryString(terms);
		
		String urlStr = createSearchURL(expandedTerm);
		return urlStr;
	}
	
	public  String createSearchURL(String searchTerm) {
		String url = null;
		if (searchableDatabase.equals(SearchableDatabase.CLINICAL_TRIALS_GOV)) {
			url = "http://clinicaltrials.gov/ct2/results?term="+searchTerm;				
		}
		return url;
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
	 * @param label
	 * @return list of labels to be used in searching
	 */
	public Set<String> expandSearchTerm(String label) {
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
	
	public Set<String> expandSearchTerm(Collection<IdentifiedObject> objs) {
		Set<String> terms = new HashSet<String>();
		expandSearchTerm(objs,terms,0);
		return mostGeneral(terms);
	}
	public Set<String> expandSearchTerm(IdentifiedObject obj) {
		return expandSearchTerm(Collections.singleton(obj));
	}
	private void expandSearchTerm(Collection<IdentifiedObject> objs, Set<String> terms, int depth) {
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
	
	private String createDisjunctiveQueryString(Collection<String> terms) {
		StringBuffer sb = new StringBuffer();
		if (searchableDatabase.equals(SearchableDatabase.NCBI))
			sb.append("(");
		int i = 0;
		for (String term : terms) {
			if (i>0) {
				sb.append("+OR+");
			}
			try {
				term = URLEncoder.encode(term, "UTF-8");
			} catch (UnsupportedEncodingException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			// we quote terms for all database flavours
			sb.append("\""+term+"\"");
			if (searchableDatabase.equals(SearchableDatabase.NCBI))
				sb.append("[All Fields]");
			else if (searchableDatabase.equals(SearchableDatabase.CLINICAL_TRIALS_GOV))
				sb.append("[ALL-FIELDS]");
			i++;
		}
		if (searchableDatabase.equals(SearchableDatabase.NCBI))
			sb.append(")");
		return sb.toString();
	}


	/**
	 * TODO: improve algorithm
	 * Should tRNA subsume stRNA? Use token based rather than character based sumsumption?
	 */
	private Set<String> mostGeneral(Set<String> terms) {
		HashSet<String> gterms = new HashSet<String>();
		// O(N^2) -- however, we do not expect massive term lists
		for (String s1 : terms) {
			boolean isGeneral = true;
			for (String s2 : terms) {
				if (s1.equals(s2))
					continue;
				if (s1.contains(s2)) {
					isGeneral = false;
					//logger.info("excl:"+s1);
					break;
				}
			}
			if (isGeneral)
				gterms.add(s1);
		}
		return gterms;
	}
	
	/*
	 * NOTES:
	 * 
	 * AND queries plus quotes on ct.gov:
	 * http://clinicaltrials.gov/ct2/results?term=%22induced+apoptosis%22+AND+%22cervical+cancer%22&recr=&type=&cond=&intr=&spons=&id=&state1=&cntry1=&state2=&cntry2=&state3=&cntry3=&locn=&rcv_s=&rcv_e=
	 * 
	 * 
	 */
	
	// TODO: this isn't much good unless we load ontologies...
	public static void main(String[] args) {
		SearchableDatabase qs = SearchableDatabase.NCBI;
		Collection<String> terms = new LinkedList<String>();
		for (int i=0; i<args.length; i++) {
			if (args[i].equals("-resultFormat")) {
				i++;
				String resultFormat = args[i];
				if (resultFormat.contains("trial"))
					qs = SearchableDatabase.CLINICAL_TRIALS_GOV;
				else if (resultFormat.contains("google"))
					qs = SearchableDatabase.GOOGLE;
				else
					qs = SearchableDatabase.NCBI;
			}
			else {
				terms.add(args[i]);
			}
		}
		
		//String searchTerm = createQueryString(qs,terms);
		//logger.info(searchTerm);
	}
}
