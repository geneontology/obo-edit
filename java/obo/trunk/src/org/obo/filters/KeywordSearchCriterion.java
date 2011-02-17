package org.obo.filters;

import java.util.*;

import org.obo.datamodel.*;

import org.apache.log4j.*;

/** I think this class is obsolete--it now does the same thing as AllTextFieldsCriterion. */

public class KeywordSearchCriterion extends AbstractStringCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(KeywordSearchCriterion.class);

	protected List keywordCriteria = new LinkedList();

	/*
	 * protected static final delimChars = { '\t', ' ', '-', ',', '+', '_', '.',
	 * '!', '?', '\'' '"', '(', ')', '{', '}', '[]<>;/ };
	 */

        public KeywordSearchCriterion() {
          this(false);
        }

	public KeywordSearchCriterion(boolean excludeObsoletes) {
		super();
		keywordCriteria.add(new NameSynonymSearchCriterion(excludeObsoletes));
		keywordCriteria.add(new IDSearchCriterion(excludeObsoletes));
		keywordCriteria.add(new CommentSearchCriterion(excludeObsoletes));
		keywordCriteria.add(new DefinitionSearchCriterion(excludeObsoletes));
		keywordCriteria.add(new DbxrefSearchCriterion(excludeObsoletes));
	}

	public Collection getValues(Collection scratch, Object obj) {
		
		for(Object kc : keywordCriteria){
			SearchCriterion sc = (SearchCriterion) kc;
			List scratchList = new LinkedList();
			sc.getValues(scratchList, obj);
			for(Object sl : scratchList){
				String s = sl.toString();
				// extractKeywords(scratch, s);
				scratch.add(s);
			}
		}
		return scratch;
	}

	public String getID() {
		return "keyword";
	}

	public static void extractKeywords(Collection c, String s) {
		if (s == null)
			return;
		StringTokenizer tokenizer = new StringTokenizer(s,
				"\t -,+_.!?'\"(){}[]<>;/");
		while (tokenizer.hasMoreTokens()) {
			String token = tokenizer.nextToken();
			if (token.length() > 0)
				c.add(token);
		}
	}

	public Class getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "Keyword";
	}
}
