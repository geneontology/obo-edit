package org.obo.filters;

import java.util.*;

import org.obo.datamodel.*;

public class KeywordSearchCriterion extends AbstractStringCriterion {

	protected List keywordCriteria = new LinkedList();

	/*
	 * protected static final delimChars = { '\t', ' ', '-', ',', '+', '_', '.',
	 * '!', '?', '\'' '"', '(', ')', '{', '}', '[]<>;/ };
	 */
	public KeywordSearchCriterion() {
		super();
		keywordCriteria.add(new NameSynonymSearchCriterion());
		keywordCriteria.add(new IDSearchCriterion());
		keywordCriteria.add(new CommentSearchCriterion());
		keywordCriteria.add(new DefinitionSearchCriterion());
		keywordCriteria.add(new DbxrefSearchCriterion());
	}

	public Collection getValues(Collection scratch, Object obj) {
		Iterator it = keywordCriteria.iterator();

		while (it.hasNext()) {
			SearchCriterion sc = (SearchCriterion) it.next();
			List scratchList = new LinkedList();
			sc.getValues(scratchList, obj);
			Iterator it2 = scratchList.iterator();
			while (it2.hasNext()) {
				String s = it2.next().toString();
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
