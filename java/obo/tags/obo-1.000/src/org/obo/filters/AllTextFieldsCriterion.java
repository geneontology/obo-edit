package org.obo.filters;

import java.util.*;

import org.obo.datamodel.*;

public class AllTextFieldsCriterion extends AbstractStringCriterion {

	protected List keywordCriteria = new LinkedList();

	public AllTextFieldsCriterion() {
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
			scratch.addAll(scratchList);
		}

		return scratch;
	}

	public String getID() {
		return "all_text_fields";
	}

	public Class getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "All text fields";
	}
}
