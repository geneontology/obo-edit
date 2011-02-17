package org.obo.filters;

import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class AllTextFieldsCriterion extends AbstractStringCriterion {

	protected final static Logger logger = Logger.getLogger(AllTextFieldsCriterion.class);
	protected List keywordCriteria = new LinkedList();

        public AllTextFieldsCriterion() {
          this(false);
        }

	public AllTextFieldsCriterion(boolean excludeObsoletes) {
		super();
		keywordCriteria.add(new NameSynonymSearchCriterion(excludeObsoletes));
		keywordCriteria.add(new IDSearchCriterion(excludeObsoletes, " ID"));
		keywordCriteria.add(new CommentSearchCriterion(excludeObsoletes));
		keywordCriteria.add(new DefinitionSearchCriterion(excludeObsoletes));
		keywordCriteria.add(new DbxrefSearchCriterion(excludeObsoletes));
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
		return "Any text field";
	}
}
