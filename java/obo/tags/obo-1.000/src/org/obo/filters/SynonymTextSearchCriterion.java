package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.Synonym;

public class SynonymTextSearchCriterion extends AbstractCriterion<Synonym, String> {

	public static final SynonymTextSearchCriterion CRITERION = new SynonymTextSearchCriterion();
	
	public String getID() {
		return "synonym text";
	}

	public Class<Synonym> getInputType() {
		return Synonym.class;
	}

	public Class<String> getReturnType() {
		return String.class;
	}
	
	@Override
	public int getMaxCardinality() {
		return 1;
	}

	@Override
	public int getMinCardinality() {
		return 1;
	}

	public Collection<String> getValues(Collection<String> scratch, Synonym obj) {
		scratch.add(obj.getText());
		return scratch;
	}
	
	public String toString() {
		return "Synonym text";
	}

}
