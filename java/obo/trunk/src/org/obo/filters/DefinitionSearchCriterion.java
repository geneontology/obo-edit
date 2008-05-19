package org.obo.filters;

import java.util.Collection;

import org.obo.datamodel.*;

import org.apache.log4j.*;

public class DefinitionSearchCriterion extends AbstractStringCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(DefinitionSearchCriterion.class);

	public static final DefinitionSearchCriterion CRITERION =
		new DefinitionSearchCriterion();

	public Collection getValues(Collection scratch, Object obj) {
		if (obj instanceof DefinedObject)
			scratch.add(((DefinedObject) obj).getDefinition());
		return scratch;
	}
	
	@Override
	public int getMaxCardinality() {
		return 1;
	}
	
	@Override
	public int getMinCardinality() {
		return 0;
	}

	public String getID() {
		return "definition";
	}

	public Class getInputType() {
		return IdentifiedObject.class;
	}

	@Override
	public String toString() {
		return "Definition";
	}
}
