package org.obo.filters;

import org.obo.datamodel.*;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

public class IsTransitiveCriterion extends AbstractBooleanCriterion {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IsTransitiveCriterion.class);

	public String getID() {
		return "is_transitive";
	}

	public boolean matches(IdentifiedObject o) {
		return TermUtil.isProperty(o)
				&& ((OBOProperty) o).isTransitive();
	}

	@Override
	public Class getInputType() {
		return OBOProperty.class;
	}

	@Override
	public String toString() {
		return "Is Transitive";
	}
}
