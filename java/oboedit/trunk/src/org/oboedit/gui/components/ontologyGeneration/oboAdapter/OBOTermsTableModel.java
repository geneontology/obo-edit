package org.oboedit.gui.components.ontologyGeneration.oboAdapter;

import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.oboedit.gui.components.ontologyGeneration.interfaces.AbstractOntologyTermsTableModel;

public class OBOTermsTableModel extends AbstractOntologyTermsTableModel<LinkedObject, OBOProperty>
{
	private static final long serialVersionUID = 2327784875383421301L;

	@Override
	public OBOProperty[] getDefaultRelationTypes()
	{
		return OBOProperty.BUILTIN_TYPES;
	}

	@Override
	public OBOProperty getDefaultRelationType()
	{
		return OBOProperty.IS_A;
	}

	@Override
	public String getTermId(LinkedObject term)
	{
		if (term == null) {
			return null;
		}
		return term.getID();
	}

	@Override
	public String getTermName(LinkedObject term)
	{
		if (term == null) {
			return null;
		}
		return term.getName();
	}

}
