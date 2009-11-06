package org.oboedit.gui.components.ontologyGeneration.oboAdapter;

import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.oboedit.gui.components.ontologyGeneration.interfaces.AbstractOntologyTermsTableModel;

public class OBOTermsTableModel extends AbstractOntologyTermsTableModel<LinkedObject>
{
	private static final long serialVersionUID = 2327784875383421301L;

	@Override
	public Object[] getDefaultRelationTypes()
	{
		return OBOProperty.BUILTIN_TYPES;
	}

	@Override
	public Object getPreselectedRelationType()
	{
		return OBOProperty.IS_A;
	}

	@Override
	public String getTermId(LinkedObject term)
	{
		return term.getID();
	}

	@Override
	public String getTermName(LinkedObject term)
	{
		return term.getName();
	}

}
