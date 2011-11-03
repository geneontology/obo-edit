package org.oboedit.gui.components.ontologyGeneration.oboAdapter;

import org.obo.datamodel.OBOProperty;
import org.oboedit.gui.components.ontologyGeneration.interfaces.AbstractOntologyTermsTable;
import org.oboedit.gui.components.ontologyGeneration.interfaces.AbstractOntologyTermsTableModel;

public class OBOTermsTable extends AbstractOntologyTermsTable<OBOClassWrapper, OBOProperty>
{

	/**
	 * 
	 */
	private static final long serialVersionUID = -8382894126882103533L;

	public OBOTermsTable(AbstractOntologyTermsTableModel<OBOClassWrapper, OBOProperty> tableModel)
	{
		super(tableModel);
	}

	@Override
	public String nameFor(Object object)
	{
		if (object == null)
			return null;
		return ((OBOProperty) object).getName();
	}

}
