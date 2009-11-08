package org.oboedit.gui.components.ontologyGeneration.oboAdapter;

import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.oboedit.gui.components.ontologyGeneration.interfaces.AbstractOntologyTermsTable;
import org.oboedit.gui.components.ontologyGeneration.interfaces.AbstractOntologyTermsTableModel;

public class OBOTermsTable extends AbstractOntologyTermsTable<LinkedObject, OBOProperty>
{

	/**
	 * 
	 */
	private static final long serialVersionUID = -8382894126882103533L;

	public OBOTermsTable(AbstractOntologyTermsTableModel<LinkedObject, OBOProperty> tableModel)
	{
		super(tableModel);
	}

	@Override
	public String nameFor(Object arg1)
	{
		return ((OBOProperty) arg1).getName();
	}

}
