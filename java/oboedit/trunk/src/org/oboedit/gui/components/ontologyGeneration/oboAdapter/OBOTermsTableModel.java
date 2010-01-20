package org.oboedit.gui.components.ontologyGeneration.oboAdapter;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

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
	public String getTermId(LinkedObject linkedObject)
	{
		if (linkedObject == null) {
			return null;
		}
		return linkedObject.getID();
	}

	@Override
	public String getTermName(LinkedObject linkedObject)
	{
		if (linkedObject == null) {
			return null;
		}
		if (isFoundBySynonym(linkedObject)) {
			StringBuilder builder = new StringBuilder();
			builder.append("<html>");
			builder.append(linkedObject.getName());
			builder.append(" (<i>synonym</i>: <b>");
			builder.append(getSynonymNameMatch(linkedObject));
			builder.append("</b> )");
			builder.append("</html>");
			return builder.toString();
		}
		return linkedObject.getName();
	}

	@Override
	public Collection<String> getSynonymNames(LinkedObject linkedObject)
	{
		if (linkedObject == null) {
			return Collections.emptyList();
		}
		List<String> synonymNames = OBOOntologyModelAdapter.getInstance().getSynonymsForOntologyTerm(linkedObject);

		if (synonymNames == null || synonymNames.isEmpty()) {
			return Collections.emptyList();
		}
		return synonymNames;
	}

}
