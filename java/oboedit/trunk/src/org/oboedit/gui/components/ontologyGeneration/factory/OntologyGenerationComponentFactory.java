package org.oboedit.gui.components.ontologyGeneration.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.oboedit.gui.components.ontologyGeneration.OntologyGenerationComponent;


/**
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), Sep 22, 2008
 */
public class OntologyGenerationComponentFactory extends AbstractComponentFactory<OntologyGenerationComponent>
{

    private static final String ONTOLOGY_GENERATION_VIEW_ID_STRING = "ONTOLOGY_GENERATION_VIEW";

    /**
     * Constructs a {@link OntologyGenerationComponentFactory}
     */
	public OntologyGenerationComponentFactory()
	{
	}

	public String getID()
	{
		return ONTOLOGY_GENERATION_VIEW_ID_STRING;
	}


	public FactoryCategory getCategory()
	{
		return FactoryCategory.TOOLS;
	}

	public String getName()
	{
		return "Ontology Generation Tool";
	}

	
	@Override
	public OntologyGenerationComponent doCreateComponent(String id)
	{
		return new OntologyGenerationComponent(id);
	}

	@Override
	public String getHelpTopicID()
	{
		return ONTOLOGY_GENERATION_VIEW_ID_STRING;
	}
}
