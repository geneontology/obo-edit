package org.oboedit.gui.components.ontologyGeneration.factory;

import org.bbop.framework.AbstractComponentFactory;
import org.bbop.framework.AbstractGUIComponent;
import org.oboedit.gui.components.ontologyGeneration.OBOOntologyGenerationGUIComponent;
import org.oboedit.gui.components.ontologyGeneration.OntologyGenerationComponent;


/**
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), Sep 22, 2008
 */
public class OntologyGenerationComponentFactory extends AbstractComponentFactory<AbstractGUIComponent>
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
	public AbstractGUIComponent doCreateComponent(String id)
	{
		OBOOntologyGenerationGUIComponent oboOntologyGenerationGUIComponent = new OBOOntologyGenerationGUIComponent(id);
		return oboOntologyGenerationGUIComponent;
	}

	@Override
	public String getHelpTopicID()
	{
		return ONTOLOGY_GENERATION_VIEW_ID_STRING;
	}
}
