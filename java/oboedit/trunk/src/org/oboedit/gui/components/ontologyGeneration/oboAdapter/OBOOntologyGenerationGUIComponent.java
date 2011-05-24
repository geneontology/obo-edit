package org.oboedit.gui.components.ontologyGeneration.oboAdapter;

import java.awt.Cursor;

import org.bbop.framework.AbstractGUIComponent;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOProperty;
import org.oboedit.gui.components.ontologyGeneration.OntologyGenerationComponent;
import org.oboedit.gui.components.ontologyGeneration.interfaces.AbstractOntologyTermsTable;
import org.oboedit.gui.components.ontologyGeneration.interfaces.OntologyModelAdapterInterface;

/**
 * Ontology Generation Plugin for OBO-Edit 2 which supports the automatic generation of candidate terms from text
 * retrieved from PubMed, Web, PDF or Text sources. It includes definition creation support for terms.
 * <p>
 * Developed at the Bioinformatics Group, BIOTEC, TU Dresden, Dresden, Germany
 * </p>
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2009
 */
public class OBOOntologyGenerationGUIComponent extends AbstractGUIComponent
{

	private static final long serialVersionUID = 6424353594539158432L;
	final OntologyGenerationComponent<LinkedObject, OBOProperty> innerComponent;
	final OntologyModelAdapterInterface<LinkedObject, OBOProperty> adapter;

	public OBOOntologyGenerationGUIComponent(String id)
	{
		super(id);
		adapter = OBOOntologyModelAdapter.getInstance();
		innerComponent = new OntologyGenerationComponent<LinkedObject,OBOProperty>(adapter, this){

			@Override
			public AbstractOntologyTermsTable<LinkedObject, OBOProperty> createOntologyTermsTable()
			{
				return new OBOTermsTable(new OBOTermsTableModel());
			}};
			
		adapter.setService(innerComponent);
		this.setTitle("Ontology Generation view");
	}

	@Override
	public void init()
	{
		add(innerComponent.buildGUI());
		innerComponent.initListener();
		validate();
		innerComponent.initClipboardAndWorker();
		adapter.refillOntologyTermsTableWithExistingOntologyTerms();
	}

	@Override
	public void cleanup()
	{
		adapter.cleanup();
	}

	@Override
	public void setCursor(Cursor cursor)
	{
		super.setCursor(cursor);
	}

	public void setUpdateUI()
	{
		super.updateUI();
	}

	public void updateEditDefArea()
	{
		innerComponent.updateEditDefArea(null);
	}
}
