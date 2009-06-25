package org.oboedit.gui.components.ontologyGeneration;

import java.awt.Cursor;

import org.bbop.framework.AbstractGUIComponent;

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
	final OntologyGenerationComponent innerComponent;
	final OntologyModelAdapterInterface adapter;

	public OBOOntologyGenerationGUIComponent(String id)
	{
		super(id);
		adapter = OBOOntologyModelAdapter.getInstance();
		innerComponent = new OntologyGenerationComponent(adapter, this);
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
		adapter.getTermsFromOntologyModel();
		adapter.updateSelectedLinkedObjectAndParents();
	}

	@Override
	public void cleanup()
	{
		adapter.removeListeners();
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

	public void updateEditDefArea(String definition)
	{
		innerComponent.updateEditDefArea(definition);
	}
}
