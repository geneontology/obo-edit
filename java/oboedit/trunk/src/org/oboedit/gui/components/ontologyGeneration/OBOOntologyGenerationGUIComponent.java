package org.oboedit.gui.components.ontologyGeneration;

import java.awt.Cursor;

import org.bbop.framework.AbstractGUIComponent;

public class OBOOntologyGenerationGUIComponent extends AbstractGUIComponent {

	/**
	 * 
	 */
	private static final long serialVersionUID = 6424353594539158432L;
	final OntologyGenerationComponent innerComponent;
	final OBOOntologyModelAdapterInterface adapter;
	
	public OBOOntologyGenerationGUIComponent(String id) {
		super(id);
		adapter = new OBOOntologyModelAdapter();
		innerComponent = new OntologyGenerationComponent(adapter, this);
		adapter.setService(innerComponent);
		this.setTitle("Ontology Generation view");
	}

	@Override
	public void init() {
		add(innerComponent.buildGUI());
		innerComponent.initListener();
		validate();
		innerComponent.initClipboardAndWorker();
		adapter.getTermsFromOntologyModel();
		adapter.updateSelectedLinkedObjectAndParents();
	}
	
	@Override
	public void cleanup() {
		adapter.removeListeners();
	}
	
	@Override
	public void setCursor(Cursor cursor) {
		super.setCursor(cursor);
	}

	public void setUpdateUI() {
		super.updateUI();
	}

	public void updateEditDefArea(String definition) {
		innerComponent.updateEditDefArea(definition);
	}
}
