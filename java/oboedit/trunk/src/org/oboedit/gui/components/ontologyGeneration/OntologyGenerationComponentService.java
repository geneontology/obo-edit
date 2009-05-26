package org.oboedit.gui.components.ontologyGeneration;

import javax.swing.JComponent;

public interface OntologyGenerationComponentService {
	
	public abstract CandidateTerm getSelectedCandidateTerm();
	public abstract DefinitionsTable getDefinitionsTable();
	public abstract OBOTermsTable getOboTermsTable();
	public abstract TermsTable getTermsTable();
	public abstract void updateInputFieldsForSelectedLinkedObjectLabel(String label);
	public abstract void setTextSelectedLinkedObjectField(String s);
	public abstract String getId();
	public abstract JComponent buildGUI();
	public abstract void initListener();
	public abstract void initClipboardAndWorker();
}
