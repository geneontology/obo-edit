package org.oboedit.gui.components.ontologyGeneration.interfaces;

import javax.swing.JComponent;

import org.oboedit.gui.components.ontologyGeneration.DefinitionsTable;
import org.oboedit.gui.components.ontologyGeneration.TermsTable;

public interface OntologyGenerationComponentServiceInterface<T,R> {

	public abstract AbstractOntologyTermsTable<T,R> getOntologyTermsTable();

	public abstract JComponent buildGUI();

	public abstract DefinitionsTable getDefinitionsTable();

	public abstract String getId();

	public abstract TermsTable getTermsTable();

	public abstract void initClipboardAndWorker();

	public abstract void initListener();

	public abstract void setTextSelectedLinkedObjectField(String s);

	public abstract void showProgressDlg(boolean displayDlg, String displayMsg);

	public abstract void updateAllDependedOnSelectedTerm();
	
	public abstract void updateInputFieldsForSelectedLinkedObjectLabel(String label);
}
