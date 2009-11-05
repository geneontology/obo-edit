package org.oboedit.gui.components.ontologyGeneration.interfaces;

import javax.swing.JComponent;

import org.oboedit.gui.components.ontologyGeneration.DefinitionsTable;
import org.oboedit.gui.components.ontologyGeneration.TermsTable;
import org.oboedit.gui.components.ontologyGeneration.oboAdapter.OBOTermsTable;

public interface OntologyGenerationComponentServiceInterface {

	public abstract JComponent buildGUI();

	public abstract DefinitionsTable getDefinitionsTable();

	public abstract String getId();

	public abstract OBOTermsTable getOboTermsTable();

	public abstract TermsTable getTermsTable();

	public abstract void initClipboardAndWorker();

	public abstract void initListener();

	public abstract void setTextSelectedLinkedObjectField(String s);

	public abstract void showProgressDlg(boolean displayDlg, String displayMsg);

	public abstract void updateAllDependedOnSelectedTerm();
	
	public abstract void updateInputFieldsForSelectedLinkedObjectLabel(String label);
}
