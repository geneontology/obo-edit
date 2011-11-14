package org.oboedit.gui.components.ontologyGeneration.interfaces;

import javax.swing.JComponent;

import org.oboedit.gui.components.ontologyGeneration.CandidateTerm;
import org.oboedit.gui.components.ontologyGeneration.DefinitionsTable;
import org.oboedit.gui.components.ontologyGeneration.TermsTable;

/**
 * Interface holding methods of the internal ontology generation tool to allow
 * access from Protege and OBO-Edit
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2010
 * 
 * @param <T>
 *            concept type of the external ontology model
 * @param <R>
 *            relationShipType used in the external ontology model
 */
public interface OntologyGenerationComponentServiceInterface<T extends OntologyClassInterface,R> {

	public abstract AbstractOntologyTermsTable<T,R> getOntologyTermsTable();

	public abstract JComponent buildGUI();

	public abstract DefinitionsTable getDefinitionsTable();

	public abstract String getId();

	public abstract TermsTable getTermsTable();

	public abstract void initClipboardAndWorker();

	public abstract void initListener();

	public abstract void setTextSelectedOntologyTermField(String s);

	public abstract void showProgressDialog(boolean displayDlg, String displayMsg);

	public abstract void updateAllDependedOnSelectedCandidateTerm();

	public abstract void updateOnOntologyTermSelectionChange(CandidateTerm candidateTerm);

	public abstract CandidateTerm getSelectedCandidateTerm();
	
	public abstract void updateEditDefArea(String definition);
	
	public abstract void ontologyTermSelectionChanged();
}
