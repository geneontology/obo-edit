package org.geneontology.gold.io;

import owltools.graph.OWLGraphWrapper;

/**
 * This class provides empty implementation of {@link DbOperationsListener}.
 * This provides convenience over the implementations of the {@link DbOperationsListener} interface that 
 * instead of implementing all of its methods just overrides the methods of interest of this class. 
 * @author Shahid Manzoor
 *
 */
public class DbOperationsListenerAdapter implements DbOperationsListener {

	public void bulkLoadStart() {

	}

	public void bulkLoadEnd() {

	}

	public void dumpFilesStart() {

	}

	public void dumpFilesEnd() {

	}

	public void buildSchemaStart() {

	}

	public void buildSchemaEnd() {

	}

	public void loadTsvFilesStart() {

	}

	public void loadTsvFilesEnd() {

	}

	public void updateStart() {

	}

	public void updateEnd() {

	}

	public void startOntologyLoad() {
		
	}

	public void endOntologyLoad(OWLGraphWrapper graph) {
		
	}

}
