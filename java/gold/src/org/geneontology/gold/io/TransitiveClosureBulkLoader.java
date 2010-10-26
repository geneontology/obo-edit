package org.geneontology.gold.io;

import owltools.graph.OWLGraphWrapper;

/**
 * loads graph closure into database. This includes
 * 
 * - all paths from A to B
 * - relations in that path
 * - distances
 * 
 */
public class TransitiveClosureBulkLoader {

	/**
	 * Ontologies are accessed via GraphWrappers
	 */
	private OWLGraphWrapper graphWrapper;

}
