package org.geneontology.gold.io;

import java.io.IOException;

import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;

import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;

/**
 * loads graph closure into database. This includes
 * 
 * - all paths from A to B
 * - relations in that path
 * - distances
 * 
 */
public class TransitiveClosureBulkLoader extends AbstractBulkLoader{

	public TransitiveClosureBulkLoader(OWLGraphWrapper wrapper) {
		super(wrapper);
	}

	public TransitiveClosureBulkLoader(OWLGraphWrapper wrapper, String path) {
		super(wrapper, path);
	}
	
	
	/**
	 * Ontologies are accessed via GraphWrappers
	 */


	public void dumpBulkLoadTables() throws IOException{
		
		
		TableDumper dumper = new TableDumper("inferred_relationship", this.path);
	//	TableDumper subclass_ofDumper = new TableDumper("subclass_of");
		//TableDumper allSomeRelationship = new TableDumper("all_some_relationship");
		
		
		for (OWLClass cls : getOwlOntology().getClassesInSignature()) {
			String id = graphWrapper.getIdentifier(cls);
			for (OWLGraphEdge ge : graphWrapper.getOutgoingEdgesClosure(cls)) {
				String pid = ge.getSingleQuantifiedProperty().getPropertyId();
				boolean isDirect = false; // todo
				boolean isReflexive = pid.equals(id);
				dumper.dumpRow(id, ge.getTargetId(), pid, isDirect ? "t" : "f", isReflexive ? "t" : "f");
			}
		}
	}
}
