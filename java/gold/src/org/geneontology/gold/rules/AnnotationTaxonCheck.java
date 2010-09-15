package org.geneontology.gold.rules;

import java.util.Set;

import org.semanticweb.owlapi.model.OWLObject;

import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;

public class AnnotationTaxonCheck {
	OWLGraphWrapper graphWrapper;
	
	public AnnotationTaxonCheck(OWLGraphWrapper graphWrapper) {
		super();
		this.graphWrapper = graphWrapper;
	}
	
	public AnnotationTaxonCheck(String annotationOntolPath,
			String taxonOntolPath, String constraintsOntolPath) {
		super();
		this.graphWrapper = new OWLGraphWrapper();
		
	}


	public boolean check(String annotationCls, String taxonCls) {
		OWLObject cls = graphWrapper.getOWLObjectByIdentifier(annotationCls);
		OWLObject tax = graphWrapper.getOWLObjectByIdentifier(annotationCls);
		OWLObject rNever = graphWrapper.getOWLObjectByIdentifier("never_in_taxon");
		OWLObject rOnly = graphWrapper.getOWLObjectByIdentifier("only_in_taxon");
		Set<OWLGraphEdge> edges = graphWrapper.getEdgesBetween(cls, tax);
		boolean ok = true;
		for (OWLGraphEdge ge : edges) {
			
		}
		return ok;
		
	}
	
	public boolean check(String annotationCls, int ncbiTaxonId) {
		return check(annotationCls, "NCBITaxon:"+ncbiTaxonId);
	}
}
