package org.geneontology.gold.rules;

import java.io.IOException;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import org.geneontology.gaf.hibernate.GeneAnnotation;
import org.obolibrary.oboformat.model.FrameMergeException;
import org.semanticweb.owlapi.model.OWLNamedObject;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;

import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.graph.OWLQuantifiedProperty;
import owltools.io.ParserWrapper;

public  class AbstractAnnotationRule implements AnnotationRule {

	private String ruleId;
	
	public Set<AnnotationRuleViolation> getRuleViolations(GeneAnnotation a) {
		throw new Error("not implemented");
	}

	@Override
	public void setRuleId(String ruleId) {
		this.ruleId = ruleId;
		
	}

	@Override
	public String getRuleId() {
		return this.ruleId;
	}

}

