package org.geneontology.gold.rules;

import java.util.HashSet;
import java.util.Set;

import org.geneontology.gaf.hibernate.GeneAnnotation;
import org.obolibrary.obo2owl.Obo2OWLConstants;
import org.semanticweb.owlapi.model.OWLClass;

import owltools.graph.OWLGraphWrapper;

public class GoClassReferenceAnnotationRule extends AbstractAnnotatioRule {

	
	private OWLGraphWrapper graph;
	
	public GoClassReferenceAnnotationRule(OWLGraphWrapper wrapper){
		this.graph = wrapper;
	}
	
	@Override
	public Set<AnnotationRuleViolation> getRuleViolations(GeneAnnotation a) {

		
		String cls = a.getCls().replace(":", "_");
		
		
		
		OWLClass owlClass= graph.getOWLClass(Obo2OWLConstants.DEFAULT_IRI_PREFIX + cls);
		
		HashSet<AnnotationRuleViolation> set = new HashSet<AnnotationRuleViolation>();
		
		if(owlClass == null){
			set.add(new AnnotationRuleViolation("The GO id in the annotation is a dangling reference", a));
		}
		
		boolean isObsolete = graph.getIsObsolete(owlClass);
		
		if(isObsolete){
			set.add(new AnnotationRuleViolation("The GO id in the annotation is a obsolete class", a));
		}
		
		
		return set;
	}

	
	
}
