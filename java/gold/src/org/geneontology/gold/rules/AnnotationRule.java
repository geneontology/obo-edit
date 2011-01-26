package org.geneontology.gold.rules;

import java.util.Set;

import org.geneontology.gaf.hibernate.GeneAnnotation;

public interface AnnotationRule {
	
	public Set<RuleViolation> getRuleViolations(GeneAnnotation a);	

}
