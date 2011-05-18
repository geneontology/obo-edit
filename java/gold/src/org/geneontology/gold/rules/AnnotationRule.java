package org.geneontology.gold.rules;

import java.util.Set;

import org.geneontology.gaf.hibernate.GeneAnnotation;

public interface AnnotationRule {
	
	/**
	 * Given an annotation, find the list of violations using the rule
	 * @param a
	 * @return
	 */
	public Set<AnnotationRuleViolation> getRuleViolations(GeneAnnotation a);	

	public void setRuleId(String ruleId);
	public String getRuleId();
	
}
