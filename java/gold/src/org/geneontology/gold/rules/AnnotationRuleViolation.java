package org.geneontology.gold.rules;

import java.util.Set;

import org.geneontology.gaf.hibernate.GeneAnnotation;

public class AnnotationRuleViolation {

	private final String message;
	private Set<GeneAnnotation> suggestedReplacements;
	private GeneAnnotation sourceAnnotation;
	private String annotationRow;
	
	private String ruleId;
	
	private int lineNumber;
	
	
	public String getRuleId() {
		return ruleId;
	}


	public void setRuleId(String ruleId) {
		this.ruleId = ruleId;
	}


	public AnnotationRuleViolation(String msg) {
		message = msg;
	}
	
	
	public AnnotationRuleViolation(String message,
			GeneAnnotation sourceAnnotation) {
		super();
		this.message = message;
		this.sourceAnnotation = sourceAnnotation;
		this.annotationRow = sourceAnnotation != null ? sourceAnnotation.toString() : null;
	}


	public AnnotationRuleViolation(String message, String annotationRow) {
		super();
		this.message = message;
		this.annotationRow = annotationRow;
	}
	
	
	public String getMessage() {
		return message;
	}


	public Set<GeneAnnotation> getSuggestedReplacements() {
		return suggestedReplacements;
	}


	public void setSuggestedReplacements(Set<GeneAnnotation> suggestedReplacements) {
		this.suggestedReplacements = suggestedReplacements;
	}


	public GeneAnnotation getSourceAnnotation() {
		return sourceAnnotation;
	}


	public void setSourceAnnotation(GeneAnnotation sourceAnnotation) {
		this.sourceAnnotation = sourceAnnotation;
		this.annotationRow = sourceAnnotation != null ? sourceAnnotation.toString() : null;

	}


	public String getAnnotationRow() {
		return annotationRow;
	}


	public void setAnnotationRow(String annotationRow) {
		this.annotationRow = annotationRow;
	}


	public int getLineNumber() {
		return lineNumber;
	}


	public void setLineNumber(int lineNumber) {
		this.lineNumber = lineNumber;
	}
	

	

}
