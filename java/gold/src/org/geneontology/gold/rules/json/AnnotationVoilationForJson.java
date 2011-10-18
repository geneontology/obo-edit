package org.geneontology.gold.rules.json;

public class AnnotationVoilationForJson {

	private String message;
	private String annotation;
	private String ruleId;
	private int lineNumber;
	private String fileName;
	private int fileId;
	
	public AnnotationVoilationForJson(String message, String annotation,
			String ruleId, int lineNumber, String fileName) {
		super();
		this.message = message;
		this.annotation = annotation;
		this.ruleId = ruleId;
		this.lineNumber = lineNumber;
		this.fileName = fileName;
		this.fileId = fileName == null ? "null".hashCode() : fileName.hashCode();
	}
	
	
	
	
}
