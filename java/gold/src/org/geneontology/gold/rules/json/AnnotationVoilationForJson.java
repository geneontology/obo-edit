package org.geneontology.gold.rules.json;

public class AnnotationVoilationForJson {

	/**
	 * refer to the error message
	 */
	private String message;
	/**
	 * refer to gene annotation row
	 */
	private String annotation;
	/**
	 * The rule which is voilated by the annotation
	 */
	private String ruleId;
	/**
	 * The line number of annotation in the GAF file
	 */
	private int lineNumber;
	/**
	 * The gaf file name
	 */
	private String fileName;
	/**
	 * Hash code for the GAf file. This id is assigned to an HTML element to group annotations of a particular file. 
	 */
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
