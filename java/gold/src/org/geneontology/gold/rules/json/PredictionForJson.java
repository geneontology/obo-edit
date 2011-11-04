package org.geneontology.gold.rules.json;

public class PredictionForJson {

	/**
	 * Gene Annotation row taken from the GAF file
	 */
	private String annotation;
	/**
	 * Hash code for the GAf file. This id is assigned to an HTML element to group annotations of a particular file. 
	 */
	private int fileId;
	private boolean isRedundantWithExistingAnnotations;
	private boolean isRedundantWithOtherPredictions;
	/**
	 * GAF file name
	 */
	private String fileName;

	public PredictionForJson(String annotation,
			boolean isRedundantWithExistingAnnotations,
			boolean isRedundantWithOtherPredictions, String fileName) {
		super();
		this.annotation = annotation;
		this.isRedundantWithExistingAnnotations = isRedundantWithExistingAnnotations;
		this.isRedundantWithOtherPredictions = isRedundantWithOtherPredictions;
		this.fileName = fileName;
		this.fileId = fileName == null ? "null".hashCode() : fileName.hashCode();
	}
	
	
	
}
