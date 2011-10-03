package org.geneontology.gold.rules;

/**
 * This excpception is thrown when an exception occurs during the execution of annotation rules.
 * @author Shahid Manzoor
 *
 */
public class AnnotationRuleCheckException extends Exception {

	public AnnotationRuleCheckException() {
	}

	public AnnotationRuleCheckException(String message) {
		super(message);
	}

	public AnnotationRuleCheckException(Throwable cause) {
		super(cause);
	}

	public AnnotationRuleCheckException(String message, Throwable cause) {
		super(message, cause);
	}

}
