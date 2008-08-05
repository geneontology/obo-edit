package org.obo.reasoner.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.LinkedList;

import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.reasoner.ExplanationType;
import org.obo.util.TermUtil;

import org.apache.log4j.*;

/**
 * An Explanation for a link inferred via an IntersectionRule
 * 
 * @author cjm
 *
 */
public class PropertyIntersectionExplanation extends AbstractExplanation {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(PropertyIntersectionExplanation.class);
	/**
	 * 
	 */
	private static final long serialVersionUID = 6982440827960096291L;

	public PropertyIntersectionExplanation() {
	}
	

	public ExplanationType getExplanationType() {
		return ExplanationType.PROPERTY_INTERSECTION;
	}

	@Override
	public String toString() {
		return "PROPERTY_INTERSECTION: matches=" + getEvidence();
	}
}
