package org.geneontology.gold.rules;

import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.Logger;

public class AnnotationRulesEngine {

	private List<AbstractAnnotatioRule> rules;
	
	private static AnnotationRulesEngine engine;
	
	private static Logger LOG = Logger.getLogger(AnnotationRulesEngine.class);
	
	private AnnotationRulesEngine(){
		rules = new ArrayList<AbstractAnnotatioRule>();
		AbstractAnnotatioRule rule = new AnnotationRuglarExpressionFromXMLRule();
		rules.add(rule);
		
		
		try{
			rule  = new AnnotationTaxonRule();
			rules.add(rule);
		}catch(Exception ex){
			LOG.error(ex.getMessage(), ex);
		}
		
		rule = new CardinalityCheckRule();
		rules.add(rule);
		
		rule = new DbAbbreviationsRule();
		rules.add(rule);
		
		rule = new GoClassReferenceAnnotationRule();
		rules.add(rule);
	}
	
	public static AnnotationRulesEngine getInstance(){
		if(engine == null){
			engine = new AnnotationRulesEngine();
		}
		
		return engine;
	}
}
