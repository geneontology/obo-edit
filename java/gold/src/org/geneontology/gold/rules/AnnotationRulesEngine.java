package org.geneontology.gold.rules;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.log4j.Logger;
import org.geneontology.gaf.hibernate.GafDocument;
import org.geneontology.gaf.hibernate.GeneAnnotation;

public class AnnotationRulesEngine {

	private List<AbstractAnnotatioRule> rules;
	
	private static AnnotationRulesEngine engine;
	
	private static Logger LOG = Logger.getLogger(AnnotationRulesEngine.class);
	
	private AnnotationRulesEngine(){
		rules = new ArrayList<AbstractAnnotatioRule>();
		AbstractAnnotatioRule rule = new AnnotationRuglarExpressionFromXMLRule();
		rules.add(rule);
		
		
		/*try{
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
		rules.add(rule);*/
	}
	
	public static AnnotationRulesEngine getInstance(){
		if(engine == null){
			engine = new AnnotationRulesEngine();
		}
		
		return engine;
	}
	
	public List<AbstractAnnotatioRule> getRules(){
		return rules;
	}
	
	
	public Set<AnnotationRuleViolation> validateAnnotations(GafDocument doc){
		HashSet<AnnotationRuleViolation> set = new HashSet<AnnotationRuleViolation>();
		
		try{
		
			for(GeneAnnotation annotation: doc.getGeneAnnotations()){
				for(AbstractAnnotatioRule rule: rules){
					set.addAll( rule.getRuleViolations(annotation) );
				}
			}
		}catch(Exception ex){
			LOG.error(ex.getMessage(), ex);
		}
		
		return set;
	}
	
}
