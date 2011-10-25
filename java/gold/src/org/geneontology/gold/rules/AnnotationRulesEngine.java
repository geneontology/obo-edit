package org.geneontology.gold.rules;

import java.util.Collections;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Set;
import org.apache.log4j.Logger;
import org.geneontology.gaf.hibernate.GafDocument;

import owltools.gaf.GeneAnnotation;

public class AnnotationRulesEngine {

///	private List<AbstractAnnotationRule> rules;
	
//	private static AnnotationRulesEngine engine;
	
	private static Logger LOG = Logger.getLogger(AnnotationRulesEngine.class);
	
//	private Exception initException;
	
	//if this variable is set to rue then stop the annotation checks
	private Hashtable<String, Set<AnnotationRuleViolation>> annotationRuleViolations;
	private Hashtable<String, Integer> annotationRuleViolationsCounter;
	
	private int annotationVoilationLimit;

	
	public AnnotationRulesEngine(){
		this(-1);
	}
	
	
	public AnnotationRulesEngine(int annotationVoilationLimit){
	//	AbstractAnnotatioRule rule = new AnnotationRuglarExpressionFromXMLRule();
		//rules.add(rule);
		annotationRuleViolations = new Hashtable<String, Set<AnnotationRuleViolation>>();
		annotationRuleViolationsCounter = new Hashtable<String, Integer>();
		this.annotationVoilationLimit = annotationVoilationLimit;
		
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
	
	/*public static AnnotationRulesEngine getInstance(){
		if(engine == null){
			engine = new AnnotationRulesEngine();
		}
		
		return engine;
	}
	
	public List<AbstractAnnotationRule> getRules(){
		return rules;
	}
	
	public void stopAnnotaitonChecks(){
		this.stopAnnotaitonChecks = true;
	}*/
	
	
	public Hashtable<String, Set<AnnotationRuleViolation>> validateAnnotations(GafDocument doc) throws AnnotationRuleCheckException{
		
//		this.stopAnnotaitonChecks = false;
		AnnotationRulesFactory rulesFactory = AnnotationRulesFactory.getInstance();
		List<AnnotationRule> rules = rulesFactory.getRules();
		if(rules == null || rules.isEmpty()){
			throw new AnnotationRuleCheckException("Rules are not initialized. Please check the annotation_qc.xml file for errors and restart the server");
		}
		
		//HashSet<AnnotationRuleViolation> set = new HashSet<AnnotationRuleViolation>();
		try{
		
			HashSet<String> rulesNotToRun = new HashSet<String>();
			for(GeneAnnotation annotation: doc.getGeneAnnotations()){
				for(AnnotationRule rule: rules){
					
					if(rulesNotToRun.contains(rule.getRuleId()))
						continue;
					
					for(AnnotationRuleViolation av: rule.getRuleViolations((org.geneontology.gaf.hibernate.GeneAnnotation)annotation)){
						Set<AnnotationRuleViolation> setV= annotationRuleViolations.get(av.getRuleId());
						Integer counter = annotationRuleViolationsCounter.get(av.getRuleId());
						if(setV == null){
							setV = new HashSet<AnnotationRuleViolation>();
							setV = Collections.synchronizedSet(setV);
							annotationRuleViolations.put(av.getRuleId(), setV);
							counter = 0;
						}
						
						if(annotationVoilationLimit != -1 && counter>=annotationVoilationLimit)
							rulesNotToRun.add(rule.getRuleId());
						else	
							setV.add(av);
						
						annotationRuleViolationsCounter.put(av.getRuleId(), counter+1);
	
						
						
//						annotationVoilationLimitReached = annotationVoilationLimit != -1 || setV.size()>annotationVoilationLimit;
					}
					
				}
			}
		}catch(Exception ex){
			LOG.error(ex.getMessage(), ex);
			throw new RuntimeException(ex);
		}
		
		return annotationRuleViolations;
	}
	
	
	
	
}
