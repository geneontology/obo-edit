package org.geneontology.gold.rules;

import java.io.File;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import org.apache.log4j.Logger;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gaf.hibernate.GafDocument;
import org.geneontology.gaf.hibernate.GeneAnnotation;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.input.SAXBuilder;
import org.jdom.xpath.XPath;

public class AnnotationRulesEngine {

	private List<AbstractAnnotatioRule> rules;
	
	private static AnnotationRulesEngine engine;
	
	private static Logger LOG = Logger.getLogger(AnnotationRulesEngine.class);
	
	private AnnotationRulesEngine(){
		init();
	//	AbstractAnnotatioRule rule = new AnnotationRuglarExpressionFromXMLRule();
		//rules.add(rule);
		
		
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
	
	
	private  void init(){
		rules = new ArrayList<AbstractAnnotatioRule>();
		SAXBuilder builder = new SAXBuilder();
		Document doc = null;
		try {
			String path = GeneOntologyManager.getInstance().getAnnotationQCFile();
			
			if(!(path.startsWith("http://") || path.startsWith("file:///"))){
				File f = new File(path);
				path = f.toURI().toString();
			}
			URI uri = new URI(path);
			doc = builder.build(uri.toURL());
		} catch (Exception e) {
			LOG.error(e.getMessage(), e);
		}
		
		if(doc == null)
			return;
		
		
		try{
			XPath regexRule = XPath.newInstance("//implementation/script[@language='regex']");			
		    List regexRules = regexRule.selectNodes(doc);
	
		    AbstractAnnotatioRule rule = new AnnotationRuglarExpressionFromXMLRule(regexRules);
		    rules.add(rule);
		}catch(Exception ex){
			LOG.error(ex.getMessage(), ex);
		}
	    
		
		try{
			XPath javaRule = XPath.newInstance("//implementation/script[@language='java']");			
		    Iterator itr = javaRule.selectNodes(doc).iterator();
		    
		    while(itr.hasNext()){
		    	Element script = (Element)itr.next();
		    	String className = script.getAttributeValue("source");
		    	if(className != null){
		    		try{
		    			AbstractAnnotatioRule rule= (AbstractAnnotatioRule) Class.forName(className).newInstance();
		    			rules.add(rule);
		    		}catch(Exception ex){
		    			LOG.error(ex.getMessage(), ex);
		    		}
		    	}
		    }
		}catch(Exception ex){
			LOG.error(ex.getMessage(), ex);
		}
	    
	    

	}
	
	
}
