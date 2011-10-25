package org.geneontology.gold.rules;

import java.io.File;
import java.net.URI;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.regex.Pattern;

import org.apache.log4j.Logger;
import org.geneontology.conf.GoConfigManager;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.input.SAXBuilder;
import org.jdom.xpath.XPath;


/**
 * This class reads the annotation_qc.xml file and builds {@link AnnotationRule} objects from the qc file.
 * @author shahidmanzoor
 *
 */
public class AnnotationRulesFactory {

	private List<AnnotationRule> rules;

	private static AnnotationRulesFactory factory;
	
	private static Logger LOG = Logger.getLogger(AnnotationRulesFactory.class);
	
	private AnnotationRulesFactory(){
		init();
	}
	
	
	public static AnnotationRulesFactory getInstance(){
		if(factory == null){
			synchronized (LOG) {
				factory = new AnnotationRulesFactory();
				LOG.notifyAll();
			}
		}
		
		return factory;
	}
	
	
	private  void init(){
		rules = new ArrayList<AnnotationRule>();
		SAXBuilder builder = new SAXBuilder();
		Document doc = null;
		try {
			String path = GoConfigManager.getInstance().getAnnotationQCFile();
			
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
	
		   
		    for(Object obj: regexRules){
		    	Element script = (Element) obj;
		    	
		    	String regex = script.getTextNormalize();
		    	boolean isCaseInsensitive = regex.endsWith("/i");
		    	
		    	//java doesn't like the /^ switch so it is replaced by ^
		    	regex = regex.replace("/^", "^");
		    	//java does not support the /i case in-sensitivity switch so it is removed
		    	regex = regex.replace("/i", "");
		    	
		    	//title is child of the rule element
		    	Element title= script.getParentElement().getParentElement().getParentElement().getChild("title");
		    	
		    	
		    	String titleString = "";
		    	if(title != null){
		    		titleString = title.getTextNormalize();
		    	}
		    	
		    	Element idElement = title.getParentElement().getChild("id");
		    	String id = "";
		    	if(idElement != null){
		    		id = idElement.getTextNormalize();
		    	}
		    	
		    	Pattern pattern = null;
		    	
		    	if(isCaseInsensitive)
		    		pattern = Pattern.compile(regex, Pattern.CASE_INSENSITIVE);
		    	else
		    		pattern = Pattern.compile(regex);
		    	
		    	
		    	AnnotationRuglarExpressionFromXMLRule rule = new AnnotationRuglarExpressionFromXMLRule();
		    	rule.setRuleId(id);
		    	rule.setErrorMessage(titleString);
		    	rule.setPattern(pattern);
		    	rule.setRegex(regex);
		    	
		    	rules.add(rule);
		    }
		}catch(Exception ex){
			LOG.error(ex.getMessage(), ex);
		}
	    
		
		try{
			XPath javaRule = XPath.newInstance("//implementation/script[@language='java']");			
		    Iterator itr = javaRule.selectNodes(doc).iterator();
		    
		    while(itr.hasNext()){
		    	Element script = (Element)itr.next();
		    	Element idElement = script.getParentElement().getParentElement().getParentElement().getChild("id");
		    	String id = "";
		    	
		    	if(idElement != null){
		    		id = idElement.getTextNormalize();
		    	}
		    	
		    	String className = script.getAttributeValue("source");
		    	if(className != null){
		    		try{
		    			AbstractAnnotationRule rule= (AbstractAnnotationRule) Class.forName(className).newInstance();
		    			rule.setRuleId(id);
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
	
	
	public List<AnnotationRule> getRules(){
		return this.rules;
	}
	
}
