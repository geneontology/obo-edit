package org.geneontology.gold.rules;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import org.apache.log4j.Logger;
import org.geneontology.gaf.hibernate.GeneAnnotation;
import org.jdom.Element;

/**
 * This class implements execution of the regular expression rules. All the regular expression rules
 * are run by the single instance of this class.
 * @author Shahid Manzoor
 *
 */
public class AnnotationRuglarExpressionFromXMLRule extends
		AbstractAnnotationRule {

	private String regex;
	private Pattern pattern;
	private String errorMessage;
	
	
	
	/**
	 * 
	 * @param The parameter is the list of the {@link Element} objects. These elements objects are 
	 * build from the annotation-qc.xml xml file. The each element object points to 'script' xml tag.
	 */
	public AnnotationRuglarExpressionFromXMLRule(){
		//init(regexList);
	}

	private static Logger LOG = Logger.getLogger(AnnotationRuglarExpressionFromXMLRule.class);
	
	
	//private  List<RuleData> rules;
	
	public Set<AnnotationRuleViolation> getRuleViolations(GeneAnnotation a) {

		if(a == null){
			throw new IllegalArgumentException("GeneAnnotation argument is null");
		}
		
		HashSet<AnnotationRuleViolation> voilations = new HashSet<AnnotationRuleViolation>();

		getRuleViolations(voilations, a);
		
		return voilations;
	}
	
	private void getRuleViolations(Set<AnnotationRuleViolation> voilations, GeneAnnotation ann){

		Matcher m = pattern.matcher(ann.toString());
		
		if(m.find()){
			AnnotationRuleViolation v = new AnnotationRuleViolation(this.errorMessage, ann);
			v.setRuleId(this.getRuleId());
			voilations.add(v);
		}
		
		
	}

	public String getRegex() {
		return regex;
	}

	public Pattern getPattern() {
		return pattern;
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setRegex(String regex) {
		this.regex = regex;
	}

	public void setPattern(Pattern pattern) {
		this.pattern = pattern;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	
	
	/**
	 * 
	 * @param This method iterates through the script xml elements to get the regular expression. 
	 * It gets the 'title' element (child of the 'rule' element). During the execution of the rule title
	 * is used as an error message when the regular expression is matched.
	 */
	/*private  void init(List regexList){
		rules = new ArrayList<AnnotationRuglarExpressionFromXMLRule.RuleData>();
		
		try {
		    Iterator itr = regexList.iterator();
		    
		    while(itr.hasNext()){
		    	Element script = (Element) itr.next();
		    	
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
		    	
		    	RuleData data = new RuleData(pattern, titleString, id);
		    	
		    	rules.add(data);
		    }
		    
		} catch (Exception e) {
			LOG.error(e.getMessage(), e);
		}
	}
	
	
	private class RuleData{
		
		public RuleData(Pattern pattern, String errorMsg, String ruleId){
			this.pattern = pattern;
			this.errorMsg = errorMsg;
			this.ruleId = ruleId;
		}
		
		private Pattern pattern;
		private String errorMsg;
		private String ruleId;
	}*/
	
}
