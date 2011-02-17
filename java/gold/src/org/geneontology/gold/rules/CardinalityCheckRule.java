package org.geneontology.gold.rules;

import java.util.HashSet;
import java.util.Set;

import org.geneontology.gaf.hibernate.Bioentity;
import org.geneontology.gaf.hibernate.GeneAnnotation;

public class CardinalityCheckRule extends AbstractAnnotatioRule {

	@Override
	public Set<AnnotationRuleViolation> getRuleViolations(GeneAnnotation a) {

		HashSet<AnnotationRuleViolation> set = new HashSet<AnnotationRuleViolation>();
		
		Bioentity bioentity = a.getBioentityObject();
		
		String dbObjectId = bioentity.getId();
	
		String db = dbObjectId.substring(0, dbObjectId.indexOf(':')-1);

		checkCardinality(dbObjectId, "Column 1: DB", set, a,1,1);
		
		dbObjectId = dbObjectId.substring(db.length()-1);
		
		checkCardinality(dbObjectId, "Column 2: DB Object ID", set, a,1,1);
		
		checkCardinality(bioentity.getSymbol(), "Column 3: DB Object Symbol", set, a,1,1);
		
		checkCardinality(a.getCompositeQualifier(), "Column 4: Qualifier", set, a,0, 3);

		checkCardinality(a.getCls(), "Column 5: GO ID", set, a,1,1);
		
		checkCardinality(a.getEvidenceCls(), "Column 7: Evidence Code", set, a,1,3);
		
		checkCardinality(a.getWithExpression(), "Column 8: With or From", set, a,0,3);

		checkCardinality(bioentity.getFullName(), "Column 10: DB Object Name", set, a,0,1);
		
		checkCardinality(bioentity.getDb(), "Column 11: DB Object Synonym", set, a, 0,3);
		
		checkCardinality(bioentity.getTypeCls(), "Column 12: DB Object Type", set, a, 1,1);
		
		
		
		return set;
	
	}


	private void checkWhiteSpaces(String value, String columnName, HashSet<AnnotationRuleViolation> set, GeneAnnotation a){

		if(value.length() != value.trim().length()){
			set.add(new AnnotationRuleViolation("Spaces are not allowed in the " + columnName+ " column", a));
		}
		
	}
	
	
	
	private void checkCardinality(String value, String columnName, HashSet<AnnotationRuleViolation> set, GeneAnnotation a, int min, int max){

		if(value.length() != value.trim().length()){
			set.add(new AnnotationRuleViolation("Spaces are not allowed in the " + columnName+ " column", a));
		}
		
		if(min>=1 && value.length()==0){
			set.add(new AnnotationRuleViolation(columnName +" column cannot be empty", a));
		}
		
		if(max==1 && value.contains("|")){
			set.add(new AnnotationRuleViolation(columnName +" colmn cardinality cannt be greater than 1", a));
		}
		
		String tokens[] = value.split("|");
		
		if(tokens.length>1){
			for(String token: tokens){
				checkWhiteSpaces(token, columnName, set, a);
			}
		}
		
		
		
		
	}
	
	
}
