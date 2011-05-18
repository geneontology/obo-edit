package org.geneontology.gold.rules;

import java.util.HashSet;
import java.util.Set;

import org.geneontology.gaf.hibernate.GeneAnnotation;
import org.geneontology.web.services.GoldDbOperationsService;
import org.geneontology.web.services.ServicesConfig;
import org.obolibrary.obo2owl.Obo2OWLConstants;
import org.semanticweb.owlapi.model.OWLClass;

import owltools.graph.OWLGraphWrapper;

public class GoClassReferenceAnnotationRule extends AbstractAnnotationRule {

	
//	private OWLGraphWrapper graph;
	
/*	public GoClassReferenceAnnotationRule(OWLGraphWrapper wrapper){
		this.graph = wrapper;
	}
	*/
	
	public GoClassReferenceAnnotationRule(){
		
	}
	
	@Override
	public Set<AnnotationRuleViolation> getRuleViolations(GeneAnnotation a) {

		
		String cls = a.getCls().replace(":", "_");
		
		GoldDbOperationsService goldDb = (GoldDbOperationsService) ServicesConfig.getService("gold-db-operations");

		OWLGraphWrapper graph = goldDb.getGraphWrapper();
		
		OWLClass owlClass= graph.getOWLClass(Obo2OWLConstants.DEFAULT_IRI_PREFIX + cls);
		
		HashSet<AnnotationRuleViolation> set = new HashSet<AnnotationRuleViolation>();
		
		if(owlClass == null){
			AnnotationRuleViolation v = new AnnotationRuleViolation("The GO id in the annotation is a dangling reference", a);
			v.setRuleId(getRuleId());
			set.add(v);
		}
		
		boolean isObsolete = graph.getIsObsolete(owlClass);
		
		if(isObsolete){
			AnnotationRuleViolation arv = new AnnotationRuleViolation("The GO id in the annotation is a obsolete class", a);
			arv.setRuleId(getRuleId());
			//arv.setSuggestedReplacements(suggestedReplacements)
			set.add(arv);
		}
		
		
		return set;
	}

	
	
}
