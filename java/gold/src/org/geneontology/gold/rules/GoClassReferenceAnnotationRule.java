package org.geneontology.gold.rules;

import java.util.HashSet;
import java.util.Hashtable;
import java.util.Set;
import org.geneontology.gaf.hibernate.GeneAnnotation;
import org.geneontology.web.services.GoldDbOperationsService;
import org.obolibrary.obo2owl.Obo2OWLConstants;
import org.semanticweb.owlapi.model.OWLClass;

import owltools.graph.OWLGraphWrapper;

public class GoClassReferenceAnnotationRule extends AbstractAnnotationRule {

	// private OWLGraphWrapper graph;

	/*
	 * public GoClassReferenceAnnotationRule(OWLGraphWrapper wrapper){
	 * this.graph = wrapper; }
	 */

	// private Hashtable<String, Boolean> cache;

	public GoClassReferenceAnnotationRule() {
		//cache = new Hashtable<String, Boolean>();
	}

	@Override
	public Set<AnnotationRuleViolation> getRuleViolations(GeneAnnotation a) {

		HashSet<AnnotationRuleViolation> set = new HashSet<AnnotationRuleViolation>();
		String cls = a.getCls().replace(":", "_");

		// GoldDbOperationsService goldDb = (GoldDbOperationsService)
		// ServicesConfig.getService("gold-db-operations");

		// Boolean isObsolete = cache.get(cls);

		// if(isObsolete == null){

		OWLGraphWrapper graph = GoldDbOperationsService.getGraphWrapper();
		OWLClass owlClass = graph
				.getOWLClass(Obo2OWLConstants.DEFAULT_IRI_PREFIX + cls);

		if (owlClass == null) {
			AnnotationRuleViolation v = new AnnotationRuleViolation(
					"The GO id in the annotation is a dangling reference", a);
			v.setRuleId(getRuleId());
			set.add(v);
		}

		boolean isObsolete = graph.getIsObsolete(owlClass);

		// cache.put(cls, isObsolete);
		// }

		if (isObsolete) {
			AnnotationRuleViolation arv = new AnnotationRuleViolation(
					"The GO id in the annotation is a obsolete class", a);
			arv.setRuleId(getRuleId());
			// arv.setSuggestedReplacements(suggestedReplacements)
			set.add(arv);
		}

		return set;
	}

}
