package org.geneontology.gaf.hibernate;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Set;

import org.apache.log4j.Logger;
import org.geneontology.gold.hibernate.model.GOModel;
import org.geneontology.gold.rules.AnnotationRuleViolation;
import org.geneontology.gold.rules.AnnotationTaxonRule;
import org.geneontology.gold.rules.GoClassReferenceAnnotationRule;

import owltools.graph.OWLGraphWrapper;

public class GafDocument extends GOModel implements Serializable {

	private static Logger LOG = Logger.getLogger(GafDocument.class);
	
	private String id;
	private String documentPath;
	
	private transient Hashtable<String, Bioentity> bioentities;
	private transient Hashtable<String, List<WithInfo>> withInfos;
	private transient Hashtable<String, List<ExtensionExpression>> extensionExpressions;
	private transient Hashtable<String, List<CompositeQualifier>> compositeQualifiers; 
	private transient List<GeneAnnotation> annotations;
	
	private boolean hibernateLoad;
	
	private GafObjectsFactory factory;
	
	public GafDocument(){
		String keys [] = {"id"};
		this.initUniqueConstraintFields(GafDocument.class, keys);
		hibernateLoad = false;
		
		
		bioentities = new Hashtable<String, Bioentity>();
		withInfos = new Hashtable<String, List<WithInfo>>();
		extensionExpressions = new Hashtable<String, List<ExtensionExpression>>();
		compositeQualifiers = new Hashtable<String, List<CompositeQualifier>>();
		annotations = new ArrayList<GeneAnnotation>();
	}
	
	private GafObjectsFactory getFactory(){
		if(factory == null){
			factory = new GafObjectsFactory();
		}
		
		return factory;
	}
	
	void setHibernateLoad(){
		hibernateLoad = true;
	}
	
	public GafDocument(String id, String documentPath) {
		this();
		this.id = id;
		this.documentPath = documentPath;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getDocumentPath() {
		return documentPath;
	}

	public void setDocumentPath(String documentPath) {
		this.documentPath = documentPath;
	}
	
	public Bioentity getBioentity(String id){
		if(hibernateLoad && bioentities.isEmpty()){
			getBioentities();
		}
	
		return bioentities.get(id);
	}

	public Collection<Bioentity> getBioentities(){
		if(hibernateLoad && bioentities.isEmpty()){
			for(Bioentity entity: factory.getBioentities(getId())){
				addBioentity(entity);
			}
		}
		
		return bioentities.values();
	}
	

	public List<GeneAnnotation> getGeneAnnotations(){
		if(hibernateLoad && annotations.isEmpty()){
			for(GeneAnnotation annotation: factory.getGeneAnnotations(getId())){
				addGeneAnnotation(annotation);
			}
			
			
		}
		
		return annotations;
	}
	
	public Set<GeneAnnotation> getGeneAnnotations(String bioentity){
		return null;
	}
	
	public Set<GeneAnnotation> getGeneAnnotationsByGoCls(String cls){
		return null;
	}
	
	public void addBioentity(Bioentity bioentity){
		bioentity.setGafDocument(this.getId());
		bioentities.put(bioentity.getId(), bioentity);
	}
	
	public void addCompositeQualifier(CompositeQualifier compositeQualifier){
		List<CompositeQualifier> list = compositeQualifiers.get(compositeQualifier.getId());
		if(list == null){
			list = new ArrayList<CompositeQualifier>();
			compositeQualifiers.put(compositeQualifier.getId(), list);
		}
		
		if(!list.contains(compositeQualifier))
			list.add(compositeQualifier);
	}
	
	public Set<String> getCompositeQualifiersIds(){
		return compositeQualifiers.keySet();
	}
	
	public List<CompositeQualifier> getCompositeQualifiers(String id){
		List<CompositeQualifier> list = compositeQualifiers.get(id);
		
		if(list == null && hibernateLoad){
			list = getHibernateObjects(CompositeQualifier.class, id, id);
			compositeQualifiers.put(id, list);
		}
		
		return list;
	}
	
	public void addWithInfo(WithInfo withInfo){
		
		List<WithInfo> list = withInfos.get(withInfo.getId());
		if(list == null){
			list = new ArrayList<WithInfo>();
			withInfos.put(withInfo.getId(), list);
		}
		
		if(!list.contains(withInfo))
			list.add(withInfo);
		
	}
	
	public Set<String> getWithInfosIds(){
		return withInfos.keySet();
	}
	
	public List<WithInfo> getWithInfos(String id){
		List<WithInfo> list = withInfos.get(id);
		
		if(list == null && hibernateLoad){
			list = getHibernateObjects(WithInfo.class, "id", id);
			withInfos.put(id, list);
		}
		
		return list;
	}
	
	
	public List<ExtensionExpression> getExpressions(String id){
		List<ExtensionExpression> list = extensionExpressions.get(id);
		
		if(list == null && hibernateLoad){
			list = getHibernateObjects(ExtensionExpression.class, "id", id);
			extensionExpressions.put(id, list);
		}
		
		return list;
	}
	
	public Set<String> getExtensionExpressionIds(){
		return extensionExpressions.keySet();
	}
	
	public void addExtensionExpression(ExtensionExpression extensionExpression){
		List<ExtensionExpression> list = extensionExpressions.get(extensionExpression.getId());
		if(list == null){
			list = new ArrayList<ExtensionExpression>();
			extensionExpressions.put(extensionExpression.getId(), list);
		}
		
		if(!list.contains(extensionExpression))
			list.add(extensionExpression);
	}
	
	public void addGeneAnnotation(GeneAnnotation ga){
		ga.setGafDocumetObject(this);
		ga.setGafDocument(this.getId());
		annotations.add(ga);
	}
	
	public Set<AnnotationRuleViolation> validateAnnotations(OWLGraphWrapper graph){
		HashSet<AnnotationRuleViolation> set = new HashSet<AnnotationRuleViolation>();
		
		try{
		
			GoClassReferenceAnnotationRule rule = new GoClassReferenceAnnotationRule();
		//	AnnotationTaxonRule taxonRule = new AnnotationTaxonRule();
			
			for(GeneAnnotation annotation: getGeneAnnotations()){
				set.addAll(rule.getRuleViolations(annotation));
			//	set.addAll(taxonRule.getRuleViolations(annotation));
			}
		}catch(Exception ex){
			LOG.error(ex.getMessage(), ex);
		}
		
		return set;
	}
}
