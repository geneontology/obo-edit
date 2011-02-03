package org.geneontology.gaf.hibernate;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Set;

import org.geneontology.gold.hibernate.model.GOModel;

public class GafDocument extends GOModel implements Serializable {

	private String id;
	private String documentPath;
	
	private transient Hashtable<String, Bioentity> bioentities;
	private transient Hashtable<String, Set<WithInfo>> withInfos;
	private transient Hashtable<String, Set<ExtensionExpression>> extensionExpressions;
	private transient Hashtable<String, Set<CompositeQualifier>> compositeQualifiers; 
	private transient HashSet<GeneAnnotation> annotations;
	
	public GafDocument(){
		String keys [] = {"id"};
		this.initUniqueConstraintFields(GafDocument.class, keys);
		bioentities = new Hashtable<String, Bioentity>();
		withInfos = new Hashtable<String, Set<WithInfo>>();
		extensionExpressions = new Hashtable<String, Set<ExtensionExpression>>();
		compositeQualifiers = new Hashtable<String, Set<CompositeQualifier>>();
		annotations = new HashSet<GeneAnnotation>();
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
		return bioentities.get(id);
	}

	public Collection<Bioentity> getBioentities(){
		return bioentities.values();
	}
	

	public Set<GeneAnnotation> getGeneAnnotations(){
		return annotations;
	}
	
	public Set<GeneAnnotation> getGeneAnnotations(String bioentity){
		return null;
	}
	
	public Set<GeneAnnotation> getGeneAnnotationsByGoCls(String cls){
		return null;
	}
	
	public void addBioentity(Bioentity bioentity){
		if(bioentities.contains(bioentity.getId()))
				return;
		
		bioentities.put(bioentity.getId(), bioentity);
	}
	
	public void addCompositeQualifier(CompositeQualifier compositeQualifier){
		Set<CompositeQualifier> set = compositeQualifiers.get(compositeQualifier.getId());
		if(set == null){
			set = new HashSet<CompositeQualifier>();
			compositeQualifiers.put(compositeQualifier.getId(), set);
		}
		
		set.add(compositeQualifier);
	}
	
	public Set<String> getCompositeQualifiersIds(){
		return compositeQualifiers.keySet();
	}
	
	public Set<CompositeQualifier> getCompositeQualifiers(String id){
		return compositeQualifiers.get(id);
	}
	
	public void addWithInfo(WithInfo withInfo){
		
		Set<WithInfo> set = withInfos.get(withInfo.getId());
		if(set == null){
			set = new HashSet<WithInfo>();
			withInfos.put(withInfo.getId(), set);
		}
		
		set.add(withInfo);
		
	}
	
	public Set<String> getWithInfosIds(){
		return withInfos.keySet();
	}
	
	public Set<WithInfo> getWithInfos(String id){
		return withInfos.get(id);
	}
	
	
	public Set<ExtensionExpression> getExpressions(String id){
		return extensionExpressions.get(id);
	}
	
	public Set<String> getExtensionExpressionIds(){
		return extensionExpressions.keySet();
	}
	
	public void addExtensionExpression(ExtensionExpression extensionExpression){
		Set<ExtensionExpression> set = extensionExpressions.get(extensionExpression.getId());
		if(set == null){
			set = new HashSet<ExtensionExpression>();
			extensionExpressions.put(extensionExpression.getId(), set);
		}
		
		set.add(extensionExpression);
	}
	
	public void addGeneAnnotation(GeneAnnotation ga){
		annotations.add(ga);
	}
	
}
