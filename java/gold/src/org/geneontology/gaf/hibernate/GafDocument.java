package org.geneontology.gaf.hibernate;

import java.io.Serializable;
import java.util.Set;

import org.geneontology.gold.hibernate.model.GOModel;

public class GafDocument extends GOModel implements Serializable {

	private String id;
	private String documentPath;
	
	public GafDocument(){
		String keys [] = {"id"};
		this.initUniqueConstraintFields(GafDocument.class, keys);
		
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
		return null;
	}

	public Set<Bioentity> getBioentities(){
		return null;
	}
	

	public Set<GeneAnnotation> getGeneAnnotations(){
		return null;
	}
	
	public Set<GeneAnnotation> getGeneAnnotations(String bioentity){
		return null;
	}
	
	public Set<GeneAnnotation> getGeneAnnotationsByGoCls(String cls){
		return null;
	}
	
	public void addBioentity(Bioentity bioentity){
		
	}
	
	public void addCompositeQualifier(CompositeQualifier compositeQualifier){
		
	}
	
	public void addWithInfo(WithInfo withInfo){
		
	}
	
	public void addExtensionExpression(ExtensionExpression extensionExpression){
		
	}
	
	public void addGeneAnnotation(GeneAnnotation ga){
		
	}
	
}
