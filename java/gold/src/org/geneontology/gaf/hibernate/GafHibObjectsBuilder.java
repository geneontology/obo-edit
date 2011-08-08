package org.geneontology.gaf.hibernate;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import org.apache.log4j.Logger;

import owltools.gaf.Bioentity;
import owltools.gaf.ExtensionExpression;
import owltools.gaf.GAFParser;
import owltools.gaf.WithInfo;

public class GafHibObjectsBuilder {

	private final static Logger LOG = Logger.getLogger(GafHibObjectsBuilder.class);
	
	private GafDocument gafDocument;
	
	private owltools.gaf.GafObjectsBuilder docBuilder;
	
	
	public GafHibObjectsBuilder(){
		
		docBuilder = new owltools.gaf.GafObjectsBuilder();
		
	}
	
	public GafDocument getGafDocument(){
		return gafDocument;
	}
	
	public GAFParser getParser(){
		return docBuilder.getParser();
	}
	
	public GafDocument buildDocument(Reader reader, String docId, String path) throws IOException{
	
		GafDocument doc = convert( docBuilder.buildDocument(reader, docId, path) );
		
		return doc;
	
	}
	
	
	private GafDocument convert(owltools.gaf.GafDocument doc){
		if(doc == null)
			return null;
		
		GafDocument hibDoc = new GafDocument(doc.getId(), doc.getDocumentPath());
		
		for(owltools.gaf.Bioentity entity: doc.getBioentities() ){
			org.geneontology.gaf.hibernate.Bioentity hibEntity = new org.geneontology.gaf.hibernate.Bioentity(entity.getId(), entity.getSymbol(), entity.getFullName(), 
					entity.getTypeCls(), entity.getNcbiTaxonId(), entity.getDb(), entity.getGafDocument());
			
			hibDoc.addBioentity(hibEntity);
		}
		
		
		for(owltools.gaf.GeneAnnotation ga: doc.getGeneAnnotations()){
			GeneAnnotation hibGa = new GeneAnnotation(ga.getBioentity(), ga.getIsContributesTo(), ga.getIsIntegralTo(), 
					ga.getCompositeQualifier(), ga.getCls(), ga.getReferenceId(), ga.getEvidenceCls(), 
					ga.getWithExpression(), ga.getActsOnTaxonId(), ga.getLastUpdateDate(), ga.getAssignedBy(), 
					ga.getExtensionExpression(), ga.getGeneProductForm(), ga.getGafDocument());
			hibDoc.addGeneAnnotation(hibGa);
		}
		
		for(String id: doc.getCompositeQualifiersIds()){
			for(owltools.gaf.CompositeQualifier cq: doc.getCompositeQualifiers(id)){
				CompositeQualifier hibCq = new CompositeQualifier(cq.getId(), cq.getQualifierObj());
				hibDoc.addCompositeQualifier(hibCq);
			}
		}
		
		for(String id: doc.getExtensionExpressionIds()){
			for(ExtensionExpression ee: doc.getExpressions(id)) {
				org.geneontology.gaf.hibernate.ExtensionExpression hibEe = new org.geneontology.gaf.hibernate.ExtensionExpression(ee.getId(), ee.getRelation(), ee.getCls());
				hibDoc.addExtensionExpression(hibEe);
			}
		}
		
		for(String id: doc.getWithInfosIds()){
			for(WithInfo wi: doc.getWithInfos(id)) {
				org.geneontology.gaf.hibernate.WithInfo hibWi = new org.geneontology.gaf.hibernate.WithInfo(wi.getId(), wi.getWithXref());
				hibDoc.addWithInfo(hibWi);
			}
		}
		
		return hibDoc;
	}
	
	
	public GafDocument getNextSplitDocument() throws IOException{

		GafDocument doc = convert( docBuilder.getNextSplitDocument() );

		return doc;
		
	}
	
	public boolean isDocumentSplitted(){
		return docBuilder.isDocumentSplitted();
	}
		
	
	public GafDocument buildDocument(File gafFilePath) throws IOException{
		
		FileReader reader = new FileReader(gafFilePath);
		return buildDocument(reader, gafFilePath.getName(), gafFilePath.getCanonicalPath());
		
	}

	
}
