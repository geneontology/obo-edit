package org.geneontology.gaf.hibernate;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import org.apache.log4j.Logger;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gaf.hibernate.GafConstants.EvidenceCode;
import org.geneontology.gaf.hibernate.GafConstants.Qualifier;
import org.geneontology.gaf.parser.GAFParser;
import org.geneontology.gold.rules.AnnotationRuleViolation;

public class GafObjectsBuilder {

	private final static Logger LOG = Logger.getLogger(GAFParserHandlerForHibernate.class);
	
	private GafDocument gafDocument;
	
	private GAFParser parser;
	
	public GafObjectsBuilder(){
		gafDocument = new GafDocument();
	}
	
	public GafDocument getGafDocument(){
		return gafDocument;
	}
	
	public void startDocument(File gafFile) {
		gafDocument.setDocumentPath(gafFile.getAbsolutePath());
		gafDocument.setId(gafFile.getName());
		
	}

	public GAFParser getParser(){
		return parser;
	}
	
	public GafDocument buildDocument(Reader reader, String docId, String path) throws IOException{
	
		gafDocument = new GafDocument(docId, path);
		
		parser = new GAFParser();
		
		parser.parse(reader);
		
		while(parser.next()){
			Bioentity entity= addBioEntity(parser);
			addGeneAnnotation(parser, entity);
			addWithInfo(parser);
			addCompositeQualifier(parser);
			addExtensionExpression(parser);
		}
		
		
		
		return gafDocument;
		
	
	}
		
	
	public GafDocument buildDocument(File gafFilePath) throws IOException{
		
		FileReader reader = new FileReader(gafFilePath);
		return buildDocument(reader, gafFilePath.getName(), gafFilePath.getCanonicalPath());
		
	}
	
	private Bioentity addBioEntity(GAFParser parser){
		String id = parser.getDb() + ":" + parser.getDbObjectId();
		String symbol = parser.getDbObjectSymbol();
		String fullName = parser.getDbObjectName();
		String typeCls = parser.getDBObjectType();
		int ncbiTaxonId =-1;
		String taxons[] = parser.getTaxon().split("\\|");
		taxons = taxons[0].split(":");
		ncbiTaxonId = Integer.parseInt(taxons[1]);
		
		String db = parser.getDbObjectSynonym();
		
		Bioentity entity = new Bioentity(id, symbol, fullName, typeCls, ncbiTaxonId, db, gafDocument.getId());
		
		gafDocument.addBioentity(entity);
		

		return entity;
	}
	
	
	private void addWithInfo(GAFParser parser){
		if(parser.getWith().length()>0){
			String tokens[] = parser.getWith().split("\\|");
			for(String token: tokens){
				gafDocument.addWithInfo(new WithInfo(parser.getWith(), token));
			}
		}
	}
	
	private void addCompositeQualifier(GAFParser parser){
		if(parser.getQualifier().length()>0){
			String tokens[] = parser.getQualifier().split("\\|");
			for(String token: tokens){
				gafDocument.addCompositeQualifier(new CompositeQualifier(parser.getQualifier(), token));
			}
		}
	}

	private void addExtensionExpression(GAFParser parser){
		if(parser.getAnnotationExtension() != null){
			if(parser.getAnnotationExtension().length()>0){
				String tokens[] = parser.getAnnotationExtension().split("\\|");
				for(String token: tokens){
					
					int index = token.indexOf("(");
					
					if(index>0){
						String relation = token.substring(0, index);
						String cls = token.substring(index+1, token.length()-1);
						gafDocument.addExtensionExpression(new ExtensionExpression(parser.getAnnotationExtension(), relation, cls));
					}
					
				}
			}
			
		}
	}
	
	private void addGeneAnnotation(GAFParser parser, Bioentity entity){
		String compositeQualifier = parser.getQualifier();
	
		
		boolean isContributesTo = compositeQualifier.contains("contributes_to");
		boolean isIntegeralTo = compositeQualifier.contains("integral_to");
		
		String clsId = parser.getGOId();

		String referenceId = parser.getReference();
		
		String evidenceCls = parser.getEvidence();
		String withExpression = parser.getWith();

		int actsOnTaxonId =-1;
		
		String taxons[] = parser.getTaxon().split("\\|");
		if(taxons.length>1){
			taxons = taxons[1].split(":");
			actsOnTaxonId = Integer.parseInt(taxons[1]);
		}
		
		String lastUpdateDate = parser.getDate();
		
		String assignedBy = parser.getAssignedBy();

		String extensionExpression = parser.getAnnotationExtension();
		String geneProductForm = parser.getGeneProjectFormId();

		
		GeneAnnotation ga = new GeneAnnotation();
		ga.setBioentity(entity.getId());
		ga.setIsContributesTo(isContributesTo);
		ga.setIsIntegralTo(isIntegeralTo);
		ga.setCls(clsId);
		ga.setReferenceId(referenceId);
		ga.setEvidenceCls(evidenceCls);
		ga.setWithExpression(withExpression);
		ga.setActsOnTaxonId(actsOnTaxonId);
		ga.setLastUpdateDate(lastUpdateDate);
		ga.setAssignedBy(assignedBy);
		ga.setExtensionExpression(extensionExpression);
		ga.setGeneProductForm(geneProductForm);
		ga.setCompositeQualifier(compositeQualifier);
		ga.setGafDocument(gafDocument.getId());
		
		gafDocument.addGeneAnnotation(ga);
		
	}
	
	
}
