package org.geneontology.gaf.io;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.geneontology.gold.io.TableDumper;

import owltools.gaf.Bioentity;
import owltools.gaf.CompositeQualifier;
import owltools.gaf.ExtensionExpression;
import owltools.gaf.GafDocument;
import owltools.gaf.GeneAnnotation;
import owltools.gaf.WithInfo;

public class GafBulkLoader {

	
	private GafDocument doc;
	
	private String prefix;
	
	private String outputPath;
	
	public GafBulkLoader(GafDocument doc, String outputPath, String prefix){
		this.doc = doc;
		this.prefix = prefix;
		this.outputPath = outputPath;
	}
	
	/**
	 * loads all bioentities and annotations into the database
	 * 
	 * @return
	 * @throws IOException
	 */
	public List<String> loadAll() throws IOException{
		
		List<String> tables = new ArrayList<String>();
		
		String t = loadBioentity();
		tables.add(t);
		
		t = loadGeneAnnotations();
		tables.add(t);
		
		t = loadWithInfos();
		tables.add(t);
		
		t= loadExtensionExpressions();
		tables.add(t);
		
		t = loadCompositeQualifiers();
		tables.add(t);
		
		return tables;
	}
	
	private TableDumper createDumper(String tablename) throws IOException{
		return new TableDumper(this.prefix + tablename, this.outputPath);
	}
	
	private String loadBioentity() throws IOException{
		TableDumper dumper = createDumper("bioentity");
		
		
		for(Bioentity entity: doc.getBioentities()){
			dumper.dumpRow(entity.getId(), entity.getSymbol(), entity.getFullName(),
					entity.getTypeCls(), ""+entity.getNcbiTaxonId() + "", entity.getDb(), 
					entity.getGafDocument());
		}
		
		dumper.close();
	
		return dumper.getTable();
	}
	
	private String loadGeneAnnotations() throws IOException{
		TableDumper dumper = createDumper("gene_annotation");

		for(GeneAnnotation ann: doc.getGeneAnnotations()){
			dumper.dumpRow(ann.getBioentity(), ann.getCompositeQualifier(), 
					ann.getIsContributesTo()  ? ann.getIsContributesTo() + "" : null, 
					ann.getIsIntegralTo() ? ann.getIsIntegralTo() + "" : null, 
					ann.getCls(), ann.getReferenceId(),
					ann.getEvidenceCls(), ann.getWithExpression(), 
					ann.getActsOnTaxonId(),
					ann.getLastUpdateDate(), ann.getAssignedBy(), ann.getExtensionExpression(),
					ann.getGeneProductForm(), ann.getGafDocument());
		}
		
		dumper.close();
		
		return dumper.getTable();
	}
	
	private String loadWithInfos() throws IOException{
		TableDumper dumper = createDumper("with_info");

		for(String id: doc.getWithInfosIds()){
			for(WithInfo withinfo: doc.getWithInfos(id)){
				dumper.dumpRow(withinfo.getId(), withinfo.getWithXref());
			}
		}
		
		dumper.close();
		return dumper.getTable();
	}
	
	private String loadExtensionExpressions() throws IOException{
		TableDumper dumper = createDumper("extension_expression");
		
		for(String id: doc.getExtensionExpressionIds()){
			for(ExtensionExpression ee: doc.getExpressions(id)){
				dumper.dumpRow(ee.getId(), ee.getRelation(), ee.getCls());
			}
		}
		
		
		dumper.close();
		return dumper.getTable();
	}
	
	private String loadCompositeQualifiers() throws IOException{
		TableDumper dumper = createDumper("composite_qualifier");

		for(String id: doc.getCompositeQualifiersIds()){
			for(CompositeQualifier cq: doc.getCompositeQualifiers(id)){
				dumper.dumpRow(cq.getId(), cq.getQualifierObj());
			}
		}
		
		dumper.close();
		return dumper.getTable();
		
	}
	
}
