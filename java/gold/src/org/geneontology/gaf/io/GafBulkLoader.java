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

/**
 * This class generates tab separated text files for bulkload into database tables, i.e bioentity, gene_annotation 
 * with_info, extension_expression and composite_qualifier. It takes a {@link GafDocument} as input
 * and builds tables files from it.  
 * @author Shahid Manzoor
 *
 */
		
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
	public List<String> loadAll(List<String> tablesNames) throws IOException{
		
		List<String> tables = new ArrayList<String>();
		
		String t = null;
		
		if(tablesNames.isEmpty() || tablesNames.contains("bioentity")){
			t=	loadBioentity();
			tables.add(t);
		}

		if(tablesNames.isEmpty() || tablesNames.contains("gene_annotation")){
			t = loadGeneAnnotations();
			tables.add(t);
		}
		
		if(tablesNames.isEmpty() || tablesNames.contains("with_info")){
			t = loadWithInfos();
			tables.add(t);
		}
		
		if(tablesNames.isEmpty() || tablesNames.contains("extension_expression")){
			t= loadExtensionExpressions();
			tables.add(t);
		}
		
		if(tablesNames.isEmpty() || tablesNames.contains("composite_qualifier")){
			t = loadCompositeQualifiers();
			tables.add(t);
		}
		
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
