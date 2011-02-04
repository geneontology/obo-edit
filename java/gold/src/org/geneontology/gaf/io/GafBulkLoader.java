package org.geneontology.gaf.io;

import java.io.IOException;

import org.geneontology.gaf.hibernate.Bioentity;
import org.geneontology.gaf.hibernate.GafDocument;
import org.geneontology.gaf.hibernate.GeneAnnotation;
import org.geneontology.gaf.hibernate.WithInfo;
import org.geneontology.gold.io.TableDumper;

public class GafBulkLoader {

	
	private GafDocument doc;
	
	private String prefix;
	
	private String outputPath;
	
	public GafBulkLoader(GafDocument doc, String outputPath, String prefix){
		this.doc = doc;
		this.prefix = prefix;
		this.outputPath = outputPath;
	}
	
	
	
	
	public void loadAll() throws IOException{
		loadBioentity();
		loadGeneAnnotations();
	}
	
	private TableDumper createDumper(String tablename) throws IOException{
		return new TableDumper(tablename, this.outputPath, this.prefix);
	}
	
	private String loadBioentity() throws IOException{
		TableDumper dumper = createDumper("bioentity");
		
		
		for(Bioentity entity: doc.getBioentities()){
			dumper.dumpRow(entity.getId(), entity.getSymbol(), entity.getFullName(),
					entity.getTypeCls(), entity.getNcbiTaxonId() + "", entity.getDb(), 
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
					ann.getActsOnTaxonId() ==-1 ? null : ann.getActsOnTaxonId() + "",
					ann.getLastUpdateDate(), ann.getAssignedBy(), ann.getExtensionExpression(),
					ann.getGeneProductForm(), ann.getGafDocument());
		}
		
		return dumper.getTable();
	}
	
	
}
