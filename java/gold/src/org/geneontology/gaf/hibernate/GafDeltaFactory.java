package org.geneontology.gaf.hibernate;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Set;
import java.util.Vector;
import org.apache.log4j.Logger;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gaf.io.GAFDbOperations;
import org.geneontology.gold.io.DatabaseDialect;
import org.geneontology.gold.io.postgres.PostgresDialect;

/**
 * This class builds delta from two databases GOLD for the tables related to GAF
 * ,i.e. gold.dbname and gold.deltatableprefix (see gold.properties file). 
 * It returns only changed hibernate objects built from GafDocument (provided in the constructor). 
 * The objects are needed to be merged in the gold.dbname database.
 * The instance of this class is used in the {@link GAFDbOperations} class.
 * @see {@link PostgresDialect}, 
 * @author Shahid Manzoor
 *
 */
public class GafDeltaFactory {

	private static Logger LOG = Logger.getLogger(GafDeltaFactory.class);
	
	/**
	 * This object executes queries to build delta of two tables.
	 */
	private DatabaseDialect db;
	
	/**
	 * This object represents object model of the GAF file being 
	 * updated in the database
	 */
	private GafDocument doc;
	
	/**
	 * Index of the {@link GeneAnnotation} objects with their unique key.
	 */
	private Hashtable<String, GeneAnnotation> annotations;
	
	public GafDeltaFactory(GafDocument doc)throws Exception{
		this();
		this.doc = doc;
		
		annotations = new Hashtable<String, GeneAnnotation>();
		
		for(GeneAnnotation ga: doc.getGeneAnnotations()){
			annotations.put(ga.getBioentity() + ga.getCls() + ga.getReferenceId() + ga.getEvidenceCls(), ga);
		}
	}
	
	protected GafDeltaFactory()throws Exception{
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		db = GeneOntologyManager.getInstance().buildDatabaseDialect();
	}
	
	public List<Bioentity> buildBioentityDelta() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Vector<Bioentity> list = new Vector<Bioentity>();
		
		ResultSet rs = db.getDelaData("bioentity");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			list.add( doc.getBioentity(rs.getString("id"))
					);
		}
		
		return list;
	}
	
	public List<GeneAnnotation> buildGeneAnnotations() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Vector<GeneAnnotation> list = new Vector<GeneAnnotation>();
		
		ResultSet rs = db.getDelaData("gene_annotation");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
		
			String key = rs.getString("bioentity") + rs.getString("cls") + rs.getString("reference_id") + rs.getString("evidence_cls");
			GeneAnnotation ga =  annotations.get(key);
				
			list.add(ga);
		
		}
		
		return list;
		
	}
	
	public Set<CompositeQualifier> buildCompositeQualifiers() throws SQLException{
		
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Set<CompositeQualifier> set = new HashSet<CompositeQualifier>();
		
		ResultSet rs = db.getDelaData("composite_qualifier");
		
		if(rs == null)
			return set;
		
		
		while(rs.next()){
			
			set.addAll(
			doc.getCompositeQualifiers(rs.getString("id"))
			);
		}
		
		return set;
		
	}
	
	public Set<ExtensionExpression> buildExtensionExpressions() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		HashSet<ExtensionExpression> set = new HashSet<ExtensionExpression>();
		
		ResultSet rs = db.getDelaData("extension_expression");
		
		if(rs == null)
			return set;
		
		while(rs.next()){
			
			set.addAll(
			doc.getExpressions(rs.getString("id"))
			);
		}
		
		return set;
	}
	
	public Set<WithInfo> buildWithInfos() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		HashSet<WithInfo> set = new HashSet<WithInfo>();
		
		ResultSet rs = db.getDelaData("with_info");
		
		if(rs == null)
			return set;
		
		while(rs.next()){
			
			set.addAll(
			doc.getWithInfos(rs.getString("id"))
			);
		}
		
		return set;
		
	}
	
}