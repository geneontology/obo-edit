package org.geneontology.gold.hibernate.factory;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;
import java.util.Vector;

import org.apache.log4j.Logger;
import org.geneontology.conf.GeneOntologyManager;
import org.geneontology.gold.hibernate.model.AllOnlyRelationship;
import org.geneontology.gold.hibernate.model.AllSomeRelationship;
import org.geneontology.gold.hibernate.model.AnnotationAssertion;
import org.geneontology.gold.hibernate.model.AnnotationProperty;
import org.geneontology.gold.hibernate.model.Cls;
import org.geneontology.gold.hibernate.model.ClsIntersectionOf;
import org.geneontology.gold.hibernate.model.ClsUnionOf;
import org.geneontology.gold.hibernate.model.DisjointWith;
import org.geneontology.gold.hibernate.model.EquivalentTo;
import org.geneontology.gold.hibernate.model.InferredAllSomeRelationship;
import org.geneontology.gold.hibernate.model.InferredSubclassOf;
import org.geneontology.gold.hibernate.model.NeverSomeRelationship;
import org.geneontology.gold.hibernate.model.ObjAlternateId;
import org.geneontology.gold.hibernate.model.ObjAlternateLabel;
import org.geneontology.gold.hibernate.model.ObjDefinitionXref;
import org.geneontology.gold.hibernate.model.ObjSubset;
import org.geneontology.gold.hibernate.model.ObjXref;
import org.geneontology.gold.hibernate.model.Ontology;
import org.geneontology.gold.hibernate.model.OntologyAlternateLabelType;
import org.geneontology.gold.hibernate.model.OntologyAnnotation;
import org.geneontology.gold.hibernate.model.OntologyImports;
import org.geneontology.gold.hibernate.model.OntologySubset;
import org.geneontology.gold.hibernate.model.Relation;
import org.geneontology.gold.hibernate.model.RelationChain;
import org.geneontology.gold.hibernate.model.RelationDisjointWith;
import org.geneontology.gold.hibernate.model.RelationEquivalenTo;
import org.geneontology.gold.hibernate.model.SubclassOf;
import org.geneontology.gold.hibernate.model.SubrelationOf;
import org.geneontology.gold.io.DatabaseDialect;
import org.geneontology.gold.io.postgres.DeltaQueryInterceptor;
import org.geneontology.gold.io.postgres.PostgresDialect;
import org.hibernate.Session;

/**
 * This class builds delta from two databases of GOLD
 * ,i.e. gold.dbname and gold.deltatableprefix (see gold.properties file). 
 * It returns hibernate objects populated from delta tables (tables prefixed with delta word). 
 * The are need be merged in the gold.dbname database.
 * @see {@link PostgresDialect}, {@link DeltaQueryInterceptor}, 
 * @author Shahid Manzoor
 *
 */
public class GoldDeltaFactory {

	private static Logger LOG = Logger.getLogger(GoldDeltaFactory.class);
	
	private DatabaseDialect db;
	
	private GoldObjectFactory goldObjFactory;
	
	
	public Session getSession(){
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		return goldObjFactory.getSession();
	}
	
	public GoldDeltaFactory()throws Exception{
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		db = GeneOntologyManager.getInstance().buildDatabaseDialect();
		goldObjFactory = GoldObjectFactory.buildDeltaObjectFactory();
	}
	
	public List<Cls> buildClsDelta() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Vector<Cls> list = new Vector<Cls>();
		
		ResultSet rs = db.getDelaData("cls");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			String id = rs.getString("id");
			list.add(goldObjFactory.getClassById(id) );
		}
		
		return list;
	}

	public List<Relation> buildRelationDelta() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Vector<Relation> list = new Vector<Relation>();

		Session session = goldObjFactory.getSession();
		
		ResultSet rs = db.getDelaData("relation");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			String id = rs.getString("id");
			//list.add(goldObjFactory.getRelation(id, session) );
			Relation r = (Relation) session.load(Relation.class, id);
			list.add(r);
		}
		
		return list;
	}
	

	public List<SubclassOf> buildSubclassOfDelta() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Vector<SubclassOf> list = new Vector<SubclassOf>();

		ResultSet rs = db.getDelaData("subclass_of");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			SubclassOf subc= goldObjFactory.getSubClassOf(rs.getString("ontology"), 
					rs.getString("super_cls"), rs.getString("cls"));
			list.add(subc);
		}
		
		
		return list;
	}
	

	public List<SubrelationOf> buildSubrelationOf() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Vector<SubrelationOf> list = new Vector<SubrelationOf>();

		ResultSet rs = db.getDelaData("subrelation_of");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			SubrelationOf subc= goldObjFactory.getSubrelationOf(rs.getString("ontology"), 
					rs.getString("super_relation"), rs.getString("relation"));
			list.add(subc);
		}
		
		
		return list;
	}
	
		
	
	public List<ObjAlternateLabel> buildObjAlternateLabels() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Vector<ObjAlternateLabel> list = new Vector<ObjAlternateLabel>();

		ResultSet rs = db.getDelaData("obj_alternate_label");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			ObjAlternateLabel al = goldObjFactory.getObjAlternateLabelByPk(rs.getString("obj"), 
					rs.getString("label"));
			list.add(al);
		}
		
		return list;

		
	}

	public List<OntologyAlternateLabelType> buildOntologyAlternateLabelType() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		Vector<OntologyAlternateLabelType> list = new Vector<OntologyAlternateLabelType>();

		ResultSet rs = db.getDelaData("ontology_alternate_label_type");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			OntologyAlternateLabelType al = goldObjFactory.getOntologyAlternateLabelType(rs.getString("id"));
			list.add(al);
		}
		
		return list;
		
	}
		

	public List<NeverSomeRelationship> buildNeverSomeRelationship() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		
		Vector<NeverSomeRelationship> list = new Vector<NeverSomeRelationship>();

		ResultSet rs = db.getDelaData("never_some_relationship");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			NeverSomeRelationship asr = goldObjFactory.getNeverSomeRelationship(rs.getString("ontology"), 
					rs.getString("target_cls"), rs.getString("cls"), rs.getString("relation"));
			list.add(asr);
		}
		
		
		return list;
	}
	
	
	public List<AllOnlyRelationship> buildAllOnlyRelationships() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		
		Vector<AllOnlyRelationship> list = new Vector<AllOnlyRelationship>();

		ResultSet rs = db.getDelaData("all_only_relationship");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			AllOnlyRelationship asr = goldObjFactory.getAllOnlyRelationship(rs.getString("ontology"), 
					rs.getString("target_cls"), rs.getString("cls"), rs.getString("relation"));
			list.add(asr);
		}
		
		
		return list;
	}
	
	
	public List<AllSomeRelationship> buildAllSomeRelationships() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		
		Vector<AllSomeRelationship> list = new Vector<AllSomeRelationship>();

		ResultSet rs = db.getDelaData("all_some_relationship");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			AllSomeRelationship asr = goldObjFactory.getAllSomeRelationship(rs.getString("ontology"), 
					rs.getString("target_cls"), rs.getString("cls"), rs.getString("relation"));
			list.add(asr);
		}
		
		
		return list;
	}
	
	
	public List<ObjXref> buildObjXrefs() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		
		Vector<ObjXref> list = new Vector<ObjXref>();

		ResultSet rs = db.getDelaData("obj_xref");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			ObjXref objxref = goldObjFactory.getObjXref(rs.getString("obj"), 
					rs.getString("xref"));
			list.add(objxref);
		}
		return list;
		
	}
	
	
	public List<ObjDefinitionXref> buildObjDefinitionXref() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		
		Vector<ObjDefinitionXref> list = new Vector<ObjDefinitionXref>();

		ResultSet rs = db.getDelaData("obj_definition_xref");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			ObjDefinitionXref objxref = goldObjFactory.getObjDefinitionXref(rs.getString("obj"), 
					rs.getString("xref"));
			list.add(objxref);
		}
		return list;
		
	}

	
		
	
	public List<RelationEquivalenTo> buildRelationEquivalenTo() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		
		Vector<RelationEquivalenTo> list = new Vector<RelationEquivalenTo>();

		ResultSet rs = db.getDelaData("relation_equivalent_to");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			RelationEquivalenTo equiv = goldObjFactory.getRelationEquivalenTo(rs.getString("relation"), 
					rs.getString("equivalent_relation"), 
					rs.getString("ontology"));
			list.add(equiv);
		}
		return list;
		
	}
	
	public List<EquivalentTo> buildEquivalentTo() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");

		
		Vector<EquivalentTo> list = new Vector<EquivalentTo>();

		ResultSet rs = db.getDelaData("equivalent_to");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			EquivalentTo equiv = goldObjFactory.getEquivalentTo(rs.getString("cls"), 
					rs.getString("equivalent_cls"), 
					rs.getString("ontology"));
			list.add(equiv);
		}
		return list;
		
	}
	
	public List<Ontology> buildOntology() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");
		
		Vector<Ontology> list = new Vector<Ontology>();

		ResultSet rs = db.getDelaData("ontology");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			Ontology ont = goldObjFactory.getOntology(rs.getString("id"));
			list.add(ont);
		}
		return list;
		
	}
	
		
	
	public List<ObjAlternateId> buildObjAlternateId() throws SQLException{
		
		if(LOG.isDebugEnabled())
			LOG.debug("-");
		
		Vector<ObjAlternateId> list = new Vector<ObjAlternateId>();

		ResultSet rs = db.getDelaData("obj_alternate_id");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			ObjAlternateId ret = goldObjFactory.getObjAlternateId(rs.getString("obj"), rs.getString("id") );
			list.add(ret);
		}
		return list;
		
	}
	
	

	public List<RelationChain> buildRelationChain() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");
		
		Vector<RelationChain> list = new Vector<RelationChain>();

		ResultSet rs = db.getDelaData("relation_chain");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			RelationChain ret = goldObjFactory.getRelationChain(rs.getString("inferred_relation"),
					rs.getString("relation1"), rs.getString("relation2"));
			list.add(ret);
		}
		return list;
		
	}
	
	
	public List<AnnotationAssertion> buildAnnotationAssertion() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");
		
		Vector<AnnotationAssertion> list = new Vector<AnnotationAssertion>();

		ResultSet rs = db.getDelaData("annotation_assertion");
		
		if(rs == null)
		 	return list;
		
		while(rs.next()){
			AnnotationAssertion ret = goldObjFactory.getAnnotationAssertion(rs.getString("ontology"),
					rs.getString("obj"), rs.getString("target_obj"), rs.getString("relation"));
			list.add(ret);
		}
		return list;
		
	}
	
	
	public List<AnnotationProperty> buildAnnotationProperty() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");
		
		Vector<AnnotationProperty> list = new Vector<AnnotationProperty>();

		ResultSet rs = db.getDelaData("annotation_property");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			AnnotationProperty ret = goldObjFactory.getAnnotationProperty(rs.getString("id"));
			list.add(ret);
		}
		return list;
		
	}
	
	
	public List<OntologyAnnotation> buildOntologyAnnotation() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");
		
		Vector<OntologyAnnotation> list = new Vector<OntologyAnnotation>();

		ResultSet rs = db.getDelaData("ontology_annotation");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			OntologyAnnotation ont = goldObjFactory.getOntologyAnnotation(rs.getString("ontology"), 
					rs.getString("property"), rs.getString("annotation_value"));
			list.add(ont);
		}
		return list;
		
	}
	
	public List<OntologyImports> buildOntologyImports() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");
		
		Vector<OntologyImports> list = new Vector<OntologyImports>();

		ResultSet rs = db.getDelaData("ontology_imports");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			OntologyImports ont = goldObjFactory.getOntologyImports(rs.getString("ontology"), 
					rs.getString("imports_ontology"));
			list.add(ont);
		}
		return list;
		
	}
	
	public List<OntologySubset> buildOntologySubset() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");
		
		Vector<OntologySubset> list = new Vector<OntologySubset>();

		ResultSet rs = db.getDelaData("ontology_subset");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			OntologySubset ont = goldObjFactory.getOntologySubset(rs.getString("id"));
			list.add(ont);
		}
		return list;
		
	}


	public List<ObjSubset> buildObjSubset() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");
		
		Vector<ObjSubset> list = new Vector<ObjSubset>();

		ResultSet rs = db.getDelaData("obj_subset");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			ObjSubset ont = goldObjFactory.getObjSubset(rs.getString("obj"), rs.getString("ontology_subset"));
			list.add(ont);
		}
		return list;
		
	}
	
		

	public List<RelationDisjointWith> buildRelationDisjointWith() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");
		
		Vector<RelationDisjointWith> list = new Vector<RelationDisjointWith>();

		ResultSet rs = db.getDelaData("relation_disjoint_with");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			RelationDisjointWith disjoint = goldObjFactory.getRelationDisjointWith(rs.getString("relation"), 
					rs.getString("disjoint_relation"), 
					rs.getString("ontology"));
			list.add(disjoint);
		}
		return list;
		
	}
	
	
	public List<DisjointWith> buildDisjointWith() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");
		
		Vector<DisjointWith> list = new Vector<DisjointWith>();

		ResultSet rs = db.getDelaData("disjoint_with");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			DisjointWith disjoint = goldObjFactory.getDisjointWith(rs.getString("cls"), 
					rs.getString("disjoint_cls"), 
					rs.getString("ontology"));
			list.add(disjoint);
		}
		return list;
		
	}
	

	public List<ClsUnionOf> buildClsUnionOf() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");
		
		Vector<ClsUnionOf> list = new Vector<ClsUnionOf>();

		ResultSet rs = db.getDelaData("cls_union_of");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			ClsUnionOf disjoint = goldObjFactory.getClsUnionOf(rs.getString("cls"), 
					rs.getString("target_cls"), 
					rs.getString("ontology"));
			list.add(disjoint);
		}
		return list;
		
	}
	

	public List<ClsIntersectionOf> buildClsIntersectionOf() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");
		
		Vector<ClsIntersectionOf> list = new Vector<ClsIntersectionOf>();

		ResultSet rs = db.getDelaData("cls_intersection_of");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			ClsIntersectionOf disjoint = goldObjFactory.getClsIntersectionOf(rs.getString("cls"), 
					rs.getString("target_cls"), 
					rs.getString("ontology"));
			list.add(disjoint);
		}
		return list;
		
	}
	
	
	public List<InferredSubclassOf> buildInferredSubclassOf() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");
		
		Vector<InferredSubclassOf> list = new Vector<InferredSubclassOf>();

		ResultSet rs = db.getDelaData("inferred_subclass_of");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			InferredSubclassOf inferred = goldObjFactory.getInferredSubclassOf(rs.getString("cls"), 
					rs.getString("target_cls"), 
					rs.getString("ontology"));
			list.add(inferred);
		}
		return list;
		
	}
	
	public List<InferredAllSomeRelationship> buildInferredAllSomeRelationship() throws SQLException{
		if(LOG.isDebugEnabled())
			LOG.debug("-");
		
		Vector<InferredAllSomeRelationship> list = new Vector<InferredAllSomeRelationship>();

		ResultSet rs = db.getDelaData("inferred_all_some_relationship");
		
		if(rs == null)
			return list;
		
		while(rs.next()){
			InferredAllSomeRelationship inferred = goldObjFactory.getInferredAllSomeRelationship(rs.getString("cls"), 
					rs.getString("target_cls"), 
					rs.getString("relation"),
					rs.getString("ontology"));
			list.add(inferred);
		}
		return list;
		
	}

	
}
