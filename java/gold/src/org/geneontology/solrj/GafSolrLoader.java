package org.geneontology.solrj;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.apache.log4j.Logger;
import org.apache.solr.client.solrj.SolrServer;
import org.apache.solr.client.solrj.impl.CommonsHttpSolrServer;
import org.apache.solr.common.SolrInputDocument;
import org.geneontology.gaf.hibernate.Bioentity;
import org.geneontology.gaf.hibernate.CompositeQualifier;
import org.geneontology.gaf.hibernate.ExtensionExpression;
import org.geneontology.gaf.hibernate.GafDocument;
import org.geneontology.gaf.hibernate.GeneAnnotation;
import org.geneontology.gaf.hibernate.WithInfo;
import org.geneontology.gold.io.GoldIOException;
import org.geneontology.web.services.GoldDbOperationsService;
import org.geneontology.web.services.ServicesConfig;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLObject;

import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.graph.OWLQuantifiedProperty;

public class GafSolrLoader {

	private static Logger LOG = Logger.getLogger(GafSolrLoader.class);
	
    private SolrServer server;
    
    private Collection<SolrInputDocument> docs = null;
  
    public GafSolrLoader(String url){
        try{
            server = new CommonsHttpSolrServer(url);
        }catch(Exception ex){
        	LOG.error(ex.getMessage(), ex);
            throw new RuntimeException("Cann't instantiate Solr Server", ex);
        }
    }

	public void load(GafDocument doc) throws GoldIOException{
		
    	docs = new ArrayList<SolrInputDocument>();

		GoldDbOperationsService goldDb = (GoldDbOperationsService) ServicesConfig.getService("gold-db-operations");
		OWLGraphWrapper wrapper = goldDb.getGraphWrapper();
    	
		for(GeneAnnotation a: doc.getGeneAnnotations()){
            SolrInputDocument d = new SolrInputDocument();
            
            Bioentity bioentity = a.getBioentityObject();

            //building id field value
            String id = bioentity.getId();
            if(a.getCls() != null)
            	id += a.getCls();
            if(a.getReferenceId() != null)
            	id += a.getReferenceId();
            if(a.getAssignedBy() != null)
            	id += a.getAssignedBy();
            if(a.getEvidenceCls() != null)
            	id += a.getEvidenceCls();
            
            
            String bioentityId = bioentity.getId();
            
            String bioentityLabel = bioentity.getSymbol();
            
            String type = bioentity.getTypeCls();
            
            OWLClass type_cls= wrapper.getOWLClass(type);
            String type_label = wrapper.getLabel(type_cls);

//            String withExpression = a.getWithExpression();
            
 //           String compositeQualifier = a.getCompositeQualifier();
            
            String referenceId = a.getReferenceId();
            
            String source = a.getAssignedBy();

            String date = a.getLastUpdateDate();
            
    //        String extensionExpression = a.getExtensionExpression();
            
            String taxon = bioentity.getNcbiTaxonId();
            
            taxon = taxon.replace("NCBITaxon:", "taxon:");
            
            String annotationClass = a.getCls();
            
            OWLClass clsLabel = wrapper.getOWLClass(a.getCls());
            
            String clsLabelString = wrapper.getLabel(clsLabel);
            
            String evidenceType = a.getEvidenceCls();
            
            List withInfos = new ArrayList<String>();

            for(WithInfo withInfo: a.getWithInfos()){
            	withInfos.add(withInfo.getWithXref());
            }
            
            
            List qualifiers = new ArrayList<String>();
            
            
            for(CompositeQualifier q: a.getCompositeQualifiers()){
            	qualifiers.add(q.getQualifierObj());
            }
            
            List isaPartofClosure = new ArrayList<String>();
            List isaPartofClosureLabel = new ArrayList<String>();

            List regulatesClosure = new ArrayList<String>();
            List regulatesClosureLabel = new ArrayList<String>();
            
            
            for(OWLGraphEdge edge: wrapper.getOutgoingEdges(clsLabel)){
				OWLQuantifiedProperty prop = edge.getSingleQuantifiedProperty();
				String targetId = wrapper.getIdentifier( edge.getTarget() );
				String targetLabel = wrapper.getLabel(edge.getTarget());

				
				if( prop.getQuantifier() == OWLQuantifiedProperty.Quantifier.SUBCLASS_OF  ){
					isaPartofClosure.add(targetId);
					isaPartofClosureLabel.add(targetLabel);
					
				}
				
				if(prop.getQuantifier() == OWLQuantifiedProperty.Quantifier.SOME){
					String propId = wrapper.getIdentifier(prop.getProperty());
					
					if("go:part_of".equals(propId)){
						isaPartofClosure.add(targetId);
						isaPartofClosureLabel.add(targetLabel);
					}else if("go:regulates".equals(propId)){
						regulatesClosure.add(targetId);
						regulatesClosureLabel.add(targetLabel);
					}
				}
           	
            }
            
            List annotationExtensionClass = new ArrayList<String>();
            List annotationExtensionClassLabel = new ArrayList<String>();

            List annotationExtensionClassClosure = new ArrayList<String>();
            List annotationExtensionClassClosureLabel = new ArrayList<String>();

            for(ExtensionExpression ex: a.getExtensionExpressions()){
            	annotationExtensionClass.add(ex.getCls());
            	OWLObject clsObj = wrapper.getOWLObject(ex.getCls());
            	String l = wrapper.getLabel(clsObj);
            	
            	if(l != null){
            		annotationExtensionClassLabel.add(l);
            	}
            	
            	
                for(OWLGraphEdge edge: wrapper.getOutgoingEdges(clsObj)){
    				OWLQuantifiedProperty prop = edge.getSingleQuantifiedProperty();
    				String targetId = wrapper.getIdentifier( edge.getTarget() );
    				String targetLabel = wrapper.getLabel(edge.getTarget());

    				
    				if( prop.getQuantifier() == OWLQuantifiedProperty.Quantifier.SUBCLASS_OF  ){
    					annotationExtensionClassClosure.add(targetId);
    					annotationExtensionClassClosureLabel.add(targetLabel);
    					
    				}
                }
            }
            
            
            
            
            d.addField("document_category", "annotation");
            
            d.addField("id", id);
            
            if(bioentityId != null)
            	d.addField("bioentity_id", bioentityId);
            
            if(bioentityLabel != null)
            d.addField("bioentity_label", bioentityLabel);
            
            if(type != null)
            	d.addField("type", type);
            
            if(type_label != null)
            	d.addField("type_label", type_label);
            
        //    if(withExpression != null)
          //  	d.addField("with_expression", withExpression);
            
//            if(compositeQualifier != null)
//            	d.addField("composite_qualifier", compositeQualifier);
            
            if(referenceId != null)
            	d.addField("reference", referenceId);
            
            if(source !=null)
            	d.addField("source", source);
            
            if(date != null)
            	d.addField("date", date);

        //    if(extensionExpression != null)
          //  	d.addField("extension_expression", extensionExpression);
            
            if(taxon != null)
            	d.addField("taxon", taxon);
            
            if(annotationClass != null)
            	d.addField("annotation_class", annotationClass);
            
            if(clsLabelString != null)
            	d.addField("annotation_class_label", clsLabelString);
            
            if(evidenceType != null)
            	d.addField("evidence_type", evidenceType);

            
            if(withInfos.size()>0)
            	d.addField("evidence_with", withInfos.toArray());
            
            if(qualifiers.size()>0){
            	d.addField("qualifier", qualifiers.toArray());
            }
            
            
            if(isaPartofClosure.size()>0){
            	d.addField("isa_partof_closure", isaPartofClosure.toArray());
            }
            
            if(isaPartofClosureLabel.size()>0){
            	d.addField("isa_partof_label_closure", isaPartofClosureLabel.toArray());
            }
            
            if(regulatesClosure.size()>0){
            	d.addField("regulates_closure", regulatesClosure.toArray());
            }
            
            if(regulatesClosureLabel.size()>0){
            	d.addField("regulates_label_closure", regulatesClosureLabel.toArray());
            }
            
            if(annotationExtensionClass.size()>0){
            	d.addField("annotation_extension_class", annotationExtensionClass.toArray());
            }
            
            if(annotationExtensionClassLabel.size()>0){
            	d.addField("annotation_extension_class_label", annotationExtensionClassLabel.toArray());
            }
            
            if(annotationExtensionClassClosure.size()>0){
            	d.addField("annotation_extension_class_closure", annotationExtensionClassClosure.toArray());
            }
            
            if(annotationExtensionClassClosureLabel.size()>0){
            	d.addField("annotation_extension_class_label_closure", annotationExtensionClassClosureLabel.toArray());
            }

            docs.add(d);
		}
		
		try{
			server.add(docs);
			server.commit();
		}catch(Exception ex){
			throw new GoldIOException(ex);
		}
	}
}
