/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.geneontology.solrj;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Set;

import org.apache.solr.client.solrj.SolrServer;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CommonsHttpSolrServer;
import org.apache.solr.common.SolrInputDocument;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;

import owltools.graph.OWLGraphWrapper;

/**
 *
 * @author Shahid Manzoor
 * @deprecated This class is used to load Solr indexer through
 * 		Solr (client side API). The class is not used any more. 
 * 		It is replaced by {@link org.geneontology.solr.OboOwlApiProcessor}
 */
@Deprecated
public class SolrLoader {

    private SolrServer server;
    
    private OWLGraphWrapper owlGraphWrapper;

    private Collection<SolrInputDocument> docs = null;
  
    
    public SolrLoader(String url){
        try{
            server = new CommonsHttpSolrServer(url);
        }catch(Exception ex){
            throw new RuntimeException("Cann't instantiate Solr Server", ex);
        }
    }
    
    public void load(String file) throws OWLOntologyCreationException, SolrServerException, IOException{
    	load(new File(file));
    }
    
    public void load(File file) throws OWLOntologyCreationException, SolrServerException, IOException{
		OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
		load (manager.loadOntologyFromOntologyDocument(file));
    	
    }

    public void load(OWLOntology ontology) throws SolrServerException, IOException{
    	load(new OWLGraphWrapper(ontology));
    }
    
    public void load(OWLGraphWrapper wrapper) throws SolrServerException, IOException{
    	this.owlGraphWrapper = wrapper;
    	
    	docs = new ArrayList<SolrInputDocument>();
    	
    	OWLOntology ontology = wrapper.getOntology();
    	
    	Set<OWLClass> set = ontology.getClassesInSignature();
    	
    	for(OWLClass cls: set){
    		load(cls);
    	}
    	
    	server.add(docs);
    	
    	server.commit();
    }
    
    
    
    private void load(OWLClass cls){
    	
    	String id = owlGraphWrapper.getIdentifier(cls);
    	id = id.replace(':', '_');
    	
    	System.out.println(id);
    	
       	String label = owlGraphWrapper.getLabel(cls);

    	if(id == null || label ==  null){
    		System.err.println("Id or label is not defined in the '" + cls + "' OWL class");
    		return;
    	}
       	
       	
       	String def = owlGraphWrapper.getDef(cls);
       	String[] synonyms = owlGraphWrapper.getSynonymStrings(cls);
       	
        SolrInputDocument doc = new SolrInputDocument();
        doc.addField("id", id);
        doc.addField("label", label);
        doc.addField("description", def);
        doc.addField("synonym", synonyms);
       	
       	docs.add(doc);
       	
    	
    }


    
    
    


}
