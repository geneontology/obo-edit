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
import java.util.Map;
import java.util.Set;

import org.apache.solr.client.solrj.SolrServer;
import org.apache.solr.client.solrj.SolrServerException;
import org.apache.solr.client.solrj.impl.CommonsHttpSolrServer;
import org.apache.solr.common.SolrInputDocument;
import org.geneontology.solr.OboOwlApiProcessor;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.UnknownOWLOntologyException;

import owltools.graph.OWLGraphWrapper;

/**
 * This class uses solrj api to build index on the solr server.
 * @author Shahid Manzoor
 */
public class OntSolrLoader extends OboOwlApiProcessor {

    private SolrServer server;
    
    private Collection<SolrInputDocument> docs = null;
  
    public OntSolrLoader(String url){
        try{
            server = new CommonsHttpSolrServer(url);
        }catch(Exception ex){
            throw new RuntimeException("Cann't instantiate Solr Server", ex);
        }
    }
    
    public void load() throws SolrServerException, IOException{
    	
    	docs = new ArrayList<SolrInputDocument>();
    	
    	Map<String, Object> row = null;
    	while((row = nextRow()) != null){
            SolrInputDocument doc = new SolrInputDocument();
    		for(String key: row.keySet()){
    			doc.addField(key, row.get(key));
    		}
    		
           	docs.add(doc);
    	}
    	
    	
    	server.add(docs);
    	
    	server.commit();
   	
    	
    }
    


}
