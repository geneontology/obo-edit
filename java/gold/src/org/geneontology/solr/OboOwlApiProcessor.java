package org.geneontology.solr;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.log4j.Logger;
import org.apache.solr.handler.dataimport.Context;
import org.apache.solr.handler.dataimport.EntityProcessorBase;
import org.geneontology.web.services.GoldDbOperationsService;
import org.geneontology.web.services.ServicesConfig;
import org.semanticweb.owlapi.model.OWLClass;

import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.graph.OWLQuantifiedProperty;

/**
 * This import ontology from obo file into Solr. It uses OWL API with the
 * help of the {@code OWLGraphWrapper} to get the information (,e.g. id, label, synonyms) of each class in the ontology.  
 * The instance of this class is created by Solr. Its nextRow method is called repeatedly
 * during data import until null value is returned. The class is needed to be configured in the  
 * solr/go-owl-config.xml.
 * 
 * 
 * This file has to be packaged in a separate jar which is placed in the solr/lib directory. Use the target "make-solr-serv" in
 * the build.xml file to build the jar.
 * 
 * @author Shahid Manzoor
 *
 */
public class OboOwlApiProcessor extends EntityProcessorBase {

	
		private static Logger LOG = Logger.getLogger(OboOwlApiProcessor.class);
		private static boolean DEBUG = LOG.isDebugEnabled();
		
		
	  private boolean ended = false;

	  public void init(Context context) {
	    super.init(context);
	    ended = false;
	  }

	  
	  /**
	   * This method build the rowIterator object if it is not created. 
	   * This forward the call to getRow() method of the super class to
	   * return the data. The getRow method calls the next method of the rowIterator object to returns a 
	   * row object. This method is repeatedly called by Solr until null value is returned.
	   */
	  public Map<String, Object> nextRow() {

		if(rowIterator  == null && !ended){	  
			
			GoldDbOperationsService goldDb = (GoldDbOperationsService) ServicesConfig.getService("gold-db-operations");
			
			
			
			if(goldDb != null){
				OWLGraphWrapper graph = goldDb.getGraphWrapper();
				rowIterator = new RowsIterator(graph);
				
				System.out.println("****************Importinng ontology '"+ graph.getOntologyId() +"'****************");
			}
		      
		     ended = true;
		      
		}
		
		return getNext();
	    
	  }
	  
	  /**
	   * This iterators wraps around the Iterator<OWLClass>, and on each next call 
	   * it builds the instance of the class Map(String, Object>.
	   * @author Shahid Manzoor
	   *
	   */
	  protected class RowsIterator implements  Iterator<Map<String, Object>>{

		  
		  private OWLGraphWrapper wrapper;
		  
		  private Iterator<OWLClass> itr;
		  
		  RowsIterator(OWLGraphWrapper wrapper){
			  this.wrapper = wrapper;
			  
		  }
		  
//		@Override
		public boolean hasNext() {
			if(itr == null)
				itr = this.wrapper.getOntology().getClassesInSignature().iterator();

			
			return itr.hasNext();
		}

//		@Override
		public Map<String, Object> next() {
			if(hasNext()){
				OWLClass cls = itr.next();
				
				if(DEBUG)
					LOG.debug("Processing '" + cls + "' owl class for indexing");
				
				
		    	String id = wrapper.getIdentifier(cls);
		    	id = id.replace(':', '_');
		    	
		       	String label = wrapper.getLabel(cls);

		    	if(id == null || label ==  null){
		    		LOG.error("Id or label is not defined in the '" + cls + "' OWL class");
		    		//ignore this cls and move next
		    		return next();
		    	}
		       	
		       	
		       	String def = wrapper.getDef(cls);
		       	
		       	String[] synonyms = wrapper.getSynonymStrings(cls);
			
			    Map<String, Object> row = new HashMap<String, Object>();
			    row.put("document_category", "document_category");
			    row.put("id", id);
			    row.put("label", label);
			    row.put("description", def);
			    
			    if(synonyms != null && synonyms.length>0)
			    	row.put("synonym", synonyms);
			    
			    String[] altIds = wrapper.getAltId(cls);
			    if(altIds != null && altIds.length>0)
			    	row.put("alternate_id", altIds);
			    
			    String[] subsets = wrapper.getSubsets(cls);
			    
			    if(subsets != null && subsets.length>0)
			    	row.put("subset", subsets);
			    
			    boolean isObselete = wrapper.getIsObsolete(cls);
			    if(isObselete){
			    	row.put("is_obsolete", isObselete);
			    }
			    
			    String comment = wrapper.getComment(cls);
			    if(comment != null){
			    	row.put("comment", comment);
			    }

			    //getting values for the isa_partof_closure, isa_partof_label_closure and regulates_closure fields
				Set<OWLGraphEdge> outgoing = wrapper.getOutgoingEdges(cls);
				List<String> closureIdList = new ArrayList<String>();
				List<String> closureLabelList = new ArrayList<String>();
				List<String> regulatesList = new ArrayList<String>();
				List<String> regulatesLabelList = new ArrayList<String>();
				
				
				for(OWLGraphEdge edge: outgoing){
					
					if(edge.getQuantifiedPropertyList().size()>1){
					}else{
						OWLQuantifiedProperty prop = edge.getSingleQuantifiedProperty();
						String targetId = wrapper.getIdentifier( edge.getTarget() );
						String targetLabel = wrapper.getLabel(edge.getTarget());

						
						if( prop.getQuantifier() == OWLQuantifiedProperty.Quantifier.SOME ||
								prop.getQuantifier() == OWLQuantifiedProperty.Quantifier.SUBCLASS_OF ){
							
							if(targetId != null)
								closureIdList.add(targetId);
							
							if(targetLabel != null)
								closureLabelList.add(targetLabel);
							
							if(prop.getQuantifier() == OWLQuantifiedProperty.Quantifier.SOME){
								String propId = wrapper.getIdentifier(prop.getProperty());
							
								
								if("go:regulates".equals(propId)){

									if(targetId != null)
										regulatesList.add(targetId);
									if(targetLabel != null){
										regulatesLabelList.add(targetLabel);
									}
									
								}
							}
						}
						
					}
						
				}
				
				if(closureIdList.size()>0){
			    	row.put("isa_partof_closure", closureIdList.toArray());
				}
				
				if(closureLabelList.size()>0){
			    	row.put("isa_partof_label_closure", closureLabelList.toArray());
				}
			    
			    if(regulatesList.size()>0){
			    	row.put("regulates_closure", regulatesList.toArray());

			    }
			    
			    if(regulatesLabelList.size()>0){
			    	row.put("regulates_label_closure", regulatesLabelList.toArray());
			    }
				
			    return row;
				
			}
			
			return null;
		}

//		@Override
		public void remove() {
			//do nothing
			
		}
		  
	  }
	  
	 
}
