package org.obo.owl.dataadapter;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.bbop.dataadapter.AdapterConfiguration;
import org.bbop.dataadapter.DataAdapter;
import org.bbop.dataadapter.DataAdapterException;
import org.bbop.dataadapter.DataAdapterUI;
import org.bbop.dataadapter.DefaultIOOperation;
import org.bbop.dataadapter.FileAdapterConfiguration;
import org.bbop.dataadapter.FileAdapterUI;
import org.bbop.dataadapter.IOOperation;
import org.bbop.io.ProgressableInputStream;
import org.bbop.util.AbstractProgressValued;
import org.obo.annotation.datamodel.Annotation;
import org.obo.dataadapter.OBOSerializationEngine;
import org.obo.dataadapter.OBOFileAdapter.OBOAdapterConfiguration;
import org.obo.dataadapter.OBOSerializationEngine.FilteredPath;
import org.obo.datamodel.CommentedObject;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Instance;
import org.obo.datamodel.Link;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.ObjectFactory;
import org.obo.datamodel.PropertyValue;
import org.obo.datamodel.impl.DefaultObjectFactory;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.datamodel.impl.PropertyValueImpl;
import org.obo.owl.datamodel.MetadataMapping;
import org.obo.owl.util.IDSpaceRegistry;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.IDUtil;
import org.obo.util.TermUtil;
import org.semanticweb.owl.apibinding.OWLManager;
import org.semanticweb.owl.io.DefaultOntologyFormat;
import org.semanticweb.owl.model.AddAxiom;
import org.semanticweb.owl.model.OWLAnnotation;
import org.semanticweb.owl.model.OWLAnnotationAxiom;
import org.semanticweb.owl.model.OWLAxiom;
import org.semanticweb.owl.model.OWLClass;
import org.semanticweb.owl.model.OWLConstant;
import org.semanticweb.owl.model.OWLDataFactory;
import org.semanticweb.owl.model.OWLDescription;
import org.semanticweb.owl.model.OWLEntity;
import org.semanticweb.owl.model.OWLIndividual;
import org.semanticweb.owl.model.OWLObjectAllRestriction;
import org.semanticweb.owl.model.OWLObjectIntersectionOf;
import org.semanticweb.owl.model.OWLObjectProperty;
import org.semanticweb.owl.model.OWLObjectPropertyExpression;
import org.semanticweb.owl.model.OWLObjectSomeRestriction;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLOntologyChangeException;
import org.semanticweb.owl.model.OWLOntologyCreationException;
import org.semanticweb.owl.model.OWLOntologyFormat;
import org.semanticweb.owl.model.OWLOntologyManager;
import org.semanticweb.owl.model.OWLProperty;
import org.semanticweb.owl.model.OWLSubClassAxiom;
import org.semanticweb.owl.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owl.util.SimpleURIMapper;
import org.semanticweb.owl.vocab.OWLRDFVocabulary;
import org.apache.commons.lang.StringUtils;


/**
 * @author cjm
 */
public class OWLAdapter extends AbstractProgressValued implements DataAdapter {

	protected String path;

	protected ProgressableInputStream pfis;
	protected boolean cancelled = false;
	protected OWLAdapterConfiguration ioprofile;
	protected List streams = new LinkedList();
    OWLDataFactory owlFactory;
    URI ontologyURI;
    OWLOntology ontology;    
    OWLOntologyManager manager;
    
    public final static String PURL_OBO = "http://purl.org/obo/";
    
 
    OBOSession session;
    
	public static final IOOperation<OBOSession, OBOSession> WRITE_ONTOLOGY = new DefaultIOOperation<OBOSession, OBOSession>(
			"WRITE_ONTOLOGY", "write ontology", OBOSession.class,
			OBOSession.class);

	public static final IOOperation<Void, OBOSession> READ_ONTOLOGY = new DefaultIOOperation<Void, OBOSession>(
			"READ_ONTOLOGY", "read ontology", Void.class, OBOSession.class);

    
    public static class OWLAdapterConfiguration extends
    FileAdapterConfiguration {
    	protected boolean allowDangling = false;
    	
    	protected boolean allowLossy = false;
    
    	protected boolean failFast = false;

    	protected boolean saveImplied;

    	protected java.util.List saveRecords = new ArrayList();

    	protected boolean basicSave = true;

    	protected String serializer = "OBO_1_0";

    	protected String impliedType = "Save for presentation";
    	
    	protected OWLOntologyFormat ontologyFormat = new DefaultOntologyFormat();
    	
    	protected Set<MetadataMapping> metadataMappings = new HashSet<MetadataMapping>();

    	public OWLAdapterConfiguration() {
    	}

    	public void setSerializer(String serializer) {
    		this.serializer = serializer;
    	}

    	public void setFailFast(boolean failFast) {
    		this.failFast = failFast;
    	}

    	public boolean getFailFast() {
    		return failFast;
    	}

    	public String getSerializer() {
    		return serializer;
    	}

    	public boolean getBasicSave() {
    		return basicSave;
    	}

    	public void setBasicSave(boolean basicSave) {
    		this.basicSave = basicSave;
    	}

    	public java.util.List getSaveRecords() {
    		return saveRecords;
    	}

    	public void setSaveRecords(java.util.List saveRecords) {
    		if (saveRecords.contains(null))
    			(new Exception("Null save record added to profile"))
    			.printStackTrace();
    		this.saveRecords = saveRecords;
    	}

    	public boolean getAllowDangling() {
    		return allowDangling;
    	}

    	public void setAllowDangling(boolean allowDangling) {
    		this.allowDangling = allowDangling;
    	}

		public boolean isAllowLossy() {
			return allowLossy;
		}

		public void setAllowLossy(boolean allowLossy) {
			this.allowLossy = allowLossy;
		}

		public Set<MetadataMapping> getMetadataMappings() {
			return metadataMappings;
		}

		public void setMetadataMappings(Set<MetadataMapping> metadataMappings) {
			this.metadataMappings = metadataMappings;
		}
		public void addMetadataMapping(MetadataMapping metadataMapping) {
			this.metadataMappings.add(metadataMapping);
		}
		public OWLOntologyFormat getOntologyFormat() {
			return ontologyFormat;
		}

		public void setOntologyFormat(OWLOntologyFormat ontologyFormat) {
			this.ontologyFormat = ontologyFormat;
		}
		

   }
    
	public String getID() {
		return "OBO:OWLAdapter";
	}

	public String getName() {
		return "OBO OWL Adapter";
	}

	public IOOperation[] getSupportedOperations() {
		IOOperation[] supported = { READ_ONTOLOGY, WRITE_ONTOLOGY };
		return supported;
	}


	public DataAdapterUI getPreferredUI() {
		FileAdapterUI ui = new FileAdapterUI();
		ui.setReadOperation(READ_ONTOLOGY);
		ui.setWriteOperation(WRITE_ONTOLOGY);
		return ui;
	}

	public void cancel() {
		try {
			cancelled = true;
			if (pfis != null)
				pfis.close();
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	public AdapterConfiguration getConfiguration() {
		return ioprofile;
	}
	
	
	public <INPUT_TYPE, OUTPUT_TYPE> OUTPUT_TYPE doOperation(
			IOOperation<INPUT_TYPE, OUTPUT_TYPE> op,
			AdapterConfiguration configuration, INPUT_TYPE input)
			throws DataAdapterException {
		if (!(configuration instanceof OWLAdapterConfiguration)) {
			throw new DataAdapterException("Invalid configuration; this "
					+ "adapter requires an "
					+ "OWLAdapterConfiguration object.");
		}
		cancelled = false;
		this.ioprofile = (OWLAdapterConfiguration) configuration;
		
    	
		if (op.equals(READ_ONTOLOGY)) {
			OBOSession session = new DefaultObjectFactory().createSession();
			session.setDefaultNamespace(session.getObjectFactory().createNamespace("test", "test")); // TODO
			
			for (String f : ioprofile.getReadPaths()) {
				readOntology(session, f);
			}
			return (OUTPUT_TYPE) session;
		} else if (op.equals(WRITE_ONTOLOGY)) {
			java.util.List<FilteredPath> filteredPaths = new LinkedList<FilteredPath>();

			if (ioprofile.getBasicSave()) {
				filteredPaths.add(new OBOSerializationEngine.FilteredPath(
						null, null, ioprofile.getWritePath()));
			} else {
				System.err.println("gsr="+ioprofile.getSaveRecords());
				filteredPaths.addAll(ioprofile.getSaveRecords());
			}
			streams.clear();
			Iterator<FilteredPath> it = filteredPaths.iterator();
			this.session = (OBOSession)input;
			while (it.hasNext()) {
				FilteredPath filteredPath = it.next();
				writeAll((OBOSession) input, filteredPath.getPath(), filteredPath);
				return (OUTPUT_TYPE)input;
			}
		}
		return null;
	}
	
	public void readOntology(OBOSession session, String f) throws DataAdapterException {
		manager = OWLManager.createOWLOntologyManager();
		this.session = session; // TODO : check
		ObjectFactory oboFactory = session.getObjectFactory();
		try {
			System.out.println("f="+f);
			ontology = manager.loadOntologyFromPhysicalURI(URI.create("file:"+f));
			for (OWLClass owlClass : ontology.getReferencedClasses()) {
				
				String id = getOboID(owlClass.getURI());
				//System.out.println("read class:"+id+" " +owlClass);
				OBOClass oboClass = 
					(OBOClass)oboFactory.createObject(id, OBOClass.OBO_CLASS, false);
				session.addObject(oboClass);
				Set<OWLAnnotationAxiom> owlAnnotationAxioms = owlClass.getAnnotationAxioms(ontology);
				getMetadataFromAnnotationAxioms(oboClass,owlAnnotationAxioms);
				
				/* Fetch all necessary conditions
				 * In OWL, these are all subClass relations - between named classes
				 * or restrictions
				 */
				for (OWLSubClassAxiom axiom : ontology.getSubClassAxiomsForLHS(owlClass)) {
					OWLDescription owlSuper = axiom.getSuperClass();
					OBORestriction link = getOboLinkFromOWLDescription(owlSuper, oboClass);
					if (link == null) {
						String message = "Cannot convert OWLDescription: "+owlSuper;
						if (ioprofile.allowLossy) {
							System.err.println(message);
							continue;
						}
						else
							throw new DataAdapterException(message);				
					}
					oboClass.addParent(link);					
				}
				
				/* Find all N+S conditions
				 * 
				 */
				for (OWLEquivalentClassesAxiom axiom : 
						ontology.getEquivalentClassesAxioms(owlClass)) {
					Set<OWLDescription> ecDescs = axiom.getDescriptions();
					if (ecDescs.size() != 2) {
						System.err.println("all OWLEquivalentClassesAxiom must be pairs; got:"+ecDescs);
						continue; /// TODO. Register error
					}
					OWLDescription equivTo = null;
					for (OWLDescription d : ecDescs) {
						if (d.equals(owlClass))
							continue;
						if (equivTo != null) {
							System.err.println("all OWLEquivalentClassesAxiom must be pairs; got:"+ecDescs);
							continue; /// TODO. Register error
						}
						equivTo = d;
					}
					addDescription(oboClass,equivTo);
					
				}
				/* Find all N+S conditions
				 * 
				 */
				for (OWLDescription owlDisjointClass : owlClass.getDisjointClasses(ontology)) {
					OBOClass oboDisjointClass = getOboClass(owlDisjointClass);
					// all axioms are stored in OBO as "Restriction"s
					oboClass.addParent(new OBORestrictionImpl(oboClass,
							OBOProperty.DISJOINT_FROM,
							oboDisjointClass));
				}

			}
			
			/* properties
			 * 
			 */
			for (OWLProperty owlProperty : ontology.getReferencedObjectProperties()) {
				String id = getOboID(owlProperty);
				OBOProperty oboProperty =
					(OBOProperty)oboFactory.createObject(id, OBOClass.OBO_PROPERTY, false);
				session.addObject(oboProperty);
				Set<OWLAnnotationAxiom> owlAnnotationAxioms = owlProperty.getAnnotationAxioms(ontology);
				getMetadataFromAnnotationAxioms(oboProperty,owlAnnotationAxioms);			
				if (owlProperty.isFunctional(ontology)) {
					// TODO
				}
				if (owlProperty instanceof OWLObjectProperty) {
					OWLObjectProperty owlObjectProperty = (OWLObjectProperty)owlProperty;
					// TODO: check
					if (owlObjectProperty.isTransitive(ontology))
						oboProperty.setTransitive(true);
					if (owlObjectProperty.isSymmetric(ontology))
						oboProperty.setSymmetric(true);
					if (owlObjectProperty.isReflexive(ontology))
						oboProperty.setReflexive(true);
				}
					
			}
			/* instances
			 * 
			 */	
			for (OWLIndividual owlIndividual : ontology.getReferencedIndividuals()) {

				String id = getOboID(owlIndividual.getURI());
				System.out.println("read Individual:"+id+" " +owlIndividual);
				Instance oboInstance = 
					(Instance)oboFactory.createObject(id, OBOClass.OBO_INSTANCE, false);
				session.addObject(oboInstance);
				Set<OWLAnnotationAxiom> owlAnnotationAxioms = owlIndividual.getAnnotationAxioms(ontology);
				getMetadataFromAnnotationAxioms(oboInstance,owlAnnotationAxioms);				
			}
		} catch (OWLOntologyCreationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public void addDescription(OBOClass oboClass, OWLDescription owlDesc) throws DataAdapterException {
		if (owlDesc instanceof OWLObjectIntersectionOf) {
			Set<OWLDescription> elts = ((OWLObjectIntersectionOf)owlDesc).getOperands();
			for (OWLDescription d : elts) {
				OBORestriction link = getOboLinkFromOWLDescription(d,oboClass);
				if (link == null) {
					String message = "Cannot convert OWLDescription: "+owlDesc;
					if (ioprofile.allowLossy) {
						System.err.println(message);
						continue;
					}
					else
						throw new DataAdapterException(message);				
				}
				link.setCompletes(true);
				oboClass.addParent(link);
			}
		}
		else {
			
		}

	}
	
	public OBORestriction getOboLinkFromOWLDescription(OWLDescription desc, OBOClass oboClass) throws DataAdapterException {
		OBOProperty oboProp;
		OBOClass oboParentClass;
		ObjectFactory oboFactory = session.getObjectFactory();
		OBORestriction link;
		if (desc instanceof OWLClass) {
			oboProp = OBOProperty.IS_A;
			oboParentClass = getOboClass((OWLClass)desc);
			link = (OBORestriction)oboFactory.createOBORestriction(oboClass, oboProp, oboParentClass, false);
			return link;

		}
		else if (desc instanceof OWLObjectSomeRestriction) {
			OWLObjectSomeRestriction restr =
				(OWLObjectSomeRestriction)desc;
			oboProp = getOboProperty(restr.getProperty().asOWLObjectProperty());
			oboParentClass = getOboClass(restr.getFiller());
			link = oboFactory.createOBORestriction(oboClass, oboProp, oboParentClass, false);
			return link;

		}
		else if (desc instanceof OWLObjectAllRestriction) {
	
		}
		else {
			
		}
		return null;
	}
	
	public OBOClass getOboClass(OWLDescription owlDesc) throws DataAdapterException {
		OBOClass oboClass;
		if (owlDesc.isAnonymous()) {
			// TODO: Decide whether to go with temporary IDs or skolem IDs
			// An example of a skolem ID is GENUS^REL(DIFFERENTIUM_OBJ)
			String id = IDUtil.fetchTemporaryID(session);
			oboClass = (OBOClass)
			 session.getObjectFactory().createObject(id, OBOClass.OBO_CLASS, true);
			oboClass.setIsAnonymous(true);
			session.addObject(oboClass);
			addDescription(oboClass, owlDesc);
			return oboClass;
		}
		else {
			// is named
			OWLClass owlClass = (OWLClass)owlDesc;
			String id = getOboID(owlClass);
			IdentifiedObject o = session.getObject(id);
			
			if (o==null) {
				oboClass = (OBOClass) session.getObjectFactory().createObject(id, OBOClass.OBO_CLASS, false);
				session.addObject(oboClass);
			}
			else {
				oboClass = (OBOClass)o;
			}
			return oboClass;
		}
	}
	
	public String getOboID(OWLEntity oe) {
		return getOboID(oe.getURI());
	}
	
	public String getOboID(URI uri) {
		IDSpaceRegistry registry = IDSpaceRegistry.getInstance();
		String uriString = uri.toString();
		//System.out.println(" converting: "+uriString);
		if (uriString.startsWith(PURL_OBO)) {
			String[] idParts = 
				StringUtils.split(StringUtils.removeStart(uriString,PURL_OBO),
								  "#",2);
			if (idParts.length == 2) {
				return getOboID(idParts[0],idParts[1]);
			}
			// TODO
			System.err.println("URI "+uriString+" does not conform to purl.org/obo standard");
		}
		
		for (URI uriPrefix : registry.getUris()) {
			
			String uriPrefixString = uriPrefix.toString();
			if (uriString.startsWith(uriPrefixString)) {
				String localId = StringUtils.removeStart(uriString,uriPrefixString);
				String idspace = registry.getUriToIDSpace().get(uriPrefix);
				
				// register this IDspace in the actual OBO Session
				session.addIDSpace(idspace, uriPrefixString);
				return getOboID(idspace, localId);
			}
		}
		
		return uriString;
	}
	
	public String getOboID(String prefix, String localId) {
		String id;
		if (localId.startsWith(prefix+"_")) {
			localId = StringUtils.removeStart(localId, prefix+"_");
		}
		id = prefix + ":" + localId;
		//ystem.out.println("id="+id);
		return id;
	}
	

	

	

	

	
	public void getMetadataFromAnnotationAxioms(IdentifiedObject lo, 
			Set<OWLAnnotationAxiom> axioms) {
		for (OWLAnnotationAxiom axiom : axioms) {
			OWLAnnotation owlAnnot = axiom.getAnnotation();
			URI uri = owlAnnot.getAnnotationURI();
			String val = owlAnnot.getAnnotationValueAsConstant().getLiteral();
			if (uri.equals(OWLRDFVocabulary.RDFS_LABEL.getURI())) {
				lo.setName(val);
			}
		}
		Set<OWLAnnotationAxiom> unprocessedAxioms = new HashSet<OWLAnnotationAxiom>();
   		for (OWLAnnotationAxiom axiom : axioms) {
     		boolean isProcessed = false;
     		for (MetadataMapping mapping : ioprofile.getMetadataMappings()) {
     			if (mapping.translateOWLAxiom(axiom, lo))
     				isProcessed = true;
     		}
    		if (!isProcessed)
    			unprocessedAxioms.add(axiom);
    	}
   		
   		// Store any unprocessed axioms as PropertyValues
   		for (OWLAnnotationAxiom axiom : unprocessedAxioms) {
   			OWLAnnotation owlAnnot = axiom.getAnnotation();
   			URI uri = owlAnnot.getAnnotationURI();
   			
   			String propid = getOboID(uri);
   			OBOProperty prop = this.getOboProperty(propid);
   			String val = owlAnnot.getAnnotationValueAsConstant().getLiteral();
   			PropertyValue pv =
   				new PropertyValueImpl(propid,val);
   			// lo.addPropertyValue(pv); TODO
   		}
	}
	
	public OBOProperty getOboProperty(OWLProperty owlProp) {
		String id = getOboID(owlProp);
		return getOboProperty(id);
	}
	
	public OBOProperty getOboProperty(String id) {
		IdentifiedObject io = session.getObject(id);
		if (io == null) {
			return (OBOProperty)session.getObjectFactory().createObject(id, OBOClass.OBO_PROPERTY,
					false);
		}
		else {
			return TermUtil.castToProperty(io);
		}
	}


	public OBOSession writeAll(OBOSession session, String file, 
						FilteredPath filteredPath) throws DataAdapterException {
		try {
			setProgressString("Writing file...");
			
            // We first need to obtain a copy of an OWLOntologyManager, which, as the
            // name suggests, manages a set of ontologies.  An ontology is unique within
            // an ontology manager.  To load multiple copies of an ontology, multiple managers
            // would have to be used.
           manager = OWLManager.createOWLOntologyManager();

            // All ontologies have a URI, which is used to identify the ontology.  You should
            // think of the ontology URI as the "name" of the ontology.  This URI frequently
            // resembles a Web address (i.e. http://...), but it is important to realise that
            // the ontology URI might not necessarily be resolvable.  In other words, we
            // can't necessarily get a document from the URI corresponding to the ontology
            // URI, which represents the ontology.
            // In order to have a concrete representation of an ontology (e.g. an RDF/XML
            // file), we MAP the ontology URI to a PHYSICAL URI.  We do this using a URIMapper

            // We need to set up a mapping which points to a concrete file where the ontology will
            // be stored. (It's good practice to do this even if we don't intend to save the ontology).
            ontologyURI = URI.create("http://purl.org/obo/all");
            // Create a physical URI which can be resolved to point to where our ontology will be saved.
            // TODO: make this smarter. absolute vs relative path
            if (!file.contains("file:"))
            	file = "file:"+file;
            URI physicalURI = URI.create(file);
            // Set up a mapping, which maps the ontology URI to the physical URI
            SimpleURIMapper mapper = new SimpleURIMapper(ontologyURI, physicalURI);
            manager.addURIMapper(mapper);

            // Now create the ontology - we use the ontology URI (not the physical URI)
            ontology = manager.createOntology(ontologyURI);
            // Now we want to specify that A is a subclass of B.  To do this, we add a subclass
            // axiom.  A subclass axiom is simply an object that specifies that one class is a
            // subclass of another class.
            // We need a data factory to create various object from.  Each ontology has a reference
            // to a data factory that we can use.
            owlFactory = manager.getOWLDataFactory();
            
          for (IdentifiedObject io : session.getObjects()) {
                
        	  	if (io.isBuiltIn()) 
        	  		continue;
             	String id = io.getID();
            	if (io instanceof OBOClass) {
            		OBOClass oboClass = (OBOClass)io;
            		OWLClass owlClass = getOWLClass(io);
            		addOboMetadataToOwlEntity(owlClass,io);
                   
                    Set<OWLDescription> intersectionElements = new HashSet<OWLDescription>();
             		for (Link link : oboClass.getParents()) {
             			OWLClass owlParentClass = getOWLClass(link.getParent());
             			OBOProperty oboProp = link.getType();
         				if (oboProp.equals(OBOProperty.DISJOINT_FROM)) {
         					HashSet<OWLDescription> pair = 
         						new HashSet<OWLDescription>();
         					pair.add(owlClass);
         					pair.add(owlParentClass);
         					addAxiom(owlFactory.getOWLDisjointClassesAxiom(pair));
         				}
             			else {
                			OWLDescription owlSuperClass;
              
                   			if (oboProp.equals(OBOProperty.IS_A)) {
                 				owlSuperClass = owlParentClass;
                 			}
              				else {
             					OWLObjectProperty owlProp = getOWLObjectProperty(oboProp);
             					owlSuperClass = 
             						owlFactory.getOWLObjectSomeRestriction(owlProp, owlParentClass);
             				}

             				if (TermUtil.isIntersection(link)) {
             					intersectionElements.add(owlSuperClass);
             				}
             				else {
             					OWLAxiom axiom = 
             						owlFactory.getOWLSubClassAxiom(owlClass, owlSuperClass);
             					AddAxiom addAxiom = new AddAxiom(ontology, axiom);
             					// We now use the manager to apply the change
             					manager.applyChange(addAxiom);
             				}           
             			}
            		}
             		if (intersectionElements.size() > 0) {
             			OWLDescription owlIntersection =
             				owlFactory.getOWLObjectIntersectionOf(intersectionElements);
             			HashSet<OWLDescription> pair = new HashSet<OWLDescription>();
             			pair.add(owlClass);
             			pair.add(owlIntersection);
             			OWLAxiom ecAxiom =
             				owlFactory.getOWLEquivalentClassesAxiom(pair);
             			manager.applyChange(new AddAxiom(ontology,ecAxiom));
             		}
            	}
            	else if (io instanceof OBOProperty) {
            		OBOProperty oboProp = (OBOProperty)io;
            		OWLObjectProperty owlProp = getOWLObjectProperty(io);
               		addOboMetadataToOwlEntity(owlProp,io);
               		for (Link link : oboProp.getParents()) {
             			OWLObjectProperty owlParentProp = getOWLObjectProperty(link.getParent());
             			OBOProperty linkType = link.getType();
         				if (linkType.equals(OBOProperty.IS_A)) {
         					addAxiom(owlFactory.getOWLSubObjectPropertyAxiom(owlProp, owlParentProp));
         				}
         				else {
         					// TODO: throw?
         				}
               		}
               		if (oboProp.getDomain() != null) {
               			addAxiom(owlFactory.getOWLObjectPropertyDomainAxiom(owlProp,
               					getOWLClass(oboProp.getDomain())));
               		}
            	}
            	else if (io instanceof Annotation) {
            		addOboAnnotation((Annotation)io);
            	}
            	else if (io instanceof Instance) {
            		addInstance((Instance)io);
            	}
            	else {
            		// TODO
            	}
            }

            // Now save the ontology.  The ontology will be saved to the location where
            // we loaded it from, in the default ontology format
            manager.saveOntology(ontology, ioprofile.getOntologyFormat());
            //manager.saveOntology(ontology);
			return session;
		} catch (Exception e) {
			throw new DataAdapterException(e, "Write error");
		}
	}
	
	public void addAxiom(OWLAxiom axiom) throws OWLOntologyChangeException {
		manager.applyChange(new AddAxiom(ontology,axiom));
	}
	
	public void addOboMetadataToOwlEntity(OWLEntity owlEntity, IdentifiedObject io ) throws OWLOntologyChangeException {
        OWLConstant labelCon = owlFactory.getOWLUntypedConstant(io.getName());
        
        // name gets mapped to rdfs:label
        
        // The above constant is just a plain literal containing the version info text/comment
        // we need to create an annotation, which pairs a URI with the constant
        OWLAnnotation anno = 
        	owlFactory.getOWLConstantAnnotation(OWLRDFVocabulary.RDFS_LABEL.getURI(), labelCon);

        addAxiom(owlFactory.getOWLEntityAnnotationAxiom(owlEntity, anno));
        
        if (ioprofile.getMetadataMappings() != null) {
        	for (MetadataMapping mapping : ioprofile.getMetadataMappings()) {
        		for (OWLAxiom axiom : mapping.getOWLAxioms(this,owlEntity,io))
        			addAxiom(axiom);
        		// TODO: unconsumed
        	}
        }
	}
	
	public void addOboAnnotation(Annotation oboAnnotation) throws OWLOntologyChangeException {
		addInstance(oboAnnotation);
		if (oboAnnotation.getSubject() != null)
			addTriple(oboAnnotation,"oban:has_subject",oboAnnotation.getSubject());
		if (oboAnnotation.getSubject() != null)
			addTriple(oboAnnotation,"oban:has_predicate",oboAnnotation.getRelationship());
		if (oboAnnotation.getSubject() != null)
			addTriple(oboAnnotation,"oban:has_object",oboAnnotation.getObject());
	}
	
	public void addTriple(IdentifiedObject su, OBOProperty prop, IdentifiedObject ob) throws OWLOntologyChangeException {
		OWLIndividual owlSu = getOWLIndividual(su);
		OWLIndividual owlOb = getOWLIndividual(ob);
		OWLObjectProperty owlProp = getOWLObjectProperty(prop);
		addAxiom(owlFactory.getOWLObjectPropertyAssertionAxiom(owlSu, owlProp, owlOb));
		
	}
	
	public void addTriple(IdentifiedObject su, String propId, IdentifiedObject ob) throws OWLOntologyChangeException {
		OWLIndividual owlSu = getOWLIndividual(su);
		OWLIndividual owlOb = getOWLIndividual(ob);
		OWLObjectProperty owlProp = getOWLObjectProperty(propId);
		addAxiom(owlFactory.getOWLObjectPropertyAssertionAxiom(owlSu, owlProp, owlOb));	
	}
	
	public void addInstance(Instance oboInst) throws OWLOntologyChangeException {
		OWLIndividual owlIndividual = getOWLIndividual(oboInst);
		if (oboInst.getType() != null) {
			addAxiom(owlFactory.getOWLClassAssertionAxiom(owlIndividual, 
					getOWLClass(oboInst.getType())));
		}
		addOboMetadataToOwlEntity(owlIndividual,oboInst);
		for (Link link : oboInst.getParents()) {
			addTriple(oboInst,link.getType(),link.getParent());
		}
	}
	
	

	public URI getURI(IdentifiedObject io) {
		return getURI(io.getID());
	}
	
	public URI getURI(Dbxref x) {
		return getURI(x.getDatabase() + ":" + x.getDatabaseID());
	}


	
	public URI getURI(String id) {
		//System.out.println("getting uri for "+id);
		String[] idParts = StringUtils.split(id,":",2);
		String db;
		String localId;
		if (idParts.length > 1) {
			db = idParts[0];
			localId = idParts[1];
		}
		else if (idParts.length == 0) {
			db = "_global";
			localId = id;
		}
		else { // ==1
			db = "_global";
			localId = idParts[0];
		}
		String safeId;
		try {
			 safeId = java.net.URLEncoder.encode(localId,"US-ASCII");
		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			safeId = "";
		}
		return URI.create("http://purl.org/obo/" + db + "#" + db + "_" + safeId); // TODO
	}

	
	public OWLClass getOWLClass(IdentifiedObject io) {
		return owlFactory.getOWLClass(getURI(io)); 
	}
	

	
	public OWLIndividual getOWLIndividual(IdentifiedObject io) {
		return owlFactory.getOWLIndividual(getURI(io));
	}
	
	public OWLObjectProperty getOWLObjectProperty(IdentifiedObject io) {
		return owlFactory.getOWLObjectProperty(getURI(io));
	}
	public OWLObjectProperty getOWLObjectProperty(String id) {
		return owlFactory.getOWLObjectProperty(getURI(id));
	}

	public OWLDataFactory getOwlFactory() {
		return owlFactory;
	}

	public void setOwlFactory(OWLDataFactory owlFactory) {
		this.owlFactory = owlFactory;
	}



	

}
