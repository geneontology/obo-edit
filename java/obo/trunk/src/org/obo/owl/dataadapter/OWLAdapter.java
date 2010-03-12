package org.obo.owl.dataadapter;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang.StringUtils;
import org.bbop.dataadapter.AdapterConfiguration;
import org.bbop.dataadapter.DataAdapter;
import org.bbop.dataadapter.DataAdapterException;
import org.bbop.dataadapter.DataAdapterUI;
import org.bbop.dataadapter.DataAdapterUIException;
import org.bbop.dataadapter.DefaultIOOperation;
import org.bbop.dataadapter.FileAdapterConfiguration;
import org.bbop.dataadapter.FileAdapterUI;
import org.bbop.dataadapter.GraphicalUI;
import org.bbop.dataadapter.IOOperation;
import org.bbop.io.ProgressableInputStream;
import org.bbop.util.AbstractProgressValued;
import org.coode.manchesterowlsyntax.ManchesterOWLSyntaxOntologyFormat;
import org.coode.obo.parser.OBOOntologyFormat;
import org.obo.annotation.datamodel.Annotation;
import org.obo.dataadapter.OBOSerializationEngine;
import org.obo.dataadapter.OBOSerializationEngine.FilteredPath;
import org.obo.datamodel.Datatype;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Instance;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.NestedValue;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOObject;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.OBORestriction;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.ObjectFactory;
import org.obo.datamodel.PropertyValue;
import org.obo.datamodel.Value;
import org.obo.datamodel.ValueLink;
import org.obo.datamodel.impl.DatatypeValueImpl;
import org.obo.datamodel.impl.DefaultObjectFactory;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.datamodel.impl.PropertyValueImpl;
import org.obo.owl.datamodel.MetadataMapping;
import org.obo.owl.util.IDSpaceRegistry;
import org.obo.util.IDUtil;
import org.obo.util.TermUtil;
import org.semanticweb.owl.apibinding.OWLManager;
import org.semanticweb.owl.io.DefaultOntologyFormat;
import org.semanticweb.owl.io.OWLFunctionalSyntaxOntologyFormat;
import org.semanticweb.owl.io.OWLXMLOntologyFormat;
import org.semanticweb.owl.io.RDFXMLOntologyFormat;
import org.semanticweb.owl.model.AddAxiom;
import org.semanticweb.owl.model.OWLAnnotation;
import org.semanticweb.owl.model.OWLAnnotationAxiom;
import org.semanticweb.owl.model.OWLAxiom;
import org.semanticweb.owl.model.OWLAxiomAnnotationAxiom;
import org.semanticweb.owl.model.OWLClass;
import org.semanticweb.owl.model.OWLConstant;
import org.semanticweb.owl.model.OWLConstantAnnotation;
import org.semanticweb.owl.model.OWLDataFactory;
import org.semanticweb.owl.model.OWLDataPropertyExpression;
import org.semanticweb.owl.model.OWLDescription;
import org.semanticweb.owl.model.OWLEntity;
import org.semanticweb.owl.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owl.model.OWLException;
import org.semanticweb.owl.model.OWLIndividual;
import org.semanticweb.owl.model.OWLObjectAllRestriction;
import org.semanticweb.owl.model.OWLObjectCardinalityRestriction;
import org.semanticweb.owl.model.OWLObjectExactCardinalityRestriction;
import org.semanticweb.owl.model.OWLObjectIntersectionOf;
import org.semanticweb.owl.model.OWLObjectMaxCardinalityRestriction;
import org.semanticweb.owl.model.OWLObjectMinCardinalityRestriction;
import org.semanticweb.owl.model.OWLObjectProperty;
import org.semanticweb.owl.model.OWLObjectPropertyExpression;
import org.semanticweb.owl.model.OWLObjectSomeRestriction;
import org.semanticweb.owl.model.OWLObjectUnionOf;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLOntologyChangeException;
import org.semanticweb.owl.model.OWLOntologyCreationException;
import org.semanticweb.owl.model.OWLOntologyFormat;
import org.semanticweb.owl.model.OWLOntologyManager;
import org.semanticweb.owl.model.OWLProperty;
import org.semanticweb.owl.model.OWLSubClassAxiom;
import org.semanticweb.owl.util.SimpleURIMapper;
import org.semanticweb.owl.vocab.OWLRDFVocabulary;


/**
 * @author cjm
 */
import org.apache.log4j.*;

public class OWLAdapter extends AbstractProgressValued implements DataAdapter {

	//initialize logger
	protected final static Logger logger = Logger.getLogger("OWLAdapter.class");

	protected String path;

	protected ProgressableInputStream pfis;
	protected boolean cancelled = false;
	protected OWLAdapterConfiguration ioprofile;
	protected GraphicalUI advancedUI;
	protected List streams = new LinkedList();
	protected OWLDataFactory owlFactory;
	protected URI ontologyURI;
	protected OWLOntology ontology;    
	protected OWLOntologyManager manager;
	protected Map<OWLDescription,OBOClass> description2oboclass = 
		new HashMap<OWLDescription,OBOClass>();


	public final static String PURL_OBO = "http://purl.org/obo/";
	public final static String PURL_OBO_OWL = "http://purl.org/obo/owl/";


	OBOSession session;

	private Object owlProp;

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

		protected boolean combineOWLOntologies = true;

		protected List<FilteredPath> saveRecords = new ArrayList<FilteredPath>();

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

		public String getSerializer() {
			return serializer;
		}

		public boolean getBasicSave() {
			return basicSave;
		}

		public void setBasicSave(boolean basicSave) {
			this.basicSave = basicSave;
		}

		public List<FilteredPath> getSaveRecords() {
			return saveRecords;
		}

		public void setSaveRecords(List<FilteredPath> saveRecords) {
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

		/**
		 * Not all OWL Constructs can be converted at this time. For example
		 * universal restrictions.
		 * 
		 * @param allowLossy -- if false, then untranslatable constructs will be fatal
		 */
		public void setAllowLossy(boolean allowLossy) {
			this.allowLossy = allowLossy;
		}

		public boolean isCombineOWLOntologies() {
			return combineOWLOntologies;
		}

		/**
		 * OWL Ontologies can be declared to import other ontologies. The OWLAPI
		 * will by default follow imports and load these ontologies into memory. However,
		 * each is still treated as a separate ontology.
		 * If this toggle is set to true, then all these ontologies will be translated
		 * into one OBO Session (and thus, when saved, into a single OBO file)
		 * @param combineOWLOntologies
		 */
		public void setCombineOWLOntologies(boolean combineOWLOntologies) {
			this.combineOWLOntologies = combineOWLOntologies;
		}

		public Set<MetadataMapping> getMetadataMappings() {
			return metadataMappings;
		}

		public void setMetadataMappings(Set<MetadataMapping> metadataMappings) {
			this.metadataMappings = metadataMappings;
		}

		/**
		 * Different ontologies use different annotation properties for
		 * encoding synonyms, definitions, obsolete classes etc. This
		 * must be set explicitly
		 * 
		 * @param metadataMapping
		 */
		public void addMetadataMapping(MetadataMapping metadataMapping) {
			this.metadataMappings.add(metadataMapping);
		}
		public OWLOntologyFormat getOntologyFormat() {
			return ontologyFormat;
		}

		public void setOntologyFormat(OWLOntologyFormat ontologyFormat) {
			this.ontologyFormat = ontologyFormat;
		}

		/**
		 * The OWL language has a number of different concrete serializations
		 * 
		 * @param owlFormat -- one of owlxml, owlfunctionalsyntax, manchesterowlsyntax, rdfxml
		 * @throws DataAdapterException
		 */
		public void setOntologyFormat(String owlFormat) throws DataAdapterException {
			if (owlFormat.equals("owlxml"))
				ontologyFormat = new OWLXMLOntologyFormat();
			else if (owlFormat.equals("owlfunctionalsyntax"))
				ontologyFormat = new OWLFunctionalSyntaxOntologyFormat();
			else if (owlFormat.equals("manchesterowlsyntax"))
				ontologyFormat = new ManchesterOWLSyntaxOntologyFormat();
			else if (owlFormat.equals("obo"))
				ontologyFormat = new OBOOntologyFormat();
			else if (owlFormat.equals("default") || owlFormat.equals("rdfxml"))
				ontologyFormat = new RDFXMLOntologyFormat();
			else
				throw new DataAdapterException("invalid owl ontology format: "+owlFormat);
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

		FileAdapterUI ui = new FileAdapterUI() {

			/**
			 * 
			 */
			private static final long serialVersionUID = 8709597443707849569L;

			@Override
			public AdapterConfiguration createEmptyConfig() {
				return new OWLAdapterConfiguration();
			}

			@Override
			public void acceptComponentConfig(boolean storeonly)
			throws DataAdapterUIException {
				super.acceptComponentConfig(storeonly);
				((OWLAdapterConfiguration) config).setBasicSave(true);
			}

		};
		ui.setReadOperation(READ_ONTOLOGY);
		ui.setWriteOperation(WRITE_ONTOLOGY);
		GraphicalUI advancedUI = getAdvancedUI();
		if (advancedUI != null) {
			advancedUI.setSimpleUI(ui);
			ui.setAdvancedUI(advancedUI);
		}
		return ui;
	}

	// TODO - DRY
	public GraphicalUI getAdvancedUI() {
		return advancedUI;
	}
	public void setAdvancedUI(GraphicalUI advancedUI) {
		this.advancedUI = advancedUI;
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

	public void fireLossyWarning(String message, OBOObject obj) throws DataAdapterException {
		fireLossyWarning(message+" object:"+obj);
	}
	public void fireLossyWarning(String message) throws DataAdapterException {
		if (ioprofile.allowLossy) {
			logger.error(message);
			// TODO capture errors
		}
		else
			throw new DataAdapterException(message+
			" -- set allowLossy to true to ignore this message");				
	}

	public AdapterConfiguration getConfiguration() {
		return ioprofile;
	}
	public void setConfiguration(OWLAdapterConfiguration config) {
		ioprofile = config;
	}


	public <INPUT_TYPE, OUTPUT_TYPE> OUTPUT_TYPE doOperation(
			IOOperation<INPUT_TYPE, OUTPUT_TYPE> op,
			AdapterConfiguration configuration, INPUT_TYPE input)
	throws DataAdapterException {
		if (!(configuration instanceof OWLAdapterConfiguration)) {
			logger.info("conf="+configuration.getClass());
			throw new DataAdapterException(" - Invalid configuration; this "
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
				logger.info("getSaveRecprds="+ioprofile.getSaveRecords());
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
		URI uri;
		if (f.indexOf(":") > 0)  // already has scheme
			uri = URI.create(f);
		else // file path
			uri = URI.create("file:" + f);

		logger.info("reading OWL ontology from URI " + uri);
		try {
			ontology = manager.loadOntologyFromPhysicalURI(uri);
		} //
		catch (Exception e) {
			// TODO Auto-generated catch block
			logger.info("Error reading OWL ontology from " + uri + ": "+e);
			e.printStackTrace();
			throw new DataAdapterException("error reading OWL ontology from " + uri + ": "+e);
		}
		if (ioprofile.isCombineOWLOntologies()) {
			for (OWLOntology ont : manager.getOntologies()) {
				this.ontology = ont;
				logger.info("ontology: "+ont);
				transformToOBOSession(ontology, session);
			}
		}
		else {
			transformToOBOSession(ontology, session);			
		}


	}

	public void transformToOBOSession(OWLOntology ontology, OBOSession session) throws DataAdapterException {
		ObjectFactory oboFactory = session.getObjectFactory();

		/* instances
		 * 
		 */	
		for (OWLIndividual owlIndividual : ontology.getReferencedIndividuals()) {

			String id = getOboID(owlIndividual.getURI());
			Instance oboInstance = 
				(Instance)oboFactory.createObject(id, OBOClass.OBO_INSTANCE, false);
			session.addObject(oboInstance);
			Map<OWLObjectPropertyExpression, Set<OWLIndividual>> opvs = 
				owlIndividual.getObjectPropertyValues(ontology);
			for (OWLObjectPropertyExpression owlProp : opvs.keySet()) {
				if (owlProp instanceof OWLProperty) {
					OBOProperty oboProp = this.getOboProperty((OWLProperty)owlProp);
					for (OWLIndividual owlRefIndiv : opvs.get(owlProp)) {
						String toID = this.getOboID(owlRefIndiv);
						Instance oboRefInst = this.getOboInstance(toID);
						if (!(oboRefInst instanceof Value)) {
						}
						else  {
							oboInstance.addPropertyValue(oboProp, oboRefInst);
							logger.debug("PV: "+oboProp+" = "+oboRefInst);
						}
					}
				}
			}
			Map<OWLDataPropertyExpression, Set<OWLConstant>> dpvs = 
				owlIndividual.getDataPropertyValues(ontology);
			for (OWLDataPropertyExpression owlProp : dpvs.keySet()) {
				if (owlProp instanceof OWLProperty) {
					OBOProperty oboProp = this.getOboProperty((OWLProperty)owlProp);
					for (OWLConstant owlConst : dpvs.get(owlProp)) {

						DatatypeValueImpl dv = new DatatypeValueImpl(Datatype.STRING,
								owlConst.getLiteral());
						//logger.info("dv="+dv+" "+oboProp);
						oboInstance.addPropertyValue(oboProp,dv);
						logger.debug("PV: "+oboProp+" = "+dv);
					}
				}

			}
			Set<OWLDescription> owlTypes = owlIndividual.getTypes(ontology);
			for (OWLDescription owlDesc : owlTypes) {
				OBOClass oboClass = getOboClass(owlDesc);
				oboInstance.setType(oboClass);
			}
			if (owlTypes.size() > 1) {
				this.fireLossyWarning("multiple instance_of links set (obo allows only one for now)",oboInstance);
			}

			Set<OWLAnnotationAxiom> owlAnnotationAxioms = owlIndividual.getAnnotationAxioms(ontology);
			getMetadataFromAnnotationAxioms(oboInstance,owlAnnotationAxioms);				
		}

		/* classes
		 */
		for (OWLClass owlClass : ontology.getReferencedClasses()) {

			String id = getOboID(owlClass.getURI());
			//logger.info("read class:"+id+" " +owlClass);
			OBOClass oboClass = 
				(OBOClass)oboFactory.createObject(id, OBOClass.OBO_CLASS, false);
			session.addObject(oboClass);
		}
		
		for (OWLClass owlClass : ontology.getReferencedClasses()) {
			String id = getOboID(owlClass.getURI());
			OBOClass oboClass = (OBOClass) session.getObject(id);
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
					fireLossyWarning("Cannot convert OWLDescription: "+owlSuper);
					continue;						
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
					fireLossyWarning("all OWLEquivalentClassesAxiom must be pairs; got:"+ecDescs);
					continue; 
				}
				OWLDescription equivTo = null;
				for (OWLDescription d : ecDescs) {
					if (d.equals(owlClass))
						continue;
					if (equivTo != null) {
						fireLossyWarning("all OWLEquivalentClassesAxiom must be pairs; got:"+ecDescs);
						continue; 
					}
					equivTo = d;
				}
				addDescription(oboClass,equivTo);

			}
			/* Find all disjoint classes
			 * 
			 */
			for (OWLDescription owlDisjointClass : owlClass.getDisjointClasses(ontology)) {
				OBOClass oboDisjointClass = getOboClass(owlDisjointClass);
				if (oboDisjointClass != null) { 
					// all axioms are stored in OBO as "Restriction"s
					oboClass.addParent(new OBORestrictionImpl(oboClass,
							OBOProperty.DISJOINT_FROM,
							oboDisjointClass));
				}
				else {
					fireLossyWarning("disjointWith axioms must be to named classes. Can't handle: "+oboClass+" with "+owlDisjointClass);
				}
			}

		}

		/* properties
		 * 
		 */
		for (OWLProperty owlProperty : ontology.getReferencedDataProperties()) {
			String id = getOboID(owlProperty);
			OBOProperty oboProperty =
				(OBOProperty)oboFactory.createObject(id, OBOClass.OBO_PROPERTY, false);
			session.addObject(oboProperty);
			Set<OWLAnnotationAxiom> owlAnnotationAxioms = owlProperty.getAnnotationAxioms(ontology);
			getMetadataFromAnnotationAxioms(oboProperty,owlAnnotationAxioms);			

		}
		for (OWLProperty owlProperty : ontology.getReferencedObjectProperties()) {
			// TODO - DRY
			String id = getOboID(owlProperty);
			OBOProperty oboProperty =
				(OBOProperty)oboFactory.createObject(id, OBOClass.OBO_PROPERTY, false);
			session.addObject(oboProperty);
			Set<OWLAnnotationAxiom> owlAnnotationAxioms = owlProperty.getAnnotationAxioms(ontology);
			getMetadataFromAnnotationAxioms(oboProperty,owlAnnotationAxioms);			
			if (owlProperty.isFunctional(ontology)) {
				// TODO
				//oboProperty.setFunctional(true);
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

		/* everything else
		 * e.g. - OboInOWL creates fake superclasses
		 */
		for (MetadataMapping mapping : ioprofile.getMetadataMappings()) {
			mapping.translateGraph(session);
		}

		TermUtil.resolveDanglingLinks(session);


	}

	public void addDescription(OBOClass oboClass, OWLDescription owlDesc) throws DataAdapterException {
		if (owlDesc instanceof OWLObjectIntersectionOf) {
			Set<OWLDescription> elts = ((OWLObjectIntersectionOf)owlDesc).getOperands();
			for (OWLDescription d : elts) {
				OBORestriction link = getOboLinkFromOWLDescription(d,oboClass);
				if (link == null) {
					String message = "Cannot convert OWLDescription: "+d+" -- in "+owlDesc;
					if (ioprofile.allowLossy) {
						logger.info(message);
						continue;
					}
					else
						throw new DataAdapterException(message);				
				}
				link.setCompletes(true);
				oboClass.addParent(link);
			}
		}
		else if (owlDesc instanceof OWLObjectUnionOf) {
			Set<OWLDescription> elts = ((OWLObjectUnionOf)owlDesc).getOperands();
			for (OWLDescription d : elts) {
				OBORestriction link = getOboLinkFromOWLDescription(d,oboClass);
				if (link == null) {
					String message = "Cannot convert OWLDescription: "+d+" -- in "+owlDesc;
					if (ioprofile.allowLossy) {
						logger.info(message);
						continue;
					}
					else
						throw new DataAdapterException(message);				
				}
				if (link.getType().equals(OBOProperty.IS_A)) {
					link.setType(OBOProperty.UNION_OF);
					oboClass.addParent(link);
				}
				else {
					fireLossyWarning("cannot deal with "+link+" in UnionOf "+owlDesc);
				}
			}
		}
		else {

		}

	}

	public OBORestriction getOboLinkFromOWLDescription(OWLDescription desc, OBOClass oboClass) throws DataAdapterException {
		OBOProperty oboProp;
		OBOClass oboParentClass;
		ObjectFactory oboFactory = session.getObjectFactory();
		OBORestriction link = null;
		if (desc instanceof OWLClass) {
			oboProp = OBOProperty.IS_A;
			oboParentClass = getOboClass((OWLClass)desc);
			link = (OBORestriction)oboFactory.createOBORestriction(oboClass, oboProp, oboParentClass, false);
			logger.debug("subclass of named class: "+desc+" :: "+link);
		}
		else if (desc instanceof OWLObjectSomeRestriction) {
			OWLObjectSomeRestriction restr =
				(OWLObjectSomeRestriction)desc;
			oboProp = getOboProperty(restr.getProperty().asOWLObjectProperty());
			oboParentClass = getOboClass(restr.getFiller());
			link = oboFactory.createOBORestriction(oboClass, oboProp, oboParentClass, false);
			logger.debug("subclass of existential restriction: "+desc+" :: "+link);
		}
		else if (desc instanceof OWLObjectAllRestriction) {
			logger.debug("subclass of universal restriction: "+desc+" :: "+link);
			//			TODO - obof1.3
		}
		else if (desc instanceof OWLObjectCardinalityRestriction) {
			OWLObjectCardinalityRestriction restr =
				(OWLObjectCardinalityRestriction)desc;
			oboProp = getOboProperty(restr.getProperty().asOWLObjectProperty());
			oboParentClass = getOboClass(restr.getFiller());
			int card = restr.getCardinality();

			link = oboFactory.createOBORestriction(oboClass, oboProp, oboParentClass, false);
			if (restr instanceof OWLObjectMinCardinalityRestriction) {
				link.setMinCardinality(card);
			}
			else if (restr instanceof OWLObjectMaxCardinalityRestriction) {
				link.setMaxCardinality(card);
			}
			else if (restr instanceof OWLObjectExactCardinalityRestriction) {
				link.setCardinality(card);
			}
			else {
				fireLossyWarning("unknown class of OWLObjectCardinalityRestriction: "+desc);
			}
			logger.debug("subclass of QCR: "+desc+" :: "+link);

		}
		else if (desc instanceof OWLObjectIntersectionOf) {
			oboProp = OBOProperty.IS_A;
			oboParentClass = getOboClass(desc);
			// oboRestrictions are not owl restrictions!!
			link = (OBORestriction)oboFactory.createOBORestriction(oboClass, oboProp, oboParentClass, false);
			logger.debug("subclass of intersection: "+desc+" :: "+link);
		}
		else {

		}
		if (link != null) {
			if (link.getParent() == null) {
				fireLossyWarning("incomplete link: "+link+"; from conversion of: "+desc);
				link = null;
			}

		}
		else {

		}
		return link;
	}

	public OBOClass getOboClass(OWLDescription owlDesc) throws DataAdapterException {
		OBOClass oboClass;
		if (description2oboclass.containsKey(owlDesc)) {
			return description2oboclass.get(owlDesc);
		}
		else if (owlDesc.isAnonymous()) {
			// TODO: Decide whether to go with temporary IDs or skolem IDs
			// An example of a skolem ID is GENUS^REL(DIFFERENTIUM_OBJ)
			String id = IDUtil.fetchTemporaryID(session);
			oboClass = (OBOClass)
			session.getObjectFactory().createObject(id, OBOClass.OBO_CLASS, true);
			oboClass.setIsAnonymous(true);
			session.addObject(oboClass);
			addDescription(oboClass, owlDesc);
			description2oboclass.put(owlDesc,oboClass);
			return oboClass;
		}
		else {
			// is named
			OWLClass owlClass = (OWLClass)owlDesc;
			String id = getOboID(owlClass);
			IdentifiedObject o = session.getObject(id);

			if (o==null) {
				oboClass = 
					TermUtil.castToClass(session.getObjectFactory().createDanglingObject(id, false));

				//oboClass = (OBOClass) session.getObjectFactory().createObject(id, OBOClass.OBO_CLASS, false);
				session.addObject(oboClass);
				//logger.info("dangling="+id+" "+oboClass);
			}
			else {
				if (o instanceof OBOClass)
					oboClass = (OBOClass)o;
				else {
					fireLossyWarning("don't know what to do with "+owlDesc+"; expected class, got: "+o);
					return null;
				}
			}
			description2oboclass.put(owlDesc,oboClass);

			return oboClass;
		}
	}

	public String getOboID(OWLEntity oe) {
		return getOboID(oe.getURI());
	}

	public String getOboID(URI uri) {
		IDSpaceRegistry registry = IDSpaceRegistry.getInstance();
		String uriString = uri.toString();
		//logger.info(" converting: "+uriString);
		if (uriString.startsWith(PURL_OBO_OWL)) {
			String[] idParts = 
				StringUtils.split(StringUtils.removeStart(uriString,PURL_OBO_OWL),
						"#",2);
			if (idParts.length == 2) {
				return getOboID(idParts[0],idParts[1]);
			}
			// TODO
			logger.info("URI "+uriString+" does not conform to purl.org/obo standard");
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
		// flat IDspaces get mapped to obo; eg:
		// http://purl.org/obo/owl/obo#develops_from
		if (prefix.equals("obo"))
			return localId;
		if (localId.startsWith(prefix+"_")) {
			localId = StringUtils.removeStart(localId, prefix+"_");
		}
		id = prefix + ":" + localId;
		//ystem.out.println("id="+id);
		return id;
	}









	public void getMetadataFromAnnotationAxioms(IdentifiedObject lo, 
			Set<OWLAnnotationAxiom> axioms) {
		Set<OWLAnnotationAxiom> unprocessedAxioms = new HashSet<OWLAnnotationAxiom>();
		for (OWLAnnotationAxiom axiom : axioms) {
			boolean isProcessed = false;
			OWLAnnotation owlAnnot = axiom.getAnnotation();
			if (owlAnnot instanceof OWLConstantAnnotation) {
				URI uri = owlAnnot.getAnnotationURI();
				String val = owlAnnot.getAnnotationValueAsConstant().getLiteral();
				if (uri.equals(OWLRDFVocabulary.RDFS_LABEL.getURI())) {
					lo.setName(val);
					isProcessed = true;
				}
			}
			if (!isProcessed) {
				for (MetadataMapping mapping : ioprofile.getMetadataMappings()) {

					if (mapping.translateOWLAxiom(axiom, lo, this))
						isProcessed = true;
				}
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
			PropertyValueImpl pv;
			if (owlAnnot instanceof OWLConstantAnnotation) {
				// TODO -- allow for proper property value handling in org.obo
				if (false) {
					String val = owlAnnot.getAnnotationValueAsConstant().getLiteral();
					pv = new PropertyValueImpl(propid,val);
				}
				if (true) {
					String val = owlAnnot.getAnnotationValueAsConstant().getLiteral();
					pv = new PropertyValueImpl("property_value",propid+" "+val);

				}
				lo.addPropertyValue(pv);
			}
		}
	}

	public OBOProperty getOboProperty(OWLProperty owlProp) {
		String id = getOboID(owlProp);
		return getOboProperty(id);
	}


	public OBOProperty getOboProperty(String id) {
		IdentifiedObject io = session.getObject(id);
		if (io == null) {
			return
			TermUtil.castToProperty(session.getObjectFactory().createDanglingObject(id, true));
			//			return (OBOProperty)session.getObjectFactory().createObject(id, OBOClass.OBO_PROPERTY,
			//			false);
		}
		else {
			return TermUtil.castToProperty(io);
		}
	}
	public Instance getOboInstance(String id) {
		IdentifiedObject io = session.getObject(id);
		if (io == null) {
			return
			TermUtil.castToInstance(session.getObjectFactory().createDanglingObject(id, true));
			//			return (OBOProperty)session.getObjectFactory().createObject(id, OBOClass.OBO_PROPERTY,
			//			false);
		}
		else {
			return TermUtil.castToInstance((LinkedObject)io);
		}
	}


	public OBOSession writeAll(OBOSession session, String file, 
			FilteredPath filteredPath) throws DataAdapterException {
		// We first need to obtain a copy of an OWLOntologyManager, which, as the
		// name suggests, manages a set of ontologies.  An ontology is unique within
		// an ontology manager.  To load multiple copies of an ontology, multiple managers
		// would have to be used.
		manager = OWLManager.createOWLOntologyManager();

		// Create a physical URI which can be resolved to point to where our ontology will be saved.
		// TODO: make this smarter. absolute vs relative path
		URI physicalURI;
		if (!file.contains("file:"))
			file = "file:"+file;
		physicalURI = URI.create(file);
		System.err.println("writing:"+physicalURI+" -- from "+file);


		// We need to set up a mapping which points to a concrete file where the ontology will
		// be stored. (It's good practice to do this even if we don't intend to save the ontology).
		ontologyURI = URI.create("http://purl.org/obo/all");


		// Set up a mapping, which maps the ontology URI to the physical URI
		SimpleURIMapper mapper = new SimpleURIMapper(ontologyURI, physicalURI);
		manager.addURIMapper(mapper);

		try {
			setProgressString("Writing file...");
			OWLOntology owlOntology = obo2owl(session, ontologyURI);
			// Now save the ontology.  The ontology will be saved to the location where
			// we loaded it from, in the default ontology format
			manager.saveOntology(ontology, ioprofile.getOntologyFormat());
			//manager.saveOntology(ontology);
			return session;

		}
		catch (Exception e) {
			e.printStackTrace();
			throw new DataAdapterException(e, "Write error");
		}
	}
	public OWLOntology obo2owl(OBOSession session, URI ontologyURI) throws DataAdapterException {

		try {
			// All ontologies have a URI, which is used to identify the ontology.  You should
			// think of the ontology URI as the "name" of the ontology.  This URI frequently
			// resembles a Web address (i.e. http://...), but it is important to realise that
			// the ontology URI might not necessarily be resolvable.  In other words, we
			// can't necessarily get a document from the URI corresponding to the ontology
			// URI, which represents the ontology.
			// In order to have a concrete representation of an ontology (e.g. an RDF/XML
			// file), we MAP the ontology URI to a PHYSICAL URI.  We do this using a URIMapper

			// Now create the ontology - we use the ontology URI (not the physical URI)
			System.err.println("creating ontology:"+ontologyURI);
			ontology = manager.createOntology(ontologyURI);
			// Now we want to specify that A is a subclass of B.  To do this, we add a subclass
			// axiom.  A subclass axiom is simply an object that specifies that one class is a
			// subclass of another class.
			// We need a data factory to create various object from.  Each ontology has a reference
			// to a data factory that we can use.
			owlFactory = manager.getOWLDataFactory();


			for (IdentifiedObject io : session.getObjects()) {
				// TODO
				if (io instanceof Annotation) {
					Annotation a = (Annotation)io;
					Link link = a.getPositedLink();
				}
			}
			for (IdentifiedObject io : session.getObjects()) {

				if (io.isBuiltIn()) 
					continue;
				String id = io.getID();
				try {
					if (io instanceof OBOClass) {
						OBOClass oboClass = (OBOClass)io;
						OWLClass owlClass = getOWLClass(io);
						addOboMetadataToOwlEntity(owlClass,io);

						//if (io.isAnonymous())
						//	addAxiom(owlFactory.

						Set<OWLDescription> intersectionElements = new HashSet<OWLDescription>();
						Set<OWLDescription> unionElements = new HashSet<OWLDescription>();
						for (Link link : oboClass.getParents()) {

							OWLClass owlParentClass = null;
							try {
								owlParentClass = getOWLClass(link.getParent());
							}
							catch (Exception e) {
								e.printStackTrace();
							}
							if (owlParentClass == null) {
								// TODO
								continue;
							}
							OBOProperty oboProp = link.getType();
							NestedValue nv = link.getNestedValue();
							if (oboProp.equals(OBOProperty.DISJOINT_FROM)) {
								HashSet<OWLDescription> pair = 
									new HashSet<OWLDescription>();
								pair.add(owlClass);
								pair.add(owlParentClass);
								addAxiom(owlFactory.getOWLDisjointClassesAxiom(pair));
							}
							else if (TermUtil.isUnion(link)) {
								unionElements.add(owlParentClass);
							}
							else {
								Collection<OWLDescription> owlSuperClasses =
									new LinkedList<OWLDescription>();
								//Map<OWLDescription,NestedValue> class2nv = 
								//	new HashMap<OWLDescription,NestedValue>();
								if (oboProp.equals(OBOProperty.IS_A)) {
									owlSuperClasses.add(owlParentClass);
								}
								else {
									OWLObjectProperty owlProp = getOWLObjectProperty(oboProp);
									owlSuperClasses.add( 
											owlFactory.getOWLObjectSomeRestriction(owlProp, owlParentClass));
									if (oboProp.isUniversallyQuantified()) {
										owlSuperClasses.add( 
												owlFactory.getOWLObjectAllRestriction(owlProp, owlParentClass));

									}
								}

								if (TermUtil.isIntersection(link)) {
									for (OWLDescription owlSuperClass : owlSuperClasses)
										intersectionElements.add(owlSuperClass);
								}
								else {
									for (OWLDescription owlSuperClass : owlSuperClasses) {
										OWLAxiom axiom = 
											owlFactory.getOWLSubClassAxiom(owlClass, owlSuperClass);
										AddAxiom addAxiom = new AddAxiom(ontology, axiom);
										// We now use the manager to apply the change
										manager.applyChange(addAxiom);

										if (nv != null) {
											Set<PropertyValue> pvs = nv.getPropertyValues();
											for (PropertyValue pv : pvs) {
												OWLConstant con = 
													owlFactory.getOWLUntypedConstant(pv.getValue());
												URI predURI = getURI(pv.getProperty());
												OWLConstantAnnotation owlAnnot = owlFactory.getOWLConstantAnnotation(predURI,
														con);
												OWLAxiomAnnotationAxiom aaa = owlFactory.getOWLAxiomAnnotationAxiom(axiom, 
														owlAnnot);
												AddAxiom addAAA = new AddAxiom(ontology, aaa);
												manager.applyChange(addAAA);
											}
										}

									}
								}           
							}
						}
						
						if (intersectionElements.size() > 1) {
							OWLDescription owlIntersection =
								owlFactory.getOWLObjectIntersectionOf(intersectionElements);
							HashSet<OWLDescription> pair = new HashSet<OWLDescription>();
							pair.add(owlClass);
							pair.add(owlIntersection);
							OWLAxiom ecAxiom =
								owlFactory.getOWLEquivalentClassesAxiom(pair);
							manager.applyChange(new AddAxiom(ontology,ecAxiom));
						}
						if (unionElements.size() > 1) {
							OWLDescription owlUnion =
								owlFactory.getOWLObjectUnionOf(unionElements);
							HashSet<OWLDescription> pair = new HashSet<OWLDescription>();
							pair.add(owlClass);
							pair.add(owlUnion);
							OWLAxiom ecAxiom =
								owlFactory.getOWLEquivalentClassesAxiom(pair);
							manager.applyChange(new AddAxiom(ontology,ecAxiom));
						}
					}
					else if (io instanceof OBOProperty) {
						OBOProperty oboProp = (OBOProperty)io;
						if (oboProp.isNonInheritable()) {
							// TODO Annotation Property
						}
						else {
							OWLObjectProperty owlProp;
							owlProp = getOWLObjectProperty(io);
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
							if (oboProp.getTransitiveOver() != null) {
								List<OWLObjectProperty> chain = new ArrayList<OWLObjectProperty>();
								chain.add(owlProp);
								chain.add(getOWLObjectProperty(oboProp.getTransitiveOver()));
								OWLAxiom ax = 
									owlFactory.getOWLObjectPropertyChainSubPropertyAxiom(chain, owlProp);
								addAxiom(ax);
							}
							if (oboProp.getDomain() != null) {
								addAxiom(owlFactory.getOWLObjectPropertyDomainAxiom(owlProp,
										getOWLClass(oboProp.getDomain())));
							}
							// TODO symmetric
							if (oboProp.isTransitive()) // TODO
								addAxiom(owlFactory.getOWLTransitiveObjectPropertyAxiom(owlProp));
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

				catch (UnsupportedEncodingException e) {
					e.printStackTrace();
					fireLossyWarning("problem with "+io.getID());
				}
			}
		}
		catch (OWLException e) {
			e.printStackTrace();
			throw new DataAdapterException(e, "Write error");		
		}
		return ontology;
	}

	public void addAxiom(OWLAxiom axiom) throws OWLOntologyChangeException {
		manager.applyChange(new AddAxiom(ontology,axiom));
	}

	public void addOboMetadataToOwlEntity(OWLEntity owlEntity, IdentifiedObject io ) throws OWLOntologyChangeException {
		if (io.getName() != null) {
			OWLConstant labelCon = owlFactory.getOWLUntypedConstant(io.getName());

			// name gets mapped to rdfs:label

			// The above constant is just a plain literal containing the version info text/comment
			// we need to create an annotation, which pairs a URI with the constant
			OWLAnnotation anno = 
				owlFactory.getOWLConstantAnnotation(OWLRDFVocabulary.RDFS_LABEL.getURI(), labelCon);

			addAxiom(owlFactory.getOWLEntityAnnotationAxiom(owlEntity, anno));
		}
		if (ioprofile != null && ioprofile.getMetadataMappings() != null) {
			for (MetadataMapping mapping : ioprofile.getMetadataMappings()) {
				for (OWLAxiom axiom : mapping.getOWLAxioms(this,owlEntity,io))
					addAxiom(axiom);
				// TODO: unconsumed
			}
		}
	}

	public void addOboAnnotation(Annotation oboAnnotation) throws OWLOntologyChangeException, DataAdapterException {
		addInstance(oboAnnotation);
		// TODO: change from reification to AxiomAnnotationAxioms
		// can we annotate facts?
		if (oboAnnotation.getSubject() != null)
			addTriple(oboAnnotation,"oban:has_subject",oboAnnotation.getSubject());
		if (oboAnnotation.getRelationship() != null)
			addTriple(oboAnnotation,"oban:has_predicate",oboAnnotation.getRelationship());
		if (oboAnnotation.getObject() != null)
			addTriple(oboAnnotation,"oban:has_object",oboAnnotation.getObject());

		/*
		Link alink = oboAnnotation.getPositedLink();
		aaa =
		getAxiomAnnotationAxiom(axiom,OWLRDFVocabulary.RDFS_SEE_ALSO.getURI(),
				adapter.getURI(x)
		 */
	}

	public void addTriple(IdentifiedObject su, OBOProperty prop, IdentifiedObject ob) throws OWLOntologyChangeException, DataAdapterException {
		try {
			OWLIndividual owlSu = getOWLIndividual(su);
			OWLIndividual owlOb = getOWLIndividual(ob);
			OWLObjectProperty owlProp = getOWLObjectProperty(prop);
			addAxiom(owlFactory.getOWLObjectPropertyAssertionAxiom(owlSu, owlProp, owlOb));
		} catch (UnsupportedEncodingException e) {
			fireLossyWarning("Cannot convert propertyId: ",prop);
		}

	}

	public void addTriple(IdentifiedObject su, String propId, IdentifiedObject ob) throws OWLOntologyChangeException, DataAdapterException {
		try {
			OWLIndividual owlSu = getOWLIndividual(su);
			OWLIndividual owlOb = getOWLIndividual(ob);
			OWLObjectProperty owlProp = getOWLObjectProperty(propId);
			addAxiom(owlFactory.getOWLObjectPropertyAssertionAxiom(owlSu, owlProp, owlOb));	
		} catch (UnsupportedEncodingException e) {
			fireLossyWarning("Cannot convert propertyId: "+propId);
		}
	}

	public void addInstance(Instance oboInst) throws OWLOntologyChangeException, DataAdapterException {
		try {
			OWLIndividual owlIndividual = getOWLIndividual(oboInst);
			if (oboInst.getType() != null) {
				OWLClass owlClass;
				try {
					owlClass = getOWLClass(oboInst.getType());
					addAxiom(owlFactory.getOWLClassAssertionAxiom(owlIndividual, 
							owlClass));
				} catch (UnsupportedEncodingException e) {
					e.printStackTrace();
					fireLossyWarning("cannot convert to class");
				}
			}
			addOboMetadataToOwlEntity(owlIndividual,oboInst);
			for (Link link : oboInst.getParents()) {
				if (link instanceof ValueLink) {
					ValueLink vl = (ValueLink)link;
					String message = "cannot handle value links (datatype properties) yet";
					if (ioprofile.allowLossy) {
						logger.info(message);
						continue;
					}
					else
						throw new DataAdapterException(message+
						" -- set allowLossy to true to ignore this message");				

					/** TODO
					Object dt = owlFactory.getOWLDataType(vl.getValue().getType().toString());
					Object owlConst = 
						owlFactory.getOWLTypedConstant(vl.getValue().toString(),


					OWLIndividual owlSu = getOWLIndividual(oboInst);
					OWLObjectProperty owlProp = getOWLObjectProperty(link.getType());


					addAxiom(owlFactory.
							getOWLDataPropertyAssertionAxiom(owlSu, 
									owlProp, 
									owlFactory.getOWLTypedConstant(((ValueLink)link).getValue().toString())));
					 **/

				}
				else {
					addTriple(oboInst,link.getType(),link.getParent());
				}
			}

		} catch (UnsupportedEncodingException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
			fireLossyWarning("cannot convert to class");
		}
	}



	public URI getURI(IdentifiedObject io) throws UnsupportedEncodingException {

		return getURI(io.getID());
	}

	public URI getURI(Dbxref x) throws UnsupportedEncodingException {
		return getURI(x.getDatabase() + ":" + x.getDatabaseID());
	}

	public String getDefaultIDSpace() {
		//return "_global";
		return "obo";
	}

	public URI getURI(String id) throws UnsupportedEncodingException {
		//logger.info("getting uri for "+id);
		if (id.contains(" ")) {
			throw new UnsupportedEncodingException();
		}
		String[] idParts = StringUtils.split(id,":",2);
		String db;
		String localId;
		if (idParts.length > 1) {
			db = idParts[0];
			localId = idParts[1];
		}
		else if (idParts.length == 0) {
			db = getDefaultIDSpace();
			localId = id;
		}
		else { // ==1
			db = getDefaultIDSpace();
			localId = idParts[0];
		}
		if (db.equals("http")) { // TODO - roundtrip from other schemes
			return URI.create(id);
		}

		IDSpaceRegistry registry = IDSpaceRegistry.getInstance();

		for (URI uriPrefix : registry.getUris()) {
			if (db.equals(registry.getIDSpace(uriPrefix))) {
				String uriPrefixString = uriPrefix.toString();
				return URI.create(uriPrefixString+localId);
			}
		}

		//db = java.net.URLEncoder.encode(db,"US-ASCII");

		String safeId;
		safeId = java.net.URLEncoder.encode(localId,"US-ASCII");
		if (safeId.matches("^\\d.*")) {
			safeId = db+"_"+safeId;
		}
		if (safeId.contains(" "))
			safeId = safeId.replace(" ", "_");
		URI uri = null;
		try {
			uri = URI.create(PURL_OBO_OWL + db + "#" + safeId); // TODO
		} catch (IllegalArgumentException e) {
			// TODO - define new exception class for this
			throw new UnsupportedEncodingException();
		}

		//		} catch (Exception e) {
		//		logger.info(db+" : "+safeId + " is not safe!");
		//		e.printStackTrace();
		//		uri = URI.create(PURL_OBO_OWL + db + "#_"); // TODO
		//		}

		return  uri;
		//return URI.create("http://purl.org/obo/" + db + "#" + db + "_" + safeId); // TODO
	}


	public OWLClass getOWLClass(IdentifiedObject io) throws UnsupportedEncodingException {
		return owlFactory.getOWLClass(getURI(io)); 
	}



	public OWLIndividual getOWLIndividual(IdentifiedObject io) throws UnsupportedEncodingException {
		URI uri = getURI(io);
		if (uri == null) {
			logger.info("no URI for "+io);
		}
		return owlFactory.getOWLIndividual(uri);
	}

	public OWLObjectProperty getOWLObjectProperty(IdentifiedObject io) throws UnsupportedEncodingException {
		return owlFactory.getOWLObjectProperty(getURI(io));
	}
	public OWLObjectProperty getOWLObjectProperty(String id) throws UnsupportedEncodingException {
		return owlFactory.getOWLObjectProperty(getURI(id));
	}


	public OWLDataFactory getOwlFactory() {
		return owlFactory;
	}

	public void setOwlFactory(OWLDataFactory owlFactory) {
		this.owlFactory = owlFactory;
	}

	public OBOSession getSession() {
		return session;
	}

	public OWLOntologyManager getManager() {
		return manager;
	}

	public void setManager(OWLOntologyManager manager) {
		this.manager = manager;
	}





}
