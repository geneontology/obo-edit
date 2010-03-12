package org.obo.owl.datamodel.impl;

import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.UUID;

import org.apache.log4j.Logger;
import org.obo.datamodel.AnnotatedObject;
import org.obo.datamodel.CommentedObject;
import org.obo.datamodel.Dbxref;
import org.obo.datamodel.DbxrefedObject;
import org.obo.datamodel.DefinedObject;
import org.obo.datamodel.IdentifiableObject;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Instance;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.MultiIDObject;
import org.obo.datamodel.Namespace;
import org.obo.datamodel.NamespacedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOSession;
import org.obo.datamodel.ObsoletableObject;
import org.obo.datamodel.SubsetObject;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.SynonymType;
import org.obo.datamodel.SynonymedObject;
import org.obo.datamodel.TermSubset;
import org.obo.datamodel.Type;
import org.obo.datamodel.impl.DbxrefImpl;
import org.obo.datamodel.impl.SynonymImpl;
import org.obo.datamodel.impl.SynonymTypeImpl;
import org.obo.datamodel.impl.TermCategoryImpl;
import org.obo.history.DestroyObjectHistoryItem;
import org.obo.owl.dataadapter.OWLAdapter;
import org.obo.owl.util.IDSpaceRegistry;
import org.semanticweb.owl.model.OWLAnnotation;
import org.semanticweb.owl.model.OWLAnnotationAxiom;
import org.semanticweb.owl.model.OWLAxiom;
import org.semanticweb.owl.model.OWLClass;
import org.semanticweb.owl.model.OWLConstant;
import org.semanticweb.owl.model.OWLConstantAnnotation;
import org.semanticweb.owl.model.OWLDataFactory;
import org.semanticweb.owl.model.OWLEntity;
import org.semanticweb.owl.model.OWLIndividual;
import org.semanticweb.owl.model.OWLObject;
import org.semanticweb.owl.model.OWLObjectAnnotation;
import org.semanticweb.owl.model.OWLObjectProperty;
import org.semanticweb.owl.model.OWLOntology;
import org.semanticweb.owl.model.OWLOntologyAnnotationAxiom;
import org.semanticweb.owl.model.OWLTypedConstant;
import org.semanticweb.owl.model.OWLUntypedConstant;
import org.semanticweb.owl.vocab.OWLRDFVocabulary;

import uk.ac.manchester.cs.owl.OWLAnnotationAxiomImpl;
import uk.ac.manchester.cs.owl.OWLAnnotationImpl;
import uk.ac.manchester.cs.owl.OWLConstantImpl;
import uk.ac.manchester.cs.owl.OWLIndividualImpl;
import uk.ac.manchester.cs.owl.OWLObjectAnnotationImpl;
import uk.ac.manchester.cs.owl.OWLUntypedConstantImpl;

public class NCBOOboInOWLMetadataMapping extends AbstractOWLMetadataMapping {

	// initialize logger
	protected final static Logger logger = Logger.getLogger(NCBOOboInOWLMetadataMapping.class);

	private static boolean first = true;

	public NCBOOboInOWLMetadataMapping() {
		super();
		IDSpaceRegistry registry = IDSpaceRegistry.getInstance();
		registry.registerMapping("http://www.geneontology.org/formats/oboInOwl#", "OboInOWL");
	}

	public boolean isOboToOWLLossy() {
		return true;
	} // TODO: unfinished

	public String getName() {
		return "NCBO OboInOWL mapping";
	}

	public String getDesc() {
		return "See http://www.bioontology.org/wiki/index.php/OboInOwl:Main_Page";
	}

	// public static String ns() { return
	// "http://www.geneontology.org/formats/oboInOwl";}
	public static String ns() {
		return "http://www.w3.org/2000/01/rdf-schema";
	}

	public enum OboInOWLNamespaces {

		OboInOWL("http://www.geneontology.org/formats/oboInOwl#");

		String ns;

		OboInOWLNamespaces(String ns) {
			this.ns = ns;
		}

		public String toString() {
			return ns;
		}
	}

	public enum OboInOWLVocabulary {
		// SYNONYM("synonym"),
		// HAS_DEFINITION("hasDefinition"),
		// OBSOLETE_CLASS("ObsoleteClass");

		// annotation properties
		HAS_URI("hasURI"), // obo->owl, owl->obo
		HAS_ALTERNATIVE_ID("hasAlternativeId"), // obo->owl, owl->obo
		HAS_DATE("hasDate"), //
		HAS_VERSION("hasVersion"), //
		HAS_DBXREF("hasDbXref"), // obo->owl, owl->obo
		HAS_DEFAULT_NAMESPACE("hasDefaultNamespace"), // obo->owl, owl->obo
		HAS_OBO_NAMESPACE("hasOBONamespace"), // obo->owl, owl->obo
		HAS_DEFINITION("hasDefinition"), // obo->owl, owl->obo
		HAS_EXACT_SYNONYM("hasExactSynonym"), // obo->owl, owl->obo
		HAS_NARROW_SYNONYM("hasNarrowSynonym"), // obo->owl, owl->obo
		HAS_BROAD_SYNONYM("hasBroadSynonym"), // obo->owl, owl->obo
		HAS_RELATED_SYNONYM("hasRelatedSynonym"), // obo->owl, owl->obo
		HAS_SYNONYM_TYPE("hasSynonymType"), // obo->owl, owl->obo
		RESTRICTED_TO_SCOPE("restrictedToScope"), // obo->owl, owl->obo
		HAS_SUBSET("hasSubset"), // obo->owl, owl->obo
		IN_SUBSET("inSubset"), // obo->owl, owl->obo
		SAVED_BY("savedBy"), //
		REPLACED_BY("replacedBy"), // obo->owl, owl->obo
		CONSIDER("consider"), // obo->owl, owl->obo

		// classes
		DBXREF("DbXref"), // obo->owl, owl->obo
		DEFINITION("Definition"), // obo->owl, owl->obo
		SUBSET("Subset"), // obo->owl, owl->obo
		SYNONYM("Synonym"), // obo->owl, owl->obo
		SYNONYM_TYPE("SynonymType"), // obo->owl, owl->obo
		OBSOLETE_CLASS("ObsoleteClass"), // obo->owl, owl->obo

		// object properties
		OBSOLETE_PROPERTY("ObsoleteProperty") // obo->owl, owl->obo

		;

		URI uri;

		OboInOWLVocabulary(String uri) {
			this.uri = URI.create(OboInOWLNamespaces.OboInOWL + uri);
		}

		public URI getURI() {
			return uri;
		}

		public String toString() {
			return uri.toString();
		}

	}

	public static final String HAS_DEFINITION = "hasDefinition";

	public static final String HAS_SYNONYM = "hasSynonym";

	public URI getVocabURI(String s) {
		return URI.create(ns() + "#" + s);
	}

	public Set<OWLAxiom> getOWLAxioms(OWLAdapter adapter, OWLEntity owlEntity,
			IdentifiedObject io) {
		HashSet<OWLAxiom> axioms = new HashSet<OWLAxiom>();
		setFactory(adapter.getOwlFactory());

		OWLDataFactory owlFactory = adapter.getOwlFactory();

		// finding the ontology that contains the entity
		Iterator<OWLOntology> ontologies = adapter.getManager().getOntologies().iterator();
		OWLOntology ontology = null;
		while (ontologies.hasNext()) {
			ontology = ontologies.next();
			if (ontology.containsEntityDeclaration(owlEntity)) {
				break;
			}
		}
		
		if (session == null)
			session = adapter.getSession();

		// hack to get the header tags, only called when this function is called for the first time
		if (first) {
			if (ontology != null) {
				// default namespace
				if (session.getDefaultNamespace().getID() != null) {
					System.err.println("id: "+session.getDefaultNamespace().getID());
					OWLTypedConstant defNsId = owlFactory.getOWLTypedConstant(session.getDefaultNamespace().getID());
					OWLAnnotation dNs = owlFactory.getOWLConstantAnnotation(OboInOWLVocabulary.HAS_DEFAULT_NAMESPACE.uri, defNsId);
					axioms.add(owlFactory.getOWLOntologyAnnotationAxiom(ontology, dNs));
				}

				// data version
				// ? not provided by obo session

				// format version
				// ? not provided by obo session

				// saved by user
				// ? not provided by obo session
			}
			first = false;
		}

		if (io instanceof DbxrefedObject) {
			try {
				// translate xrefs
				Iterator<Dbxref> refs = ((DbxrefedObject) io).getDbxrefs().iterator();
				axioms.addAll(getOWLAxiomsForXrefs(adapter, owlFactory, owlEntity, refs));
			} catch (UnsupportedEncodingException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		if (io instanceof CommentedObject) {
			// translating the comments tag
			String comment = ((CommentedObject) io).getComment();
			if (comment != null && !comment.equals("")) {
				axioms.add(getAnnotationAxiom(owlEntity, OWLRDFVocabulary.RDFS_COMMENT.getURI(), comment));
			}
		}
		if (io instanceof DefinedObject) {
			// translating definitions and relevant dbxrefs
			DefinedObject defo = (DefinedObject) io;
			String def = defo.getDefinition();
			if (def != null && !def.equals("")) {
				try {
					OWLClass defCls = owlFactory.getOWLClass(OboInOWLVocabulary.DEFINITION.uri);
					OWLIndividual defInst = owlFactory.getOWLIndividual(adapter.getURI(io.getID()
							+ "__def__" + UUID.randomUUID().toString()));

					// definition is an instance of the Definition class
					axioms.add(owlFactory.getOWLClassAssertionAxiom(defInst, defCls));
					// definition text is a label of the definition instance
					axioms.add(getAnnotationAxiom(defInst, OWLRDFVocabulary.RDFS_LABEL.getURI(), def));
					// definition instance is an annotation of the term/typedef
					axioms.add(getAnnotationAxiom(owlEntity, OboInOWLVocabulary.HAS_DEFINITION.getURI(), defInst));

					// translate xrefs
					Iterator<Dbxref> refs = defo.getDefDbxrefs().iterator();
					axioms.addAll(getOWLAxiomsForXrefs(adapter, owlFactory, defInst, refs));

				} catch (UnsupportedEncodingException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}
		if (io instanceof SynonymedObject) {
			// translating synonyms
			SynonymedObject so = (SynonymedObject) io;

			Synonym[] syns = {};
			syns = so.getSynonyms().toArray(syns);

			OboInOWLVocabulary typeOfSynonym = null;

			// finding the scope and selecting appropriate type of synonym
			for (Synonym syn : syns) {
				switch (syn.getScope()) {
				case Synonym.EXACT_SYNONYM:
					typeOfSynonym = OboInOWLVocabulary.HAS_EXACT_SYNONYM;
					break;
				case Synonym.BROAD_SYNONYM:
					typeOfSynonym = OboInOWLVocabulary.HAS_BROAD_SYNONYM;
					break;
				case Synonym.NARROW_SYNONYM:
					typeOfSynonym = OboInOWLVocabulary.HAS_NARROW_SYNONYM;
					break;
				case Synonym.RELATED_SYNONYM:
					typeOfSynonym = OboInOWLVocabulary.HAS_RELATED_SYNONYM;
					break;
				default:
					logger.error("An invalid synonym type has been found '"
							+ so.getID() + "' " + so.getClass());
				}

				SynonymType synTypedef = syn.getSynonymType();

				if (typeOfSynonym != null) {
					try {
						// new instance of synonym
						OWLClass synCls = owlFactory.getOWLClass(OboInOWLVocabulary.SYNONYM.uri);
						OWLIndividual synInst = owlFactory.getOWLIndividual(adapter.getURI(io.getID()
								+ "__syn__" + UUID.randomUUID().toString()));

						// synonym is an instance of Synonym class
						axioms.add(owlFactory.getOWLClassAssertionAxiom(synInst, synCls));
						axioms.add(getAnnotationAxiom(synInst, OWLRDFVocabulary.RDFS_LABEL.getURI(), syn.getText()));
						axioms.add(getAnnotationAxiom(owlEntity, typeOfSynonym.getURI(), synInst));

						if (synTypedef != null) {

							String scopetd = "";
							switch (synTypedef.getScope()) {
							case Synonym.EXACT_SYNONYM: scopetd = "EXACT"; break;
							case Synonym.BROAD_SYNONYM: scopetd = "BROAD"; break;
							case Synonym.NARROW_SYNONYM: scopetd = "NARROW"; break;
							case Synonym.RELATED_SYNONYM: scopetd = "RELATED"; break;
							}

							// create synonym type instance
							OWLClass syntdCls = owlFactory.getOWLClass(OboInOWLVocabulary.SYNONYM_TYPE.uri);
							OWLIndividual syntdInst = owlFactory.getOWLIndividual(adapter.getURI(synTypedef.getID()));
							axioms.add(owlFactory.getOWLClassAssertionAxiom(syntdInst, syntdCls));
							axioms.add(getAnnotationAxiom(syntdInst, OWLRDFVocabulary.RDFS_LABEL.getURI(), synTypedef.getName()));
							if (!scopetd.equals("")) axioms.add(getAnnotationAxiom(syntdInst, OboInOWLVocabulary.RESTRICTED_TO_SCOPE.uri, scopetd));

							// set the synonym type
							axioms.add(getAnnotationAxiom(synInst, OboInOWLVocabulary.HAS_SYNONYM_TYPE.uri, syntdInst));
						}

						// translate xrefs
						Iterator<Dbxref> refs = syn.getXrefs().iterator();
						axioms.addAll(getOWLAxiomsForXrefs(adapter, owlFactory, synInst, refs));
					} catch (UnsupportedEncodingException e) {
						// TODO Autogenerated
						e.printStackTrace();
					}
				}
			}

		}
		if (io instanceof MultiIDObject) {
			MultiIDObject mo = (MultiIDObject) io;
			// retrieve the alternative IDs
			Iterator<String> miter = mo.getSecondaryIDs().iterator();
			while (miter.hasNext()) {
				String alt = miter.next();
				axioms.add(getAnnotationAxiom(owlEntity, OboInOWLVocabulary.HAS_ALTERNATIVE_ID.uri, alt));
			}
		}
		if (io instanceof NamespacedObject) {
			NamespacedObject nso = (NamespacedObject) io;
			Namespace ns = nso.getNamespace();

			// adding namespace
			Namespace defNs = session.getDefaultNamespace();
			if (ns == null) {
				
			}
			else if (ns.equals(defNs)) {
				// default namespace found, added already
			}
			else {
				// not the default namespace, simply add a namespace tag to the entity
				axioms.add(getAnnotationAxiom(owlEntity, OboInOWLVocabulary.HAS_OBO_NAMESPACE.uri, ns.getID()));
			}
		}
		if (io instanceof SubsetObject) {
			SubsetObject sso = (SubsetObject) io;
			Iterator<TermSubset> ssiter = sso.getSubsets().iterator();
			try {
				// add all subsets for the given object/term
				while (ssiter.hasNext()) {
					TermSubset tss = ssiter.next();

					// creating the subset instance
					OWLClass ssCls = owlFactory.getOWLClass(OboInOWLVocabulary.SUBSET.uri);
					OWLIndividual ssInst = owlFactory.getOWLIndividual(adapter.getURI(tss.getName()));
					axioms.add(owlFactory.getOWLClassAssertionAxiom(ssInst, ssCls));
					axioms.add(getAnnotationAxiom(owlEntity, OboInOWLVocabulary.IN_SUBSET.uri, ssInst.getURI()));

					if (ontology != null) {
						// setting the ontology annotation
						OWLAnnotation hasSS = owlFactory.getOWLObjectAnnotation(OboInOWLVocabulary.HAS_SUBSET.uri, ssInst);
						axioms.add(owlFactory.getOWLOntologyAnnotationAxiom(ontology, hasSS));
					}

					// no need for comment annotation tag if it is empty
					if (!tss.getDesc().equals(""))
						axioms.add(getAnnotationAxiom(ssInst, OWLRDFVocabulary.RDFS_COMMENT.getURI(), tss.getDesc()));
				}
			} catch (UnsupportedEncodingException e) {
				e.printStackTrace();
			}
		}
		if (io instanceof ObsoletableObject) {
			// metadata on obsolete terms and typedefs
			ObsoletableObject oo = (ObsoletableObject) io;
			if (oo.isObsolete()) {

				if (owlEntity instanceof OWLClass) {
					// obsolete term
					OWLClass obCls = owlFactory.getOWLClass(OboInOWLVocabulary.OBSOLETE_CLASS.uri);
					axioms.add(owlFactory.getOWLSubClassAxiom((OWLClass) owlEntity, obCls));
				}
				if (owlEntity instanceof OWLObjectProperty) {
					// obsolete typedef
					OWLObjectProperty obOP = owlFactory.getOWLObjectProperty(OboInOWLVocabulary.OBSOLETE_PROPERTY.uri);
					axioms.add(owlFactory.getOWLSubObjectPropertyAxiom((OWLObjectProperty) owlEntity, obOP));
				}

				try {
					// replaced by
					Iterator<ObsoletableObject> reps = oo.getReplacedBy().iterator();
					while (reps.hasNext()) {
						ObsoletableObject ooo = reps.next();
						axioms.add(getAnnotationAxiom(owlEntity, OboInOWLVocabulary.REPLACED_BY.uri, adapter.getURI(ooo)));
					}

					// considerations for replacement
					Iterator<ObsoletableObject> cons = oo.getConsiderReplacements().iterator();
					while (cons.hasNext()) {
						ObsoletableObject ooo = cons.next();
						axioms.add(getAnnotationAxiom(owlEntity, OboInOWLVocabulary.CONSIDER.uri, adapter.getURI(ooo)));
					}
				} catch (UnsupportedEncodingException e) {
					// TODO Autogenerated
					e.printStackTrace();
				}
			}
		}
		return axioms;

	}

	/**
	 * generic function for translating xrefs to OWL elements
	 * @param adapter
	 * @param owlFactory
	 * @param owlEntity
	 * @param refs list of xrefs that need to be translated
	 * @return
	 * @throws UnsupportedEncodingException
	 */
	private HashSet<OWLAxiom> getOWLAxiomsForXrefs(OWLAdapter adapter,
			OWLDataFactory owlFactory, OWLEntity owlEntity,
			Iterator<Dbxref> refs) throws UnsupportedEncodingException {

		HashSet<OWLAxiom> axioms = new HashSet<OWLAxiom>();

		OWLClass xrefCls = owlFactory.getOWLClass(OboInOWLVocabulary.DBXREF.uri);

		while (refs.hasNext()) {
			Dbxref xref = refs.next();

			String uid = adapter.getURI(xref).toString();

			OWLIndividual xrefInst = owlFactory.getOWLIndividual(adapter.getURI(uid + "__xref__" + UUID.randomUUID().toString()));
			// xref instance is an instance of xref class
			axioms.add(owlFactory.getOWLClassAssertionAxiom(xrefInst, xrefCls));

			String lbl = xref.getDatabase() + ":" + xref.getDatabaseID();
			if (xref.getDatabase().equals("http")) {
				lbl = "URL:" + uid;
			}

			// information on the xref
			axioms.add(getAnnotationAxiom(xrefInst, OWLRDFVocabulary.RDFS_LABEL.getURI(), lbl));
			axioms.add(getAnnotationAxiom(xrefInst, OboInOWLVocabulary.HAS_URI.uri, uid));

			axioms.add(getAnnotationAxiom(owlEntity, OboInOWLVocabulary.HAS_DBXREF.uri, xrefInst));

		}

		return axioms;
	}

	public boolean translateOWLAxiom(OWLAnnotationAxiom axiom, IdentifiedObject lo, OWLAdapter adapter) {
		OWLAnnotation owlAnnot = axiom.getAnnotation();
		URI uri = owlAnnot.getAnnotationURI();

		OBOSession obo = adapter.getSession();

		// finding the ontology that contains the axiom
		Iterator<OWLOntology> ontologies = adapter.getManager().getOntologies().iterator();
		OWLOntology ontology = null;
		while (ontologies.hasNext()) {
			ontology = ontologies.next();
			if (ontology.containsAxiom(axiom)) {
				break;
			}
		}

		// hack to get the ontology annotations, only called when this function is called for the first time
		if (first) {
			Iterator<OWLOntologyAnnotationAxiom> headerAxioms = ontology.getOntologyAnnotationAxioms().iterator();
			while (headerAxioms.hasNext()) {
				OWLOntologyAnnotationAxiom hx = headerAxioms.next();
				OWLAnnotation hxa = hx.getAnnotation();

				// getting default obo namespace from the owl version
				if (hxa.getAnnotationURI().equals(OboInOWLVocabulary.HAS_DEFAULT_NAMESPACE.uri)) {
					obo.setDefaultNamespace(new Namespace(hxa.getAnnotationValueAsConstant().getLiteral()));
				}
			}
			first = false;
		}

		if (owlAnnot instanceof OWLConstantAnnotation) {

			String val = owlAnnot.getAnnotationValueAsConstant().getLiteral();

			// translate comment
			if (lo instanceof CommentedObject) {
				if (uri.equals(OWLRDFVocabulary.RDFS_COMMENT.getURI())) {
					((CommentedObject) lo).setComment(val);
					return true;
				}
			}
			// translate namespace
			if (lo instanceof NamespacedObject) {
				if (uri.equals(OboInOWLVocabulary.HAS_OBO_NAMESPACE.uri)) {
					Namespace ns = new Namespace();
					ns.setID(val);
					((NamespacedObject) lo).setNamespace(ns);
					return true;
				}
			}
			// translate alternative IDs
			if (lo instanceof MultiIDObject) {
				if (uri.equals(OboInOWLVocabulary.HAS_ALTERNATIVE_ID.uri)) {
					((MultiIDObject) lo).addSecondaryID(val);
					return true;
				}
			}
			// obsolete term/typedef's consider and replaced_by tags
			if (lo instanceof ObsoletableObject) {
				if (uri.equals(OboInOWLVocabulary.CONSIDER.uri)) {
					ObsoletableObject oo = (ObsoletableObject) obo.getObject(adapter.getOboID(URI.create(val)));
					((ObsoletableObject) lo).addConsiderReplacement(oo);
					return true;
				}
				if (uri.equals(OboInOWLVocabulary.REPLACED_BY.uri)) {
					ObsoletableObject oo = (ObsoletableObject) obo.getObject(adapter.getOboID(URI.create(val)));
					((ObsoletableObject) lo).addReplacedBy(oo);
					return true;
				}
			}
		} else if (owlAnnot instanceof OWLObjectAnnotation) {

			OWLIndividual val = ((OWLObjectAnnotation) owlAnnot).getAnnotationValue();

			String instId = adapter.getOboID(val);
			IdentifiedObject io = adapter.getSession().getObject(instId);

			// relies on instances having been processed first
			if (io == null)
				return true;

			if (lo instanceof DefinedObject && uri.equals(OboInOWLVocabulary.HAS_DEFINITION.getURI())) {
				DefinedObject defo = (DefinedObject) lo;
				defo.setDefinition(io.getName());
				obo.removeObject(io);

				// check if the definition has dbxrefs
				if (ontology != null) translateOWLAxiomsForXrefs(adapter, ontology, val, lo, Dbxref.DEFINITION);
				return true;
			}
			if (lo instanceof SynonymedObject) {
				SynonymedObject so = (SynonymedObject) lo;
				String syn = io.getName();

				Synonym synObj = null;

				if (uri.equals(OboInOWLVocabulary.HAS_EXACT_SYNONYM.uri)) {
					synObj = new SynonymImpl(syn, Synonym.EXACT_SYNONYM);
				} else if (uri.equals(OboInOWLVocabulary.HAS_BROAD_SYNONYM.uri)) {
					synObj = new SynonymImpl(syn, Synonym.BROAD_SYNONYM);
				} else if (uri.equals(OboInOWLVocabulary.HAS_NARROW_SYNONYM.uri)) {
					synObj = new SynonymImpl(syn, Synonym.NARROW_SYNONYM);
				} else if (uri.equals(OboInOWLVocabulary.HAS_RELATED_SYNONYM.uri)) {
					synObj = new SynonymImpl(syn, Synonym.RELATED_SYNONYM);
				}

				if (synObj != null) {

					// checking if the synonym belongs to a synonym typedef
					SynonymType syntdObj = null;
					if (ontology != null) {
						Iterator<OWLAnnotation> syntds = val.getAnnotations(ontology, OboInOWLVocabulary.HAS_SYNONYM_TYPE.uri).iterator();
						if (syntds.hasNext()) {
							OWLAnnotation syntd = syntds.next();

							// Synonym typedef is an instance of type SYNONYM_TYPE in OWL ontology
							OWLIndividual synTdInst = (OWLIndividual) syntd.getAnnotationValue();

							// creating an OBO object for synonym type
							syntdObj = new SynonymTypeImpl(synTdInst.toString(), "");

							// finding the name of the synonym type
							Iterator<OWLAnnotation> synTdInstNames = synTdInst.getAnnotations(ontology, OWLRDFVocabulary.RDFS_LABEL.getURI()).iterator();
							if (synTdInstNames.hasNext()) {
								OWLAnnotation synTdInstName = synTdInstNames.next();
								syntdObj.setName(synTdInstName.getAnnotationValueAsConstant().getLiteral());
							}

							// finding the scope of the synonym type
							Iterator<OWLAnnotation> synTdInstScopes = synTdInst.getAnnotations(ontology, OboInOWLVocabulary.RESTRICTED_TO_SCOPE.uri).iterator();
							if (synTdInstScopes.hasNext()) {
								OWLAnnotation synTdInstScope = synTdInstScopes.next();
								String syntdScope = synTdInstScope.getAnnotationValue().toString();
								if (syntdScope.equals("EXACT")) syntdObj.setScope(Synonym.EXACT_SYNONYM);
								else if (syntdScope.equals("NARROW")) syntdObj.setScope(Synonym.NARROW_SYNONYM);
								else if (syntdScope.equals("RELATED")) syntdObj.setScope(Synonym.RELATED_SYNONYM);
								else if (syntdScope.equals("BROAD")) syntdObj.setScope(Synonym.BROAD_SYNONYM);
							}
						}
					}

					if (syntdObj != null) {
						// adding synonym typedef to the synonym and header
						synObj.setSynonymType(syntdObj);
						obo.addSynonymType(syntdObj);
					}

					so.addSynonym(synObj);
					obo.removeObject(io);

					// check if the synonym has dbxrefs
					if (ontology != null)
						translateOWLAxiomsForXrefs(adapter, ontology, val, synObj, Dbxref.RELATED_SYNONYM);

					return true;
				}
			}
			if (lo instanceof SubsetObject && uri.equals(OboInOWLVocabulary.IN_SUBSET.getURI())) {
				SubsetObject sso = (SubsetObject) lo;

				if (instId.startsWith("_global:")) instId = instId.substring("_global:".length());

				String instCmt = "";

				// finding details about the subsetdef, if available
				// we do not process the HAS_SUBSET annotation because these are automatically added to obo header
				if (ontology != null && io != null) {
					Iterator<OWLAnnotation> cmtiter = val.getAnnotations(ontology, OWLRDFVocabulary.RDFS_COMMENT.getURI()).iterator();
					while (cmtiter.hasNext()) {
						OWLConstant cmtObj = cmtiter.next().getAnnotationValueAsConstant();
						instCmt = cmtObj.getLiteral();
					}
				}

				// creating subset and adding to obo
				TermSubset tss = new TermCategoryImpl(instId, instCmt);
				obo.addSubset(tss);
				sso.addCategory(tss);
				obo.removeObject(io);
				return true;
			}
			if (lo instanceof DbxrefedObject && uri.equals(OboInOWLVocabulary.HAS_DBXREF.uri)) {
				DbxrefedObject dxo = (DbxrefedObject) lo;
				Dbxref xr = getDbxrefFromName(io.getName(), Dbxref.UNKNOWN);
				dxo.addDbxref(xr);
				obo.removeObject(io);
				return true;
			}
		}
		return false;
	}

	private void translateOWLAxiomsForXrefs(OWLAdapter adapter, OWLOntology ontology, OWLIndividual inst, IdentifiableObject ao, int type) {

		Iterator<OWLAnnotation> xrefs = inst.getAnnotations(ontology, OboInOWLVocabulary.HAS_DBXREF.uri).iterator();
		while (xrefs.hasNext()) {
			OWLAnnotation xrAnnot = xrefs.next();
			OWLObject xrObj = xrAnnot.getAnnotationValue();

			if (xrAnnot instanceof OWLConstantAnnotation) {

			} else if (xrAnnot instanceof OWLObjectAnnotation) {

				OWLIndividual xrInst = (OWLIndividual) xrObj;
				String id = adapter.getOboID(xrInst.getURI());

				Iterator<OWLAnnotationAxiom> instAxioms = xrInst.getAnnotationAxioms(ontology).iterator();

				String xrefName = null;
				while (instAxioms.hasNext()) {
					OWLAnnotationAxiom annotAx = instAxioms.next();
					OWLAnnotation annot = annotAx.getAnnotation();
					if (annot.getAnnotationURI().equals(OWLRDFVocabulary.RDFS_LABEL.getURI())) {
						xrefName = annot.getAnnotationValueAsConstant().getLiteral();
					}
				}

				if (xrefName != null) {

					Dbxref dbxref = getDbxrefFromName(xrefName, type);

					if (type == Dbxref.DEFINITION) {
						((DefinedObject) ao).addDefDbxref(dbxref);
					} else if (ao instanceof DbxrefedObject) {
						((DbxrefedObject) ao).addDbxref(dbxref);
					} else if (ao instanceof Synonym) {
						((Synonym) ao).addXref(dbxref);
					}
				}

				IdentifiedObject oboObj = adapter.getSession().getObject(id);
				if (oboObj != null) adapter.getSession().removeObject(oboObj);
			}
		}

	}

	private Dbxref getDbxrefFromName(String name, int type) {

		String db, id;
		int colon = name.indexOf(':');
		db = name.substring(0, colon);
		id = name.substring(colon + 1);

		return new DbxrefImpl(db, id, type);
	}

	public void translateGraph(OBOSession session) {
		for (IdentifiedObject io : session.getObjects()) {
			if (io.isBuiltIn())
				continue;
			if (io instanceof LinkedObject) {
				for (Link link : ((LinkedObject) io).getParents()) {
					if (link.getParent().getID().equals("OboInOWL:ObsoleteClass")) {
						((LinkedObject) io).removeParent(link);
						((ObsoletableObject) io).setObsolete(true);
					}
				}
			}
			if (io instanceof OBOClass) {
				if (io.getID().startsWith("OboInOWL:")) {
					// TODO How to get rid of the mapping specific classes?
					// The following statements throw
					// java.util.ConcurrentModificationException
					// session.removeObject(io);
					// OR
					// session.getOperationModel().apply(new
					// DestroyObjectHistoryItem(io));
				}
			}
		}
	}

}
