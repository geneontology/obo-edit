package owltools.graph;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.Vector;

import org.apache.log4j.Logger;
import org.obolibrary.obo2owl.Obo2OWLConstants;
import org.obolibrary.obo2owl.Obo2OWLConstants.Obo2OWLVocabulary;
import org.obolibrary.obo2owl.Obo2Owl;
import org.obolibrary.obo2owl.Owl2Obo;
import org.obolibrary.oboformat.parser.OBOFormatConstants.OboFormatTag;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.AddImport;
import org.semanticweb.owlapi.model.AxiomType;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationAssertionAxiom;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLAnnotationSubject;
import org.semanticweb.owlapi.model.OWLAnnotationValue;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassAssertionAxiom;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLEquivalentClassesAxiom;
import org.semanticweb.owlapi.model.OWLFunctionalDataPropertyAxiom;
import org.semanticweb.owlapi.model.OWLFunctionalObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLIndividual;
import org.semanticweb.owlapi.model.OWLInverseFunctionalObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLInverseObjectPropertiesAxiom;
import org.semanticweb.owlapi.model.OWLLiteral;
import org.semanticweb.owlapi.model.OWLNamedIndividual;
import org.semanticweb.owlapi.model.OWLNamedObject;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLObjectAllValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectHasValue;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLObjectProperty;
import org.semanticweb.owlapi.model.OWLObjectPropertyAssertionAxiom;
import org.semanticweb.owlapi.model.OWLObjectPropertyExpression;
import org.semanticweb.owlapi.model.OWLObjectSomeValuesFrom;
import org.semanticweb.owlapi.model.OWLObjectUnionOf;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLPropertyRange;
import org.semanticweb.owlapi.model.OWLQuantifiedRestriction;
import org.semanticweb.owlapi.model.OWLReflexiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLRestriction;
import org.semanticweb.owlapi.model.OWLSubClassOfAxiom;
import org.semanticweb.owlapi.model.OWLSubObjectPropertyOfAxiom;
import org.semanticweb.owlapi.model.OWLSymmetricObjectPropertyAxiom;
import org.semanticweb.owlapi.model.OWLTransitiveObjectPropertyAxiom;
import org.semanticweb.owlapi.model.UnknownOWLOntologyException;
import org.semanticweb.owlapi.vocab.OWLRDFVocabulary;

import owltools.graph.OWLQuantifiedProperty.Quantifier;
import owltools.sim.DisjunctiveSetSimilarity;

/**
 * This class provides additional capabilities on top of the OWLAPI.
 * 
 * <ul>
 * <li>convenience methods for OBO-like properties such as synonyms, textual definitions, obsoletion, replaced_by
 * <li>simple graph-like operations over ontologies, including reachability/closure queries that respect the OWL semantics
 * </ul>
 *
 * An instance of an OWLGraphWrapper wraps one or more {@link OWLOntology} objects. One of these is designated
 * the sourceOntology, the others are designated support ontologies. The source ontology may import the support
 * ontologies, but this is optional. Most OWLGraphWrapper methods operate over the union of the source ontology
 * and support ontologies. This is particularly useful for working with OBO Library ontologies, where axioms
 * connecting ontologies may be available as separate ontologies
 *
 * @see OWLGraphUtil
 * @author cjm
 *
 */
public class OWLGraphWrapper {

	private static Logger LOG = Logger.getLogger(DisjunctiveSetSimilarity.class);

	public static final String DEFAULT_IRI_PREFIX = "http://purl.obolibrary.org/obo/";

	@Deprecated
	OWLOntology ontology; // this is the ontology used for querying. may be the merge of sourceOntology+closure

	OWLOntology sourceOntology; // graph is seeded from this ontology.

	Set<OWLOntology> supportOntologySet = new HashSet<OWLOntology>();

	OWLDataFactory dataFactory;
	OWLOntologyManager manager;
	Config config = new Config();

	private Map<OWLObject,Set<OWLGraphEdge>> edgeBySource;
	private Map<OWLObject,Set<OWLGraphEdge>> edgeByTarget;
	public Map<OWLObject,Set<OWLGraphEdge>> inferredEdgeBySource = null; // public to serialize
	private Map<OWLObject,Set<OWLGraphEdge>> inferredEdgeByTarget = null;

	// used to store mappings child->parent, where
	// parent = UnionOf( ..., child, ...)
	private Map<OWLObject,Set<OWLObject>> extraSubClassOfEdges = null;


	/**
	 * Configuration options. These are typically specific to a
	 * OWLGraphWrapper instance.
	 *
	 */
	public class Config {
		// by default the graph closure includes only named entities
		public boolean isIncludeClassExpressionsInClosure = true;

		// by default we do not follow complement of - TODO
		public boolean isFollowComplementOfInClosure = false;

		public boolean isCacheClosure = true;
		public boolean isMonitorMemory = true;

		// if set to non-null, this constrains graph traversal. TODO
		public Set<OWLQuantifiedProperty> graphEdgeIncludeSet = null;
		public Set<OWLQuantifiedProperty> graphEdgeExcludeSet = null;
		public OWLClass excludeMetaClass = null;

		public void excludeProperty(OWLObjectProperty p) {
			if (graphEdgeExcludeSet == null)
				graphEdgeExcludeSet = new HashSet<OWLQuantifiedProperty>();
			graphEdgeExcludeSet.add(new OWLQuantifiedProperty(p, null));
		}
	}

	/**
	 * Create a new wrapper for an OWLOntology
	 * @throws OWLOntologyCreationException 
	 * @throws UnknownOWLOntologyException 
	 */
	public OWLGraphWrapper(OWLOntology ontology) throws UnknownOWLOntologyException, OWLOntologyCreationException {
		this(ontology, false);
	}

	@Deprecated
	public OWLGraphWrapper(OWLOntology ontology, boolean isMergeImportClosure) throws UnknownOWLOntologyException, OWLOntologyCreationException {
		super();
		manager = OWLManager.createOWLOntologyManager();
		dataFactory = manager.getOWLDataFactory();
		if (isMergeImportClosure) {
			System.out.println("setting source ontology:"+ontology);
			this.sourceOntology = ontology;
			// the query ontology is the source ontology plus the imports closure
			useImportClosureForQueries();

		}
		else {
			this.sourceOntology = ontology;
			this.ontology = ontology;
		}
		manager.getOntologyFormat(ontology);
	}

	public OWLGraphWrapper(String iri) throws OWLOntologyCreationException {
		super();
		manager = OWLManager.createOWLOntologyManager();
		dataFactory = manager.getOWLDataFactory();
		sourceOntology = manager.createOntology(IRI.create(iri));
	}

	public void addImport(OWLOntology extOnt) {
		AddImport ai = new AddImport(ontology, dataFactory.getOWLImportsDeclaration(extOnt.getOntologyID().getOntologyIRI()));
		manager.applyChange(ai);
	}

	/**
	 * if called, copies all axioms from import closure into query ontology.
	 * 
	 * @throws UnknownOWLOntologyException
	 * @throws OWLOntologyCreationException
	 */
	@Deprecated
	public void useImportClosureForQueries() throws UnknownOWLOntologyException, OWLOntologyCreationException {
		this.ontology = 
			manager.createOntology(sourceOntology.getOntologyID().getOntologyIRI(), sourceOntology.getImportsClosure());
	}

	@Deprecated
	public void addQueryOntology(OWLOntology extOnt) throws OWLOntologyCreationException {
		Set<OWLAxiom> axioms = ontology.getAxioms();
		axioms.addAll(extOnt.getAxioms());
		this.ontology = 
			manager.createOntology(axioms, sourceOntology.getOntologyID().getOntologyIRI());	
	}

	public void mergeOntology(OWLOntology extOnt) throws OWLOntologyCreationException {
		for (OWLAxiom axiom : extOnt.getAxioms()) {
			manager.applyChange(new AddAxiom(sourceOntology, axiom));
		}
	}

	@Deprecated
	public OWLOntology getOntology() {
		return ontology;
	}


	@Deprecated
	public void setOntology(OWLOntology ontology) {
		this.ontology = ontology;
	}

	/**
	 * 
	 * @return
	 */
	public OWLOntology getSourceOntology() {
		return sourceOntology;
	}

	public void setSourceOntology(OWLOntology sourceOntology) {
		this.sourceOntology = sourceOntology;
	}



	/**
	 * all operations are over a set of ontologies - the source ontology plus
	 * any number of supporting ontologies. The supporting ontologies may be drawn
	 * from the imports closure of the source ontology, although this need not be the case.
	 * 
	 * @return
	 */
	public Set<OWLOntology> getSupportOntologySet() {
		return supportOntologySet;
	}

	public void setSupportOntologySet(Set<OWLOntology> supportOntologySet) {
		this.supportOntologySet = supportOntologySet;
	}

	public void addSupportOntology(OWLOntology o) {
		this.supportOntologySet.add(o);
	}
	public void removeSupportOntology(OWLOntology o) {
		this.supportOntologySet.remove(o);
	}
	public void addSupportOntologiesFromImportsClosure() {
		for (OWLOntology o : sourceOntology.getImportsClosure()) {
			if (o.equals(sourceOntology))
				continue;
			addSupportOntology(o);
		}
	}

	/**
	 * in general application code need not call this - it is mostly used internally
	 * 
	 * @return union of source ontology plus all supporting ontologies
	 */
	public Set<OWLOntology> getAllOntologies() {
		Set<OWLOntology> all = new HashSet<OWLOntology>(getSupportOntologySet());
		all.add(getSourceOntology());
		return all;
	}

	public OWLDataFactory getDataFactory() {
		return dataFactory;
	}

	public void setDataFactory(OWLDataFactory dataFactory) {
		this.dataFactory = dataFactory;
	}



	public OWLOntologyManager getManager() {
		return manager;
	}

	public void setManager(OWLOntologyManager manager) {
		this.manager = manager;
	}

	public Config getConfig() {
		return config;
	}

	public void setConfig(Config config) {
		this.config = config;
	}


	// ----------------------------------------
	// BASIC GRAPH EDGE TRAVERSAL
	// ----------------------------------------



	/**
	 * retrieves direct edges from a source
	 * to the direct **named** target
	 * 
	 * e.g. if (A SubClassOf B) then outgoing(A) = { <A,sub,B>}
	 * e.g. if (A SubClassOf R some B) then outgoing(A) = { <A, R-some, B> }
	 * e.g. if (A SubClassOf R some (R2 some B)) then outgoing(A) = { <A, [R-some,R2-same], B> }
	 * 
	 * @param source
	 * @return all edges that originate from source to nearest named object target
	 */
	public Set<OWLGraphEdge> getOutgoingEdges(OWLObject cls) {
		Set<OWLGraphEdge> pEdges = getPrimitiveOutgoingEdges(cls);
		Set<OWLGraphEdge> edges = new HashSet<OWLGraphEdge>();
		for (OWLGraphEdge e : pEdges) {
			edges.addAll(primitiveEdgeToFullEdges(e));
		}
		return edges;
	}

	private Set<OWLObject> getOutgoingEdgesViaReverseUnion(OWLObject child) {
		if (extraSubClassOfEdges == null)
			cacheReverseUnionMap();
		if (extraSubClassOfEdges.containsKey(child)) 
			return new HashSet<OWLObject>(extraSubClassOfEdges.get(child));
		else
			return new HashSet<OWLObject>();
	}


	private void cacheReverseUnionMap() {
		extraSubClassOfEdges = new HashMap<OWLObject, Set<OWLObject>>();
		for (OWLOntology o : getAllOntologies()) {
			for (OWLClass cls : o.getClassesInSignature()) {
				for (OWLEquivalentClassesAxiom eca : o.getEquivalentClassesAxioms(cls)) {
					for (OWLClassExpression ce : eca.getClassExpressions()) {
						if (ce instanceof OWLObjectUnionOf) {
							for (OWLObject child : ((OWLObjectUnionOf)ce).getOperands()) {
								if (extraSubClassOfEdges.containsKey(child)) {
									extraSubClassOfEdges.get(child).add(cls);
								}
								else {
									extraSubClassOfEdges.put(child, new HashSet<OWLObject>());
									extraSubClassOfEdges.get(child).add(cls);
								}
							}
						}
					}
				}
			}
		}
	}

	/**
	 * primitive edges connect any combination of named objects and expressions
	 * 
	 * e.g. (A SubClassOf R some B) => <A,sub,R-some-B>, <R-some-B,R-some,B>
	 * @param source
	 * @return
	 */
	public Set<OWLGraphEdge> getPrimitiveOutgoingEdges(OWLObject s) {
		Set<OWLGraphEdge> edges = new HashSet<OWLGraphEdge>();
		for (OWLOntology o : getAllOntologies()) {
			if (s instanceof OWLClass) {

				for (OWLSubClassOfAxiom sca : o.getSubClassAxiomsForSubClass((OWLClass) s)) {
					edges.add(createSubClassOfEdge(sca.getSubClass(), sca.getSuperClass()));
				}
				for (OWLEquivalentClassesAxiom eqa : o.getEquivalentClassesAxioms((OWLClass) s)) {
					for (OWLClassExpression ce : eqa.getClassExpressions()) {
						if (!ce.equals(s))
							edges.add(createSubClassOfEdge(s, ce));
					}
				}
				for (OWLObject pbu : getOutgoingEdgesViaReverseUnion(s)) {
					if (pbu instanceof OWLClass)
						edges.add(createSubClassOfEdge(s,(OWLClass)pbu));
				}

			}
			else if (s instanceof OWLIndividual) {
				// TODO - do we care about punning?
				// need to define semantics here
				//System.err.println("getting individual axioms");
				for (OWLClassAssertionAxiom a : o.getClassAssertionAxioms((OWLIndividual) s)) {
					edges.add(new OWLGraphEdge(s,a.getClassExpression(),null,Quantifier.INSTANCE_OF,getSourceOntology()));
				}
				for (OWLObjectPropertyAssertionAxiom a : o.getObjectPropertyAssertionAxioms((OWLIndividual) s)) {
					edges.add(new OWLGraphEdge(s,a.getObject(),a.getProperty(),Quantifier.PROPERTY_ASSERTION,getSourceOntology()));
				}
			}
			else if (s instanceof OWLRestriction<?, ?, ?>) {
				edges.add(restrictionToPrimitiveEdge((OWLRestriction<?, ?, ?>) s));
			}
			else if (s instanceof OWLObjectIntersectionOf) {
				for (OWLClassExpression ce : ((OWLObjectIntersectionOf)s).getOperands()) {
					edges.add(createSubClassOfEdge(s,ce));
				}
			}
			else if (s instanceof OWLObjectUnionOf) {
				// do nothing in this direction
			}
		}

		filterEdges(edges);
		return edges;
	}

	/**
	 * only include those edges that match user constraints
	 * @param edges
	 */
	private void filterEdges(Set<OWLGraphEdge> edges) {
		Set<OWLGraphEdge> rmEdges = new HashSet<OWLGraphEdge>();
		if (config.graphEdgeIncludeSet != null) {
			for (OWLGraphEdge e : edges) {
				if (!edgeSatisfiesOneOf(e, config.graphEdgeIncludeSet)) {
					rmEdges.add(e);
				}
			}
		}
		if (config.graphEdgeExcludeSet != null) {
			for (OWLGraphEdge e : edges) {
				if (edgeSatisfiesOneOf(e, config.graphEdgeExcludeSet)) {
					rmEdges.add(e);
				}
			}
		}
		//TODO
		//if (config.excludeMetaClass != null) {
		for (OWLGraphEdge e : edges) {
			OWLObject t = e.getTarget();
			if (t instanceof OWLNamedObject) {
				OWLNamedObject nt = (OWLNamedObject) t;
				// TODO
				if (nt.getIRI().toString().startsWith("http://www.ifomis.org/bfo"))
					rmEdges.add(e);
			}
		}

		//}

		for (OWLGraphEdge e : edges) {
			OWLObject t = e.getTarget();
			// TODO - use this: dataFactory.getOWLThing();
			if (t instanceof OWLNamedObject &&
					((OWLNamedObject) t).getIRI().equals(OWLRDFVocabulary.OWL_THING.getIRI())) {
				rmEdges.add(e);
			}
		}
		edges.removeAll(rmEdges);
	}

	private boolean edgeSatisfiesOneOf(OWLGraphEdge e, Set<OWLQuantifiedProperty> qps) {
		for (OWLQuantifiedProperty c : qps) {
			if (edgeSatisfies(e, c))
				return true;
		}
		return false;
	}

	private boolean edgeSatisfies(OWLGraphEdge e, OWLQuantifiedProperty c) {
		return c.subsumes(e.getSingleQuantifiedProperty());
	}

	// e.g. R-some-B ==> <R-some-B,R,B>
	private OWLGraphEdge restrictionToPrimitiveEdge(OWLRestriction s) {
		OWLObjectPropertyExpression p = null;
		OWLObject t = null;
		OWLQuantifiedProperty.Quantifier q = null;
		if (s instanceof OWLObjectSomeValuesFrom) {
			t  = ((OWLObjectSomeValuesFrom)s).getFiller();
			p = (OWLObjectPropertyExpression) s.getProperty();
			q = OWLQuantifiedProperty.Quantifier.SOME;
		}
		else if (s instanceof OWLObjectAllValuesFrom) {
			t  = ((OWLObjectAllValuesFrom)s).getFiller();
			p = (OWLObjectPropertyExpression) s.getProperty();
			q = OWLQuantifiedProperty.Quantifier.ONLY;
		}
		else if (s instanceof OWLObjectHasValue) {
			t  = ((OWLObjectHasValue)s).getValue();
			p = (OWLObjectPropertyExpression) s.getProperty();
			q = OWLQuantifiedProperty.Quantifier.VALUE;
		}
		else {
			System.err.println("cannot handle:"+s);
		}
		return new OWLGraphEdge(s,t,p,q,getSourceOntology());
	}

	private OWLGraphEdge createSubClassOfEdge(OWLObject s, OWLClassExpression t) {
		return new OWLGraphEdge(s,t,null,Quantifier.SUBCLASS_OF,getSourceOntology());
	}


	// extend an edge target until we hit a named object.
	// this could involve multiple extensions and "forks", e.g.
	// <A sub B^C> ==> <A sub B>, <A sub C>
	private Set<OWLGraphEdge> primitiveEdgeToFullEdges(OWLGraphEdge e) {
		Set<OWLGraphEdge> edges = new HashSet<OWLGraphEdge>();
		if (e.isTargetNamedObject()) {
			edges.add(e); // do nothing
		}
		else {
			// extend
			OWLObject s = e.getSource();
			Set<OWLGraphEdge> nextEdges = getOutgoingEdges(e.getTarget());
			for (OWLGraphEdge e2 : nextEdges) {
				edges.add(this.combineEdgePair(s, e, e2, 1));
			}
		}
		return edges;
	}



	/**
	 * in general you should not need to call this directly;
	 * used internally by this class.
	 */
	public void cacheEdges() {
		edgeBySource = new HashMap<OWLObject,Set<OWLGraphEdge>>();
		edgeByTarget = new HashMap<OWLObject,Set<OWLGraphEdge>>();

		// initialize with all named objects in ontology
		Stack<OWLObject> allObjs = new Stack<OWLObject>();
		allObjs.addAll(getAllOWLObjects());
		
		Set<OWLObject> visisted = new HashSet<OWLObject>();
		
		while (allObjs.size() > 0) {
			OWLObject s = allObjs.pop();
			if (visisted.contains(s))
				continue;
			visisted.add(s);
			if (!edgeBySource.containsKey(s))
				edgeBySource.put(s, new HashSet<OWLGraphEdge>());
			for (OWLGraphEdge edge : getPrimitiveOutgoingEdges(s)) {
				edgeBySource.get(s).add(edge);
				OWLObject t = edge.getTarget();
				if (!edgeByTarget.containsKey(t))
					edgeByTarget.put(t, new HashSet<OWLGraphEdge>());
				edgeByTarget.get(t).add(edge);
				
				// we also want to get all edges from class expressions;
				// class expressions aren't in the initial signature, but
				// we add them here when we encounter them
				if (t instanceof OWLClassExpression) {
					allObjs.add(t);
				}
			}
		}
	}

	/**
	 * @param t
	 * @return all edges that have t as a direct target
	 */
	public Set<OWLGraphEdge> getIncomingEdges(OWLObject t) {
		ensureEdgesCached();
		if (edgeByTarget.containsKey(t)) {
			return new HashSet<OWLGraphEdge>(edgeByTarget.get(t));
		}
		return new HashSet<OWLGraphEdge>();
	}


	private void ensureEdgesCached() {
		if (edgeByTarget == null)
			cacheEdges();

	}


	/**
	 * pack/translate an edge (either asserted or a graph closure edge) into
	 * an OWL class expression according to the OWLGraph to OWLOntology
	 * translation rules.
	 * 
	 * (this is the reverse translation of the one from an OWLOntology to an
	 * OWLGraph)
	 * 
	 * e.g. after calling for the graph closure of an OWLClass a,
	 * we may get back an edge <a [part_of-some, adjacent_to-some, has_part-some] b>.
	 * after feeding this edge into this method we obtain the expression
	 *   part_of some (adjacent_to some (has_part some b))
	 * 
	 * @param edge
	 * @return class expression equivalent to edge
	 */
	public OWLObject edgeToTargetExpression(OWLGraphEdge e) {
		return edgeToTargetExpression(e.getQuantifiedPropertyList().iterator(),e.getTarget());
	}
	private OWLObject edgeToTargetExpression(
			Iterator<OWLQuantifiedProperty> qpi, OWLObject t) {
		if (qpi.hasNext()) {
			OWLQuantifiedProperty qp = qpi.next();
			OWLObject x = edgeToTargetExpression(qpi,t);
			OWLClassExpression t2;
			if (!(x instanceof OWLClassExpression)) {
				//System.err.println("Not a CE: "+x);
				HashSet<OWLNamedIndividual> ins = new HashSet<OWLNamedIndividual>();
				ins.add((OWLNamedIndividual) x);
				t2 = dataFactory.getOWLObjectOneOf(ins);
			}
			else {
				t2 = (OWLClassExpression) x;
			}

			if (qp.isSubClassOf()) {
				return t2;
			}
			else if (qp.isInstanceOf()) {
				return t2;
			}
			else if (qp.isIdentity()) {
				return t2;
			}
			else if (qp.isPropertyAssertion()) {
				return dataFactory.getOWLObjectSomeValuesFrom(qp.getProperty(), 
						(OWLClassExpression) t2);
			}
			else if (qp.isSomeValuesFrom()) {
				return dataFactory.getOWLObjectSomeValuesFrom(qp.getProperty(), 
						(OWLClassExpression) t2);
			}
			else if (qp.isAllValuesFrom()) {
				return dataFactory.getOWLObjectAllValuesFrom(qp.getProperty(), 
						(OWLClassExpression) t2);
			}
			else if (qp.isHasValue()) {
				if (x instanceof OWLNamedObject)
					return dataFactory.getOWLObjectHasValue(qp.getProperty(), 
							dataFactory.getOWLNamedIndividual(((OWLNamedObject) x).getIRI()));
				else {
					System.err.println("warning: treating "+x+" as allvaluesfrom");
					return dataFactory.getOWLObjectAllValuesFrom(qp.getProperty(), 
							(OWLClassExpression) x);
				}
			}
			else {
				System.err.println("cannot handle:"+qp);
				// TODO
				return null;
			}
		}
		else {
			return t;
		}
	}


	// ----------------------------------------
	// GRAPH CLOSURE METHODS
	// ----------------------------------------


	/**
	 * Retrieves the graph closure originating from source.
	 * E.g. if A SubClassOf R some B & B SubClassOf S some C, then
	 * closure(A) = { <A R-some B>, <A [R-some,S-some] C>.
	 * 
	 * Composition rules are used to compact the list of connecting edge labels
	 * (e.g. transitivity).
	 * 
	 * The resulting edges can be translated into class expressions using 
	 * method edgeToTargetExpression(e). E.g. in the above the expression would be
	 *   R some (S some C)
	 * 
	 * @param source
	 * @return closure of edges originating from source
	 */
	public Set<OWLGraphEdge> getOutgoingEdgesClosure(OWLObject s) {

		if (config.isCacheClosure) {
			//System.out.println("@@checking cache for:"+s+" cache:"+inferredEdgeBySource == null ? "NULL" : "SET");
			if (inferredEdgeBySource == null)
				inferredEdgeBySource = new HashMap<OWLObject,Set<OWLGraphEdge>>();
			if (inferredEdgeBySource.containsKey(s)) {
				//System.out.println("@@cache:"+s+" -->"+inferredEdgeBySource.get(s).size());
				return new HashSet<OWLGraphEdge>(inferredEdgeBySource.get(s));
			}
		}
		Stack<OWLGraphEdge> edgeStack = new Stack<OWLGraphEdge>();
		Set<OWLGraphEdge> closureSet = new HashSet<OWLGraphEdge>();
		Set<OWLGraphEdge> visitedSet = new HashSet<OWLGraphEdge>();
		Set<OWLObject> visitedObjs = new HashSet<OWLObject>();
		visitedObjs.add(s);

		// initialize. we seed the search with a reflexive identity edge DEPR
		//edgeStack.add(new OWLGraphEdge(s,s,null,Quantifier.IDENTITY,ontology));

		// seed stack
		edgeStack.addAll(getPrimitiveOutgoingEdges(s));
		closureSet.addAll(edgeStack);
		while (!edgeStack.isEmpty()) {
			OWLGraphEdge ne = edgeStack.pop();
			//System.out.println("NEXT: "+ne+" //stack: "+edgeStack);
			int nextDist = ne.getDistance() + 1;
			Set<OWLGraphEdge> extSet = getPrimitiveOutgoingEdges(ne.getTarget());
			for (OWLGraphEdge extEdge : extSet) {
				//System.out.println("   EXT:"+extEdge);
				OWLGraphEdge nu = combineEdgePair(s, ne, extEdge, nextDist);
				OWLObject nuTarget = nu.getTarget();
				//System.out.println("     COMBINED:"+nu);

				// check for cycles. this is not as simple as
				// checking if we have visited the node, as we are interested
				// in different paths to the same node
				//if (!visitedSet.contains(nu)) {
				boolean isEdgeVisited = false;
				if (visitedObjs.contains(nuTarget)) {
					// we have potentially visited this edge before


					// TODO - this is temporary. need to check edge not node
					isEdgeVisited = true;
					/*
					System.out.println("checking to see if  visisted "+nu);
					System.out.println(nu.getFinalQuantifiedProperty());
					for (OWLGraphEdge ve : visitedSet) {
						System.out.println(" ve:"+ve.getFinalQuantifiedProperty());
						if (ve.getFinalQuantifiedProperty().equals(nu.getFinalQuantifiedProperty())) {
							System.out.println("already visisted: "+nu);
							isEdgeVisited = true;
						}
					}
					 */
				}
				else {
					visitedObjs.add(nuTarget);
				}

				if (!isEdgeVisited) {
					//System.out.println("      *NOT VISITED:"+nu+" visistedSize:"+visitedSet.size());
					if (nu.getTarget() instanceof OWLNamedObject || 
							config.isIncludeClassExpressionsInClosure) {
						closureSet.add(nu);
					}
					edgeStack.add(nu);
					visitedSet.add(nu);		

				}

			}
		}

		if (config.isCacheClosure) {
			inferredEdgeBySource.put(s, new HashSet<OWLGraphEdge>(closureSet));
		}
		return closureSet;
	}

	/**
	 * as getOutgoingEdgesClosure(s), but also includes an identity edge
	 * @param s
	 * @return
	 */
	public Set<OWLGraphEdge> getOutgoingEdgesClosureReflexive(OWLObject s) {
		Set<OWLGraphEdge> edges = getOutgoingEdgesClosure(s);
		edges.add(new OWLGraphEdge(s,s,null,Quantifier.IDENTITY,getSourceOntology()));
		return edges;
	}

	/**
	 * find the set of classes or class expressions subsuming source, using the graph closure.
	 * 
	 * this is just the composition of getOutgoingEdgesClosure and edgeToTargetExpression -- the
	 * latter method "packs" a chain of edges into a class expression
	 * 
	 * only "linear" expressions are found, corresponding to a path in the graph.
	 * e.g. [sub,part_of-some,develops_from-some] ==> part_of some (develops_from some t)
	 * 
	 * if the edge consists entirely of subclass links, the the subsumers will be all
	 * named classes.
	 * 
	 * @param source
	 * @return
	 */
	public Set<OWLObject> getSubsumersFromClosure(OWLObject s) {
		Set<OWLObject> ts = new HashSet<OWLObject>();
		for (OWLGraphEdge e : getOutgoingEdgesClosure(s)) {
			ts.add(edgeToTargetExpression(e));
		}
		return ts;
	}

	public Set<OWLObject> queryDescendants(OWLGraphEdge e) {
		Set<OWLObject> results = new HashSet<OWLObject>();
		// reflexivity
		results.add(this.edgeToTargetExpression(e));
		List<OWLQuantifiedProperty> eqpl = e.getQuantifiedPropertyList();
		// todo - cast
		for (OWLObject d1 : queryDescendants((OWLClassExpression)e.getTarget())) {
			Set<OWLGraphEdge> dEdges = this.getIncomingEdgesClosure(d1);
			for (OWLGraphEdge dEdge : dEdges) {
				OWLGraphEdge newEdge = combineEdgePair(e.getTarget(), 
						new OWLGraphEdge(null, null, Quantifier.SUBCLASS_OF), dEdge, 1);
				List<OWLQuantifiedProperty> dqpl = newEdge.getQuantifiedPropertyList();
				if (dqpl.equals(eqpl)) {
					results.add(dEdge.getSource());
				}
			}
		}
		return results;
	}

	public Set<OWLObject> queryDescendants(OWLClassExpression t) {
		return queryDescendants(t, true, true);
	}
	
	public Set<OWLObject> queryDescendants(OWLClassExpression t, boolean isInstances, boolean isClasses) {
		Set<OWLObject> results = new HashSet<OWLObject>();
		results.add(t);

		// transitivity and link composition
		Set<OWLGraphEdge> dEdges = this.getIncomingEdgesClosure(t);
		for (OWLGraphEdge dEdge : dEdges) {
			if (dEdge.getQuantifiedPropertyList().size() > 1)
				continue;
			OWLQuantifiedProperty qp = dEdge.getSingleQuantifiedProperty();
			if ((isInstances && qp.isInstanceOf()) || 
					(isClasses && qp.isSubClassOf()))
				results.add(dEdge.getSource());
		}

		if (t instanceof OWLObjectIntersectionOf) {
			Set<OWLObject> iresults = null;
			for (OWLClassExpression y : ((OWLObjectIntersectionOf)t).getOperands()) {
				if (iresults == null) {
					iresults = queryDescendants(y, isInstances, isClasses);
				}
				else {
					iresults.retainAll(queryDescendants(y, isInstances, isClasses));
				}
				results.addAll(iresults);
			}
		}
		else if (t instanceof OWLObjectUnionOf) {
			Set<OWLObject> iresults = null;
			for (OWLClassExpression y : ((OWLObjectUnionOf)t).getOperands()) {
				results.addAll(queryDescendants(y, isInstances, isClasses));
			}
		}
		else if (t instanceof OWLRestriction) {
			results.addAll(queryDescendants(restrictionToPrimitiveEdge((OWLRestriction) t)));
		}

		// equivalent classes - substitute a named class in the query for an expression
		if (t instanceof OWLClass) {
			for (OWLOntology ont : this.getAllOntologies()) {
				for (OWLEquivalentClassesAxiom ax : ont.getEquivalentClassesAxioms((OWLClass)t)) {
					for (OWLClassExpression y : ax.getClassExpressions()) {
						if (y instanceof OWLClass)
							continue;
						results.addAll(queryDescendants(y, isInstances, isClasses));
					}
				}
			}
		}

		return results;
	}

	/**
	 * @param source
	 * @param target
	 * @return all edges connecting source and target in the graph closure
	 */

	public Set<OWLGraphEdge> getEdgesBetween(OWLObject s, OWLObject t) {
		Set<OWLGraphEdge> allEdges = getOutgoingEdgesClosureReflexive(s);
		Set<OWLGraphEdge> edges = new HashSet<OWLGraphEdge>();
		for (OWLGraphEdge e : allEdges) {
			if (e.getTarget().equals(t))
				edges.add(e);
		}
		return edges;
	}


	/**
	 * returns all ancestors of an object. Here, ancestors is defined as any
	 * named object that can be reached from x over some path of asserted edges.
	 * relations are ignored.
	 * 
	 * @param source
	 * @return all reachable target nodes, regardless of edges
	 */
	public Set<OWLObject> getAncestors(OWLObject x) {
		Set<OWLObject> ancs = new HashSet<OWLObject>();
		for (OWLGraphEdge e : getOutgoingEdgesClosure(x)) {
			ancs.add(e.getTarget());
		}
		return ancs;
	}
	public Set<OWLObject> getAncestorsReflexive(OWLObject x) {
		Set<OWLObject> ancs = getAncestors(x);
		ancs.add(x);
		return ancs;
	}
	
	public Set<OWLObject> getNamedAncestors(OWLObject x) {
		Set<OWLObject> ancs = new HashSet<OWLObject>();
		for (OWLGraphEdge e : getOutgoingEdgesClosure(x)) {
			if (e.getTarget() instanceof OWLNamedObject)
				ancs.add(e.getTarget());
		}
		return ancs;
	}
	public Set<OWLObject> getNamedAncestorsReflexive(OWLObject x) {
		Set<OWLObject> ancs = getNamedAncestors(x);
		ancs.add(x);
		return ancs;
	}

	/**
	 * see getAncestors()
	 * @param x
	 * @return
	 */
	public Set<OWLObject> getDescendants(OWLObject x) {
		Set<OWLObject> descs = new HashSet<OWLObject>();
		for (OWLGraphEdge e : getIncomingEdgesClosure(x)) {
			descs.add(e.getSource());
		}
		return descs;
	}
	public Set<OWLObject> getDescendantsReflexive(OWLObject x) {
		Set<OWLObject> getDescendants = getDescendants(x);
		getDescendants.add(x);
		return getDescendants;
	}
	public Set<OWLObject> getIndividualDescendants(OWLObject x) {
		Set<OWLObject> descs = new HashSet<OWLObject>();
		for (OWLGraphEdge e : getIncomingEdgesClosure(x)) {
			OWLObject s = e.getSource();
			if (s instanceof OWLIndividual)
				descs.add(s);
		}
		return descs;
	}


	/**
	 * TODO
	 * @see getOutgoingEdgesClosure
	 * @param target
	 * @return all edges connecting all descendents of target to target
	 */
	public Set<OWLGraphEdge> getIncomingEdgesClosure(OWLObject t) {

		if (config.isCacheClosure) {
			if (inferredEdgeByTarget == null)
				inferredEdgeByTarget = new HashMap<OWLObject,Set<OWLGraphEdge>>();
			if (inferredEdgeByTarget.containsKey(t)) {
				return new HashSet<OWLGraphEdge>(inferredEdgeByTarget.get(t));
			}
		}

		Stack<OWLGraphEdge> edgeStack = new Stack<OWLGraphEdge>();
		Set<OWLGraphEdge> closureSet = new HashSet<OWLGraphEdge>();
		Set<OWLGraphEdge> visitedSet = new HashSet<OWLGraphEdge>();
		Set<OWLObject> visitedObjs = new HashSet<OWLObject>();

		// initialize -
		// note that edges are always from src to tgt. here we are extending down from tgt to src
		
		//edgeStack.add(new OWLGraphEdge(t,t,ontology,new OWLQuantifiedProperty()));
		edgeStack.addAll(getIncomingEdges(t));
		closureSet.addAll(edgeStack);

		while (!edgeStack.isEmpty()) {
			OWLGraphEdge ne = edgeStack.pop();

			int nextDist = ne.getDistance() + 1;
			
			// extend down from this edge; e.g. [s, extEdge + ne, tgt] 
			Set<OWLGraphEdge> extSet = getIncomingEdges(ne.getSource());
			for (OWLGraphEdge extEdge : extSet) {
				
				// combine [extEdge + ne]
				OWLGraphEdge nu = combineEdgePairDown(ne, extEdge, nextDist);
				OWLObject nusource = nu.getSource();

				boolean isEdgeVisited = false;
				if (visitedObjs.contains(nusource)) {
					isEdgeVisited = true;
				}
				else {
					visitedObjs.add(nusource);
				}

				if (!isEdgeVisited) {
					if (nu.getSource() instanceof OWLNamedObject || 
							config.isIncludeClassExpressionsInClosure) {
						closureSet.add(nu);
					}
					edgeStack.add(nu);
					visitedSet.add(nu);		

				}

			}
		}

		if (config.isCacheClosure) {
			inferredEdgeByTarget.put(t, new HashSet<OWLGraphEdge>(closureSet));
		}

		return closureSet;
	}


	/**
	 * combine ne and extEdge to create a new edge.
	 * 
	 * @param s
	 * @param ne
	 * @param extEdge
	 * @param nextDist
	 * @return
	 */
	@Deprecated
	public OWLGraphEdge old___combineEdgePair(OWLObject s, OWLGraphEdge ne, OWLGraphEdge extEdge, int nextDist) {
		//System.out.println("combing edges: "+ne+ " * "+extEdge);
		// Create an edge with no edge labels; we will fill the label in later
		OWLGraphEdge nu = new OWLGraphEdge(s, extEdge.getTarget());
		nu.setDistance(nextDist);
		Vector<OWLQuantifiedProperty> qps = new Vector<OWLQuantifiedProperty>();

		// TODO - PropertyChains - need to match the tail of the list of QRs

		// put all but the final one in a new list
		int n = 0;
		int size = ne.getQuantifiedPropertyList().size();
		OWLQuantifiedProperty finalQP = null;
		for (OWLQuantifiedProperty qp : ne.getQuantifiedPropertyList()) {
			n++;
			if (n < size)
				qps.add(qp);
			else
				finalQP = qp;
		}
		OWLQuantifiedProperty combinedQP = 
			combinedQuantifiedPropertyPair(ne.getFinalQuantifiedProperty(), extEdge.getSingleQuantifiedProperty());
		if (combinedQP == null) {
			qps.add(finalQP);
			qps.add(extEdge.getSingleQuantifiedProperty());
		}
		else {
			qps.add(combinedQP);
		}
		nu.setQuantifiedPropertyList(qps);
		//System.out.println("DONE edges: "+ne+ " * "+extEdge+" ==> "+nu);

		return nu;
	}

	public OWLGraphEdge combineEdgePair(OWLObject s, OWLGraphEdge ne, OWLGraphEdge extEdge, int nextDist) {
		//System.out.println("combing edges: "+ne+ " * "+extEdge);
		// Create an edge with no edge labels; we will fill the label in later
		OWLGraphEdge nu = new OWLGraphEdge(s, extEdge.getTarget());
		List<OWLQuantifiedProperty> qpl1 = new Vector<OWLQuantifiedProperty>(ne.getQuantifiedPropertyList());
		List<OWLQuantifiedProperty> qpl2 = new Vector<OWLQuantifiedProperty>(extEdge.getQuantifiedPropertyList());

		while (qpl1.size() > 0 && qpl2.size() > 0) {
			OWLQuantifiedProperty combinedQP = combinedQuantifiedPropertyPair(qpl1.get(qpl1.size()-1),qpl2.get(0));
			if (combinedQP == null)
				break;
			qpl1.set(qpl1.size()-1, combinedQP);
			if (combinedQP.isIdentity())
				qpl1.subList(qpl1.size()-1,qpl1.size()).clear();
			qpl2.subList(0, 1).clear();
		}
		qpl1.addAll(qpl2);
		nu.setQuantifiedPropertyList(qpl1);
		nu.setDistance(nextDist);
		return nu;
	}

	/**
	 *  combine [srcEdge + tgtEdge]
	 *  
	 * @param tgtEdge
	 * @param t
	 * @param srcEdge
	 * @param nextDist
	 * @return
	 */
	private OWLGraphEdge combineEdgePairDown(OWLGraphEdge tgtEdge, OWLGraphEdge srcEdge, int nextDist) {
		// fill in edge label later
		// todo
		OWLGraphEdge nu = new OWLGraphEdge(srcEdge.getSource(), tgtEdge.getTarget());
		nu.setDistance(nextDist);
		Vector<OWLQuantifiedProperty> qps = new Vector<OWLQuantifiedProperty>();

		// put all but the final one in a new list
		int n = 0;
		int size = tgtEdge.getQuantifiedPropertyList().size();
		OWLQuantifiedProperty finalQP = null;
		for (OWLQuantifiedProperty qp : tgtEdge.getQuantifiedPropertyList()) {
			n++;
			if (n > 1)
				qps.add(qp);
			else
				finalQP = qp;
		}
		// TODO
		// join src+tgt edge
		OWLQuantifiedProperty combinedQP = 
			combinedQuantifiedPropertyPair(srcEdge.getFinalQuantifiedProperty(), tgtEdge.getSingleQuantifiedProperty());
		//combinedQuantifiedPropertyPair(tgtEdge.getFinalQuantifiedProperty(), srcEdge.getSingleQuantifiedProperty());
		if (combinedQP == null) {
			qps.add(finalQP);
			qps.add(srcEdge.getSingleQuantifiedProperty());
		}
		else {
			qps.add(combinedQP);
		}
		nu.setQuantifiedPropertyList(qps);
		return nu;
	}

	/**
	 * Edge composition rules
	 */
	private OWLQuantifiedProperty combinedQuantifiedPropertyPair(OWLQuantifiedProperty x, OWLQuantifiedProperty y) {
		//System.out.println("combing "+x+"+"+y);
		if (x.isSubClassOf() && y.isSubClassOf()) {
			return new OWLQuantifiedProperty(Quantifier.SUBCLASS_OF);
		}
		else if (x.isInstanceOf() && y.isSubClassOf()) {
			return new OWLQuantifiedProperty(Quantifier.INSTANCE_OF);
		}
		else if (x.isSubClassOf() && y.isSomeValuesFrom()) {
			return new OWLQuantifiedProperty(y.getProperty(),Quantifier.SOME);
		}
		else if (x.isSomeValuesFrom() && y.isSubClassOf()) {
			return new OWLQuantifiedProperty(x.getProperty(),Quantifier.SOME);
		}
		else if (x.isSomeValuesFrom() &&
				y.isSomeValuesFrom() &&
				x.getProperty() != null && 
				x.getProperty().equals(y.getProperty()) && 
				x.getProperty().isTransitive(ontology)) { // todo
			return new OWLQuantifiedProperty(x.getProperty(),Quantifier.SOME);
		}
		else if (x.isPropertyAssertion() &&
				y.isPropertyAssertion() &&
				x.getProperty() != null && 
				x.getProperty().equals(y.getProperty()) && 
				x.getProperty().isTransitive(ontology)) { // todo
			return new OWLQuantifiedProperty(x.getProperty(),Quantifier.PROPERTY_ASSERTION);
		}
		else if (x.isPropertyAssertion() &&
				y.isPropertyAssertion() &&
				x.getProperty() != null && 
				isInverseOfPair(x.getProperty(),y.getProperty())) { // todo
			return new OWLQuantifiedProperty(Quantifier.IDENTITY); // TODO - doesnt imply identity
		}
		else if (x.isSubClassOf() && y.isAllValuesFrom()) {
			return new OWLQuantifiedProperty(y.getProperty(),Quantifier.ONLY);
		}
		else if (x.isAllValuesFrom() && y.isSubClassOf()) {
			return new OWLQuantifiedProperty(x.getProperty(),Quantifier.ONLY);
		}
		else {
			// cannot combine - caller will add QP to sequence
			return null;
		}
	}

	private boolean isInverseOfPair(OWLObjectProperty p1, OWLObjectProperty p2) {
		for (OWLOntology ont : getAllOntologies()) {
			for (OWLInverseObjectPropertiesAxiom a : ont.getInverseObjectPropertyAxioms(p1)) {
				if (a.getFirstProperty().equals(p2) ||
						a.getSecondProperty().equals(p2)) {
					return true;
				}
			}
		}
		return false;
	}

	/**
	 * Find all edges of the form <i INST c> in the graph closure.
	 * (this includes both direct assertions, plus assertions to objects
	 *  that link to c via a chain of SubClassOf assertions)
	 *  
	 *  the semantics are the same as inferred ClassAssertion axioms
	 * 
	 * @param owlClass
	 * @return all individuals classified here via basic graph traversal
	 */
	public Set<OWLIndividual> getInstancesFromClosure(OWLClass c) {
		Set<OWLIndividual> ins = new HashSet<OWLIndividual>();
		for (OWLOntology o : getAllOntologies()) {
			// iterate through all individuals; sequential scan may be slow for
			// large knowledge bases
			for (OWLIndividual in : o.getIndividualsInSignature()) {
				for (OWLGraphEdge e : getEdgesBetween(in, c)) {
					List<OWLQuantifiedProperty> qps = e.getQuantifiedPropertyList();
					// check for edges of the form < i INSTANCE_OF c >
					// we exclude relation chaims, e.g. <i [INSTANCE_OF PART_OF-some] c>
					if (qps.size() == 1 && qps.get(0).isInstanceOf()) {
						ins.add(in);
						break;
					}
				}
			}
		}
		return ins;
	}

	/**
	 * Finds all edges between an instance i and he given class c.
	 * 
	 * this includes inferred class assertions, as well as chains such as
	 * 
	 * i has_part j, j inst_of k, k part_of some c
	 * 
	 * @param owlClass
	 * @return all edges in closure between an instance and owlClass
	 */
	public Set<OWLGraphEdge> getInstanceChainsFromClosure(OWLClass c) {
		Set<OWLGraphEdge> edges = new HashSet<OWLGraphEdge>();
		for (OWLOntology o : getAllOntologies()) {
			// iterate through all individuals; sequential scan may be slow for
			// large knowledge bases
			for (OWLIndividual in : o.getIndividualsInSignature()) {
				edges.addAll(getEdgesBetween(in, c));
			}
		}
		return edges;
	}

	/**
	 * returns all named superclasses of a named class.
	 * 
	 * Currently this is implemented via graph traversal - any path in which the
	 * intermediate notes are solely subclass edges (and including the reflexive case)
	 * 
	 * (corresponds to inferred_subclass_of in gold)
	 * 
	 * @param cls
	 * @return list of named classes
	 */
	public Set<OWLClass> getInferredSuperclasses(OWLClass c) {
		// TODO
		return null;
	}

	/**
	 * returns all outgoing edges that consist of a [some-R] inferred link.
	 * also includes edge chains in which all the quantifiers are 'some], e.g.
	 * [some-R1,some-R2]
	 * 
	 * (corresponds to inferred_all_some_relationship in gold)
	 * 
	 * @param cls
	 * @return list of named classes
	 */
	public Set<OWLGraphEdge> getInferredAllSomeRelationships(OWLClass c) {
		// TODO
		return null;
	}
	
	
	/**
	 * Given a set of OWLObjects (for example, all OWLClass objects in a GO slim),
	 * find the set of asserted and inferred edges that connect
	 * 
	 * @param objs
	 * @return
	 */
	public Set<OWLGraphEdge> getMinimalEdgesFromSubset(Set<OWLObject> objs) {
		return null;
	}

	// ----------------------------------------
	// BASIC WRAPPER UTILITIES
	// ----------------------------------------

	/**
	 * @return all named objects
	 */
	public Set<OWLObject> getAllOWLObjects() {
		Set<OWLObject> obs = new HashSet<OWLObject>();
		for (OWLOntology o : getAllOntologies()) {
			obs.addAll(o.getClassesInSignature());
			obs.addAll(o.getIndividualsInSignature());
			obs.addAll(o.getObjectPropertiesInSignature());
		}
		return obs;
	}


	/**
	 * assumes zero or one rdfs:label
	 * 
	 * @param c
	 * @return
	 */
	public String getLabel(OWLObject c) {
		OWLAnnotationProperty lap = dataFactory.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_LABEL.getIRI()); 
		return getAnnotationValue(c, lap);
	}
	public String getLabelOrDisplayId(OWLObject c) {
		String label = getLabel(c);
		if (label == null) {
			if (c instanceof OWLNamedObject) {
				OWLNamedObject nc = (OWLNamedObject)c;
				label = nc.getIRI().getFragment();
			}
			else {
				label = c.toString();
			}
		}
		return label;
	}


	public String getComment(OWLObject c) {
		OWLAnnotationProperty lap = dataFactory.getOWLAnnotationProperty(OWLRDFVocabulary.RDFS_COMMENT.getIRI()); 

		return getAnnotationValue(c, lap);
	}


	public String getAnnotationValue(OWLObject c, OWLAnnotationProperty lap) {
		Set<OWLAnnotation>anns = new HashSet<OWLAnnotation>();
		if (c instanceof OWLEntity) {
			for (OWLOntology ont : getAllOntologies()) {
				anns.addAll(((OWLEntity) c).getAnnotations(ont,lap));
			}
		}
		else {
			return null;
		}
		for (OWLAnnotation a : anns) {
			if (a.getValue() instanceof OWLLiteral) {
				OWLLiteral val = (OWLLiteral) a.getValue();
				return val.getLiteral(); // return first - todo - check zero or one
			}
		}
		return null;
	}

	public String[] getAnnotationValues(OWLObject c, OWLAnnotationProperty lap) {
		Set<OWLAnnotation>anns = new HashSet<OWLAnnotation>();
		if (c instanceof OWLEntity) {
			for (OWLOntology ont : getAllOntologies()) {
				anns.addAll(((OWLEntity) c).getAnnotations(ont,lap));
			}
		}
		else {
			return null;
		}
		
		ArrayList<String> list = new ArrayList<String>();
		for (OWLAnnotation a : anns) {
			if (a.getValue() instanceof OWLLiteral) {
				OWLLiteral val = (OWLLiteral) a.getValue();
				list.add( val.getLiteral()); 
			}
		}
		
		String ar[] = new String[list.size()];
		
		return list.toArray(ar);
	}

	
	/**
	 * assumes zero or one def
	 * It returns the definition text (encoded as def in obo format and IAO_0000115 annotation property in OWL format) of a class
	 * @param c
	 * @return
	 */
	public String getDef(OWLObject c) {
		OWLAnnotationProperty lap = dataFactory.getOWLAnnotationProperty(Obo2OWLVocabulary.IRI_IAO_0000115.getIRI()); 

		return getAnnotationValue(c, lap);
	}

	/**
	 * It returns the value of the is_metadata_tag tag.
	 * @param c could OWLClass or OWLObjectProperty
	 * @return
	 */
	public boolean getIsMetaTag(OWLObject c) {
		OWLAnnotationProperty lap = getAnnotationProperty(OboFormatTag.TAG_IS_METADATA_TAG.getTag());

		String val = getAnnotationValue(c, lap);

		return val == null ? false: Boolean.valueOf(val);
	}

	/**
	 * It returns the value of the subset tag.
	 * @param c could OWLClass or OWLObjectProperty
	 * @return
	 */
	public String getSubset(OWLObject c) {
		OWLAnnotationProperty lap = getAnnotationProperty(OboFormatTag.TAG_SUBSET.getTag());

		return getAnnotationValue(c, lap);
	}

	/**
	 * It returns the value of the domain tag
	 * @param prop
	 * @return
	 */
	public String getDomain(OWLObjectProperty prop){
		Set<OWLClassExpression> domains = prop.getDomains(ontology);

		for(OWLClassExpression ce: domains){
			return getIdentifier(ce);
		}

		return null;
	}


	/**
	 * It returns the value of the range tag
	 * @param prop
	 * @return
	 */
	public String getRange(OWLObjectProperty prop){
		Set<OWLClassExpression> domains = prop.getRanges(ontology);

		for(OWLClassExpression ce: domains){
			return getIdentifier(ce);
		}

		return null;
	}


	/**
	 * It returns the value of the replaced_by tag or IAO_0100001 annotation.
	 * @param c could OWLClass or OWLObjectProperty
	 * @return
	 */
	public String[] getReplacedBy(OWLObject c) {
		OWLAnnotationProperty lap = dataFactory.getOWLAnnotationProperty(Obo2OWLVocabulary.IRI_IAO_0100001.getIRI());

		return getAnnotationValues(c, lap);
	}

	/**
	 * It returns the value of the replaced_by tag.
	 * @param c could OWLClass or OWLObjectProperty
	 * @return
	 */
	public String getConsider(OWLObject c) {
		OWLAnnotationProperty lap = getAnnotationProperty(OboFormatTag.TAG_CONSIDER.getTag());

		return getAnnotationValue(c, lap);
	}




	/**
	 * It returns the value of the is-obsolete tag.
	 * @param c could OWLClass or OWLObjectProperty
	 * @return
	 */
	public boolean getIsObsolete(OWLObject c) {
		OWLAnnotationProperty lap = getAnnotationProperty(OboFormatTag.TAG_IS_OBSELETE.getTag()); 

		String val = getAnnotationValue(c, lap);

		return val == null ? false: Boolean.valueOf(val);
	}



	/**
	 * It returns the value of the alt_id tag
	 * @param c
	 * @return
	 */
	public String getAltId(OWLObject c) {
		OWLAnnotationProperty lap = getAnnotationProperty(OboFormatTag.TAG_ALT_ID.getTag());

		return getAnnotationValue(c, lap);
	}

	/**
	 * It returns the value of the builtin tag
	 * @param c
	 * @return
	 */
	public boolean getBuiltin(OWLObject c) {
		OWLAnnotationProperty lap = getAnnotationProperty(OboFormatTag.TAG_BUILTIN.getTag());

		String val = getAnnotationValue(c, lap);

		return val == null ? false: Boolean.valueOf(val);
	}

	/**
	 * It returns the value of the is_anonymous tag
	 * @param c
	 * @return
	 */
	public boolean getIsAnonymous(OWLObject c) {
		OWLAnnotationProperty lap = getAnnotationProperty(OboFormatTag.TAG_IS_ANONYMOUS.getTag());

		String val = getAnnotationValue(c, lap);

		return val == null ? false: Boolean.valueOf(val);
	}





	/**
	 * It translates a oboformat tag into an OWL annotation property
	 * @param tag
	 * @return
	 */
	public OWLAnnotationProperty getAnnotationProperty(String tag){
		//return dataFactory.getOWLAnnotationProperty(IRI.create(DEFAULT_IRI_PREFIX + "IAO_"+ tag)); 
		return dataFactory.getOWLAnnotationProperty(Obo2Owl.trTagToIRI(tag));
	}


	/**
	 * It returns the value of the namespace tag
	 * @param c
	 * @return
	 */
	public String getNamespace(OWLObject c) {
		OWLAnnotationProperty lap = getAnnotationProperty(OboFormatTag.TAG_NAMESPACE.getTag());

		return getAnnotationValue(c, lap);
	}


	/**
	 * It returns the value of the created_by tag
	 * @param c
	 * @return
	 */
	public String getCreatedBy(OWLObject c) {
		OWLAnnotationProperty lap = getAnnotationProperty(OboFormatTag.TAG_CREATED_BY.getTag()); 

		return getAnnotationValue(c, lap);
	}


	/**
	 * It returns the value of the is_anti_symmetric tag or IAO_0000427 annotation
	 * @param c
	 * @return
	 */
	public boolean getIsAntiSymmetric(OWLObject c) {
		OWLAnnotationProperty lap = dataFactory.getOWLAnnotationProperty(Obo2OWLVocabulary.IRI_IAO_0000427.getIRI());
			
		String val = getAnnotationValue(c, lap);

		return val == null ? false: Boolean.valueOf(val);
	}


	/**
	 * It returns the value of the is_cyclic tag 
	 * @param c
	 * @return
	 */
	public boolean getIsCyclic(OWLObject c) {
		OWLAnnotationProperty lap = getAnnotationProperty(OboFormatTag.TAG_IS_CYCLIC.getTag()); 

		String val = getAnnotationValue(c, lap);

		return val == null ? false: Boolean.valueOf(val);
	}


	// TODO - fix for multiple ontologies
	public boolean getIsTransitive(OWLObjectProperty c) {
		Set<OWLTransitiveObjectPropertyAxiom> ax = sourceOntology.getTransitiveObjectPropertyAxioms(c);

		return ax.size()>0;
	}

	// TODO - fix for multiple ontologies
	public boolean getIsFunctional(OWLObjectProperty c) {
		Set<OWLFunctionalObjectPropertyAxiom> ax = sourceOntology.getFunctionalObjectPropertyAxioms(c);

		return ax.size()>0;
	}

	// TODO - fix for multiple ontologies
	public boolean getIsInverseFunctional(OWLObjectProperty c) {
		Set<OWLInverseFunctionalObjectPropertyAxiom> ax = sourceOntology.getInverseFunctionalObjectPropertyAxioms(c);

		return ax.size()>0;
	}



	// TODO - fix for multiple ontologies
	public boolean getIsReflexive(OWLObjectProperty c) {
		Set<OWLReflexiveObjectPropertyAxiom> ax = sourceOntology.getReflexiveObjectPropertyAxioms(c);

		return ax.size()>0;
	}

	// TODO - fix for multiple ontologies
	public boolean getIsSymmetric(OWLObjectProperty c) {
		Set<OWLSymmetricObjectPropertyAxiom> ax = sourceOntology.getSymmetricObjectPropertyAxioms(c);

		return ax.size()>0;
	}

	public Set<OWLObjectPropertyExpression> getSuperPropertiesOf(OWLObjectPropertyExpression p) {
		Set<OWLObjectPropertyExpression> ps = new HashSet<OWLObjectPropertyExpression>();
		for (OWLOntology ont : getAllOntologies()) {
			for (OWLSubObjectPropertyOfAxiom a : ont.getObjectSubPropertyAxiomsForSubProperty(p)) {
				ps.add(a.getSuperProperty());
			}
		}
		return ps;
	}

	/**
	 * 
	 * @param c
	 * @return It returns null if no xref annotation is found.
	 */

	public List<String> getXref(OWLObject c){
		OWLAnnotationProperty lap = getAnnotationProperty(OboFormatTag.TAG_XREF.getTag());

		Set<OWLAnnotation>anns = null;
		if (c instanceof OWLEntity) {
			anns = ((OWLEntity) c).getAnnotations(sourceOntology,lap);
		}
		else {
			return null;
		}
		List<String> list = new ArrayList<String>();
		for (OWLAnnotation a : anns) {


			if (a.getValue() instanceof OWLLiteral) {
				OWLLiteral val = (OWLLiteral) a.getValue();
				list.add( val.getLiteral()) ;
			}
		}
		return list;
	}


	public List<String> getDefXref(OWLObject c){
		OWLAnnotationProperty lap = dataFactory.getOWLAnnotationProperty(Obo2OWLVocabulary.IRI_IAO_0000115.getIRI()); 
		OWLAnnotationProperty xap = getAnnotationProperty(OboFormatTag.TAG_XREF.getTag());

		List<String> list = new ArrayList<String>();

		if(c instanceof OWLEntity){
			for (OWLAnnotationAssertionAxiom oaax :((OWLEntity) c).getAnnotationAssertionAxioms(sourceOntology)){

				if(lap.equals(oaax.getProperty())){

					for(OWLAnnotation a: oaax.getAnnotations(xap)){
						if(a.getValue() instanceof OWLLiteral){
							list.add( ((OWLLiteral)a.getValue()).getLiteral() );
						}
					}
				}

			}
		}

		return list;
	}


	/**
	 * Return the names of the asserted subClasses of the cls (Class) 
	 * passed in the argument
	 * 
	 * 
	 * @param cls
	 * @return
	 */
	@Deprecated
	public String[] getSubClassesNames(OWLClass cls){
		Set<OWLClassExpression> st = cls.getSubClasses(sourceOntology);


		ArrayList<String> ar = new ArrayList<String>();
		for(OWLClassExpression ce: st){
			if(ce instanceof OWLNamedObject)
				ar.add(getLabel(ce)); 
		}

		return ar.toArray(new String[ar.size()]);
	}

	/**
	 * It returns array of synonyms (is encoded as synonym in obo format and IAO_0000118 annotation property in OWL format) of a class
	 * @param c
	 * @return
	 */
	@Deprecated
	public String[] getSynonymStrings(OWLObject c) {
		OWLAnnotationProperty lap = dataFactory.getOWLAnnotationProperty(IRI.create(DEFAULT_IRI_PREFIX + "IAO_0000118")); 
		Set<OWLAnnotation>anns = null;
		if (c instanceof OWLEntity) {
			anns = ((OWLEntity) c).getAnnotations(sourceOntology,lap);
		}
		else {
			return null;
		}

		ArrayList<String> list = new ArrayList<String>();
		for (OWLAnnotation a : anns) {
			if (a.getValue() instanceof OWLLiteral) {
				OWLLiteral val = (OWLLiteral) a.getValue();
				list.add(val.getLiteral()); // return first - todo - check zero or one
			}
		}
		return list.toArray(new String[list.size()]);
	}


	public String getOntologyId(){
		return Owl2Obo.getOntologyId(this.ontology);
	}


	public String getIdentifier(OWLObject owlObject) {
		return Owl2Obo.getIdentifier(owlObject);
	}


	public String getIdentifier(IRI iriId) {
		return Owl2Obo.getIdentifier(iriId);
	}

	@Deprecated
	public IRI getIRIByIdentifier(String id) {
		String[] parts = id.split(":", 2);
		String s;
		if (parts.length <2) {
			// TODO!
			s = "http://purl.obolibrary.org/obo/TODO_"+parts[0];
		}
		else {
			s = "http://purl.obolibrary.org/obo/"+parts[0]+"_"+parts[1];
		}

		return IRI.create(s);
	}

	/**
	 * translates to obo URIs
	 * 
	 * @param id - e.g. GO:0008150
	 * @return
	 */
	public OWLObject getOWLObjectByIdentifier(String id) {
		return dataFactory.getOWLClass(getIRIByIdentifier(id));
	}

	public OWLObject getOWLObjectPropertyByIdentifier(String id) {
		return dataFactory.getOWLObjectProperty(getIRIByIdentifier(id));
	}
	public OWLNamedIndividual getOWLIndividualByIdentifier(String id) {
		return dataFactory.getOWLNamedIndividual(getIRIByIdentifier(id));
	}

	/**
	 * fetches an OWL Object by rdfs:label.
	 * 
	 * if there is >1 match, return the first one encountered
	 * 
	 * @param label
	 * @return
	 */
	public OWLObject getOWLObjectByLabel(String label) {
		for (OWLOntology o : getAllOntologies()) {
			Set<OWLAnnotationAssertionAxiom> aas = o.getAxioms(AxiomType.ANNOTATION_ASSERTION);
			for (OWLAnnotationAssertionAxiom aa : aas) {
				// TODO - check for label
				OWLAnnotationValue v = aa.getValue();
				if (v instanceof OWLLiteral) {
					if (label.equals( ((OWLLiteral)v).getLiteral())) {
						OWLAnnotationSubject obj = aa.getSubject();
						if (obj instanceof IRI) {
							return getOWLObject( ((IRI)obj) );
						}
						return obj;
					}
				}
			}
		}
		return null;
	}


	/**
	 * Returns an OWLClass given an IRI string.
	 * 
	 * the class must be declared in either the source ontology, or in a support ontology,
	 * otherwise null is returned
	 * 
	 * @param iri
	 * @return
	 */
	public OWLClass getOWLClass(String s) {
		IRI iri = IRI.create(s);
		return getOWLClass(iri);
	}
	public OWLClass getOWLClass(IRI iri) {
		OWLClass c = getDataFactory().getOWLClass(iri);
		for (OWLOntology o : getAllOntologies()) {
			if (o.getDeclarationAxioms(c).size() > 0) {
				return c;
			}
		}
		return null;
	}

	public OWLClass getOWLClass(OWLObject x) {
		return dataFactory.getOWLClass(((OWLNamedObject)x).getIRI());
	}


	public OWLNamedIndividual getOWLIndividual(IRI iri) {
		OWLNamedIndividual c = dataFactory.getOWLNamedIndividual(iri);
		/*
		for (OWLOntology o : getAllOntologies()) {
			if (o.getDeclarationAxioms(c).size() > 0) {
				return c;
			}
		}
		return null;
		 */
		return c;
	}
	public OWLObject getOWLIndividual(String s) {
		IRI iri = IRI.create(s);
		return getOWLIndividual(iri);
	}

	public OWLObjectProperty getOWLObjectProperty(String id) {
		return dataFactory.getOWLObjectProperty(IRI.create(id));
	}


	// TODO - make this more efficient
	public OWLObject getOWLObject(String s) {
		OWLObject o;
		o = getOWLClass(s);
		if (o == null) {
			o = getOWLIndividual(s);
		}
		return o;
	}

	private OWLObject getOWLObject(IRI s) {
		OWLObject o;
		o = getOWLClass(s);
		if (o == null) {
			o = getOWLIndividual(s);
		}
		return o;
	}
}

