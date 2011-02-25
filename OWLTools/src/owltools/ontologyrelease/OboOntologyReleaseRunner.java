package owltools.ontologyrelease;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import org.apache.log4j.Logger;
import org.obolibrary.obo2owl.Obo2Owl;
import org.obolibrary.obo2owl.Owl2Obo;
import org.obolibrary.oboformat.model.OBODoc;
import org.obolibrary.oboformat.parser.OBOFormatDanglingReferenceException;
import org.obolibrary.oboformat.parser.OBOFormatParser;
import org.obolibrary.oboformat.parser.OBOFormatConstants.OboFormatTag;
import org.obolibrary.oboformat.writer.OBOFormatWriter;
import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.io.RDFXMLOntologyFormat;
import org.semanticweb.owlapi.model.AddAxiom;
import org.semanticweb.owlapi.model.OWLAnnotation;
import org.semanticweb.owlapi.model.OWLAnnotationProperty;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLObjectIntersectionOf;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyFormat;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import org.semanticweb.owlapi.reasoner.Node;
import org.semanticweb.owlapi.reasoner.NodeSet;
import org.semanticweb.owlapi.reasoner.OWLReasoner;
import org.semanticweb.owlapi.reasoner.OWLReasonerFactory;
import org.semanticweb.owlapi.util.InferredAxiomGenerator;
import org.semanticweb.owlapi.util.InferredDisjointClassesAxiomGenerator;
import org.semanticweb.owlapi.util.InferredEquivalentClassAxiomGenerator;
import org.semanticweb.owlapi.util.InferredOntologyGenerator;
import org.semanticweb.owlapi.util.InferredSubClassAxiomGenerator;

import owltools.graph.OWLGraphWrapper;

import uk.ac.manchester.cs.factplusplus.owlapiv3.FaCTPlusPlusReasonerFactory;

import com.clarkparsia.pellet.owlapiv3.PelletReasonerFactory;

public class OboOntologyReleaseRunner {

	
	protected final static Logger logger = Logger.getLogger(OboOntologyReleaseRunner.class);

	
	private static OWLOntology getOntology(String path) throws IOException, OWLOntologyCreationException{
	
		OWLOntology ontology = null;
		
		if(path.endsWith(".obo")){
			OBOFormatParser p = new OBOFormatParser();
			OBODoc obodoc = p.parse(path);
			
			Obo2Owl bridge = new Obo2Owl();
			ontology = bridge.convert(obodoc);
		}else{
			OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
			ontology = manager.loadOntologyFromOntologyDocument(new File(path));
			
		}
		
		return ontology;
		
		
	}
	
	public static void main(String[] args) throws IOException, OWLOntologyCreationException, OWLOntologyStorageException,
		OBOFormatDanglingReferenceException{

		String path = null;
		OWLOntologyFormat format = new RDFXMLOntologyFormat();
		String outPath = ".";
		String reasoner="pellet";
		String version = null;
		boolean asserted = false;
		boolean simple = false;
		String oboIncludes = "";
		
		int i=0;
		Vector<String> paths = new Vector<String>();
		while (i < args.length) {
			String opt = args[i];
			i++;

			if(opt.trim().length()==0)
				continue;
			
			System.out.println("processing arg: "+opt);
			if (opt.equals("-h") || opt.equals("--help")) {
				usage();
				System.exit(0);
			}
			else if (opt.equals("-outdir")) {
				outPath = args[i];
				i++;
			}
			else if (opt.equals("-owlversion")) {
				version = args[i];
				i++;
			}
			else if (opt.equals("-reasoner")) {
				reasoner = args[i];
				i++;
			}
			/*else if (opt.equals("-oboincludes")) {
				oboIncludes = args[i];
				i++;
			}*/
			else if (opt.equals("--asserted")) {
				asserted= true;
			}
			else if (opt.equals("--simple")) {
				simple = true;
			}
			
			else {
				

				String tokens[]= opt.split(" ");
				for(String token: tokens )
					paths.add(token);
			}
		}

		if(paths.size()>0)
			path = paths.get(0);
		
	//	for (String iri : ) {
			/*String iri = path;
			
			OWLOntologyManager manager = null;
			OWLOntology ontology = null;
			
			if(iri.endsWith(".obo")){
				OBOFormatParser p = new OBOFormatParser();
				OBODoc obodoc = p.parse(iri);
				
				Obo2Owl bridge = new Obo2Owl();
				manager = bridge.getManager();
				ontology = bridge.convert(obodoc);
			}else{
				manager = OWLManager.createOWLOntologyManager();
				ontology = manager.loadOntologyFromOntologyDocument(new File(iri));
				
			}*/
		
			System.out.println("Processing Ontologies: " + paths);
		
			OWLOntology ontology = getOntology(path);
			OWLGraphWrapper graphWrapper = new OWLGraphWrapper(ontology);
		//	if(oboIncludes != null){
				//String paths[] = oboIncludes.split(" ");
			//	for(String p: paths){
				for(int k=1;k<paths.size();k++){
				//	if(p.length()>0){
						OWLOntology ont = getOntology(paths.get(k));
						graphWrapper.mergeOntology(ont);
				//	}
				}
			//}
			
			OWLOntologyManager manager = graphWrapper.getManager();
			
		//	if (iri.endsWith(".obo")) {
				//showMemory();
				
				if(version != null){
					addVersion(ontology, version, manager);
				}
				
				String ontologyId = Owl2Obo.getOntologyId(ontology);

				if(asserted){
					System.out.println("Creating Asserted Ontology");
					
					String	outputURI = new File(outPath,   ontologyId+ "-asserted.owl").getAbsolutePath();

					System.out.println("saving to "+ outputURI);
					FileOutputStream os = new FileOutputStream(new File(outputURI));
					manager.saveOntology(ontology, format, os);
					os.close();
					
					Owl2Obo owl2obo = new Owl2Obo();
					OBODoc doc = owl2obo.convert(ontology);

					outputURI = new File(outPath,   ontologyId+ "-asserted.obo").getAbsolutePath();
					System.out.println("saving to "+ outputURI);
					
					
					OBOFormatWriter writer = new OBOFormatWriter();
					
					BufferedWriter bwriter = new BufferedWriter(new FileWriter(new File(outputURI)));
					
					writer.write(doc, bwriter);
					
					bwriter.close();
				
					System.out.println("Asserted Ontolog Creationg Completed");
				}
				
				if(simple){
			
					System.out.print("Creating simple ontology");
					
					OWLOntologyManager simpleManager = OWLManager.createOWLOntologyManager();
					
					OWLOntology simpleOnt = getOntology(path);

					if(version != null){
						addVersion(simpleOnt, version, simpleManager);
					}
					
					System.out.println("Creating Inferences");
					if(reasoner != null){
						//buildInferredOntology(simpleOnt, manager, reasoner);
						buildInferences(simpleOnt, manager, reasoner);
						
					}
					System.out.println("Inferences creation completed");
				
					
					String	outputURI = new File(outPath,   ontologyId+ "-simple.owl").getAbsolutePath();

					System.out.println("saving to "+ ontologyId + "," +outputURI+" via "+format);
					FileOutputStream os = new FileOutputStream(new File(outputURI));
					simpleManager.saveOntology(simpleOnt, format, os);
					os.close();
					
					Owl2Obo owl2obo = new Owl2Obo();
					OBODoc doc = owl2obo.convert(simpleOnt);

					outputURI = new File(outPath,   ontologyId+ "-simple.obo").getAbsolutePath();
					System.out.println("saving to "+ outputURI);
					
					
					OBOFormatWriter writer = new OBOFormatWriter();
					
					BufferedWriter bwriter = new BufferedWriter(new FileWriter(new File(outputURI)));
					
					writer.write(doc, bwriter);
					
					bwriter.close();
				
					System.out.println("Creating simple ontology completed");
					
				}
				
				System.out.println("Creating basic ontology");
				
				
				System.out.println("Creating inferences");
				if(reasoner != null)
					buildInferences(ontology, manager, reasoner);
				//	ontology= buildInferredOntology(ontology, manager, reasoner);
			
				System.out.println("Inferences creation completed");
				
				String	outputURI = new File(outPath,   ontologyId+ ".owl").getAbsolutePath();

				//IRI outputStream = IRI.create(outputURI);
				//format = new OWLXMLOntologyFormat();
				//OWLXMLOntologyFormat owlFormat = new OWLXMLOntologyFormat();
				System.out.println("saving to "+ ontologyId + "," +outputURI+" via "+format);
				FileOutputStream os = new FileOutputStream(new File(outputURI));
				manager.saveOntology(ontology, format, os);
				os.close();
				
				Owl2Obo owl2obo = new Owl2Obo();
				OBODoc doc = owl2obo.convert(ontology);

				outputURI = new File(outPath,   ontologyId+ ".obo").getAbsolutePath();
				System.out.println("saving to "+ outputURI);
				
				
				OBOFormatWriter writer = new OBOFormatWriter();
				
				BufferedWriter bwriter = new BufferedWriter(new FileWriter(new File(outputURI)));
				
				writer.write(doc, bwriter);
				
				bwriter.close();
				
				
		//	}
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			/*
			if (iri.endsWith(".obo")) {
				//showMemory();
				OBOFormatParser p = new OBOFormatParser();
				OBODoc obodoc = p.parse(iri);
				
				Obo2Owl bridge = new Obo2Owl();
				OWLOntologyManager manager = bridge.getManager();
				OWLOntology ontology = bridge.convert(obodoc);
				
				if(version != null){
					addVersion(ontology, version, manager);
				}
				
				String ontologyId = Owl2Obo.getOntologyId(ontology);

				if(asserted){
					String	outputURI = new File(outPath,   ontologyId+ "-asserted.owl").getAbsolutePath();

					//IRI outputStream = IRI.create(outputURI);
					//format = new OWLXMLOntologyFormat();
					//OWLXMLOntologyFormat owlFormat = new OWLXMLOntologyFormat();
					System.out.println("saving to "+ outputURI);
					FileOutputStream os = new FileOutputStream(new File(outputURI));
					manager.saveOntology(ontology, format, os);
					os.close();
					
					Owl2Obo owl2obo = new Owl2Obo();
					OBODoc doc = owl2obo.convert(ontology);

					outputURI = new File(outPath,   ontologyId+ "-asserted.obo").getAbsolutePath();
					System.out.println("saving to "+ outputURI);
					
					
					OBOFormatWriter writer = new OBOFormatWriter();
					
					BufferedWriter bwriter = new BufferedWriter(new FileWriter(new File(outputURI)));
					
					writer.write(doc, bwriter);
					
					bwriter.close();
					
				}
				
				
				ontology= buildInferredOntology(ontology, manager, reasoner);
				
				String	outputURI = new File(outPath,   ontologyId+ ".owl").getAbsolutePath();

				//IRI outputStream = IRI.create(outputURI);
				//format = new OWLXMLOntologyFormat();
				//OWLXMLOntologyFormat owlFormat = new OWLXMLOntologyFormat();
				System.out.println("saving to "+ ontologyId + "," +outputURI+" via "+format);
				FileOutputStream os = new FileOutputStream(new File(outputURI));
				manager.saveOntology(ontology, format, os);
				os.close();
				
				Owl2Obo owl2obo = new Owl2Obo();
				OBODoc doc = owl2obo.convert(ontology);

				outputURI = new File(outPath,   ontologyId+ ".obo").getAbsolutePath();
				System.out.println("saving to "+ outputURI);
				
				
				OBOFormatWriter writer = new OBOFormatWriter();
				
				BufferedWriter bwriter = new BufferedWriter(new FileWriter(new File(outputURI)));
				
				writer.write(doc, bwriter);
				
				bwriter.close();
				
				
			}
			else {
				OWLOntologyManager manager = OWLManager.createOWLOntologyManager(); // persist?
				OWLOntology ontology = manager.loadOntologyFromOntologyDocument(IRI.create(iri));
				
				if(version != null){
					addVersion(ontology, version, manager);
				}
			
				String ontologyId = Owl2Obo.getOntologyId(ontology);
				
				if(asserted){
					String	outputURI = new File(outPath,   ontologyId+ "-asserted.owl").getAbsolutePath();

					System.out.println("saving to "+ outputURI);
					FileOutputStream os = new FileOutputStream(new File(outputURI));
					manager.saveOntology(ontology, format, os);
					os.close();
					
					Owl2Obo owl2obo = new Owl2Obo();
					OBODoc doc = owl2obo.convert(ontology);

					outputURI = new File(outPath,   ontologyId+ "-asserted.obo").getAbsolutePath();
					System.out.println("saving to "+ outputURI);
					
					OBOFormatWriter writer = new OBOFormatWriter();
					
					BufferedWriter bwriter = new BufferedWriter(new FileWriter(new File(outputURI)));
					
					writer.write(doc, bwriter);
					
					bwriter.close();
					
				}
				
				ontology= buildInferredOntology(ontology, manager, reasoner);
				
				String	outputURI = new File(outPath,   ontologyId+ ".owl").getAbsolutePath();

				//IRI outputStream = IRI.create(outputURI);
				//format = new OWLXMLOntologyFormat();
				//OWLXMLOntologyFormat owlFormat = new OWLXMLOntologyFormat();
				System.out.println("saving to "+ ontologyId + "," +outputURI+" via "+format);
				FileOutputStream os = new FileOutputStream(new File(outputURI));
				manager.saveOntology(ontology, format, os);
				os.close();
				
				Owl2Obo owl2obo = new Owl2Obo();
				OBODoc doc = owl2obo.convert(ontology);

				outputURI = new File(outPath,   ontologyId+ ".obo").getAbsolutePath();
				System.out.println("saving to "+ outputURI);
				
				
				OBOFormatWriter writer = new OBOFormatWriter();
				
				BufferedWriter bwriter = new BufferedWriter(new FileWriter(new File(outputURI)));
				
				writer.write(doc, bwriter);
				
				bwriter.close();
			}*/
	//	}

	}

	

	private static Set<OWLAxiom>  buildInferences(OWLOntology ontology, OWLOntologyManager manager,  String reasonerName){
	//	OWLOntology ontology = graph.getSourceOntology();
	//	PelletReasonerFactory factory = new PelletReasonerFactory();
	//	OWLReasoner reasoner = factory.createReasoner(ontology);

		OWLDataFactory factory = manager.getOWLDataFactory();
		OWLReasoner reasoner = createReasoner(ontology, reasonerName);
		
		
		Set<OWLClass> nrClasses = new HashSet<OWLClass>();

		Set<OWLAxiom> set = new HashSet<OWLAxiom>();
		for (OWLClass cls : ontology.getClassesInSignature()) {
			if (nrClasses.contains(cls))
				continue; // do not report these
			
			// REPORT INFERRED EQUIVALENCE BETWEEN NAMED CLASSES
			for (OWLClass ec : reasoner.getEquivalentClasses(cls)) {
				if (nrClasses.contains(ec))
					continue; // do not report these
				
				if(cls.toString().equals(ec.toString()))
					continue;
				
				if (cls.toString().compareTo(ec.toString()) > 0) // equivalence is symmetric: report each pair once
					set.add( factory.getOWLEquivalentClassesAxiom(cls, ec));
					//consitancyCheckBuffer.append("&nbsp;&nbsp;&nbsp;* INFERRED: equivalent "+getLabel(cls, graph)+" "+ getLabel(ec, graph) + "<br />" );
			}
			
			// REPORT INFERRED SUBCLASSES NOT ALREADY ASSERTED

			NodeSet<OWLClass> scs = reasoner.getSuperClasses(cls, true);
			for (Node<OWLClass> scSet : scs) {
				for (OWLClass sc : scSet) {
					if (sc.toString().endsWith("Thing")) {
						continue;
					}
					if (nrClasses.contains(sc))
						continue; // do not report subclasses of owl:Thing

					// we do not want to report inferred subclass links
					// if they are already asserted in the ontology
					boolean isAsserted = false;
					for (OWLClassExpression asc : cls.getSuperClasses(ontology)) {
						if (asc.equals(sc)) {
							// we don't want to report this
							isAsserted = true;								
						}
					}
					for (OWLClassExpression ec : cls.getEquivalentClasses(ontology)) {
						
						if (ec instanceof OWLObjectIntersectionOf) {
							OWLObjectIntersectionOf io = (OWLObjectIntersectionOf)ec;
							for (OWLClassExpression op : io.getOperands()) {
								if (op.equals(sc)) {
									isAsserted = true;
								}
							}
						}
					}
					if (!isAsserted) {
						set.add(factory.getOWLSubClassOfAxiom(cls, sc));
						//this.consitancyCheckBuffer.append("&nbsp;&nbsp;&nbsp;&nbsp;*INFERRED:  "+getLabel(cls,graph)+" subClassOf "+getLabel(sc, graph) + "<br />");
					}
				}
			}
		}
		
	//	OWLOntologyManager manager = graph.getManager();
		for(OWLAxiom ax: set){
			manager.applyChange(new AddAxiom(ontology, ax));
		}
		
		return set;
		
	}
	
	
	/*private static OWLOntology buildInferredOntology(OWLOntology ontology, OWLOntologyManager manager, String reasonerName) throws OWLOntologyCreationException{

		OWLReasoner reasoner = createReasoner(ontology, reasonerName);
		List<InferredAxiomGenerator<? extends OWLAxiom>> gens = new ArrayList<InferredAxiomGenerator<? extends OWLAxiom>>();
		gens.add(new InferredSubClassAxiomGenerator());
		gens.add(new InferredEquivalentClassAxiomGenerator());
		gens.add(new InferredDisjointClassesAxiomGenerator());

		// Put the inferred axiomns into a fresh empty ontology - note that there
		// is nothing stopping us stuffing them back into the original asserted ontology
		// if we wanted to do this.
		OWLOntology infOnt = ontology;
			//manager.createOntology(IRI.create(ontology.getOntologyID().getOntologyIRI().toString()));

		// Now get the inferred ontology generator to generate some inferred axioms
		// for us (into our fresh ontology).  We specify the reasoner that we want
		// to use and the inferred axiom generators that we want to use.
		InferredOntologyGenerator iog = new InferredOntologyGenerator(reasoner, gens);
		iog.fillOntology(manager, infOnt);

		return infOnt;
	
	}*/
	
	private static OWLReasoner createReasoner(OWLOntology ont, String reasonerName) {
			OWLReasonerFactory reasonerFactory = null;
			if (reasonerName == null || reasonerName.equals("factpp"))
				reasonerFactory = new FaCTPlusPlusReasonerFactory();
			else if (reasonerName.equals("pellet"))
				reasonerFactory = new PelletReasonerFactory();
			else if (reasonerName.equals("hermit")) {
				//return new org.semanticweb.HermiT.Reasoner.ReasonerFactory().createReasoner(ont);
				//reasonerFactory = new org.semanticweb.HermiT.Reasoner.ReasonerFactory();			
			}
			else
				logger.error("no such reasoner: "+reasonerName);
			
			OWLReasoner reasoner = reasonerFactory.createReasoner(ont);
			return reasoner;
	}
	
	
	
	private static void usage() {
		System.out.println("obolib-obo2owl [--to SYNTAX, --allowdangling] -o FILEPATH-URI OBO-FILE");
		System.out.println("obolib-obo2owl -b BUILDPATH-URI");
		System.out.println("\n");
		System.out.println("Converts obo files to OWL. If -b option is used, entire\n");
		System.out.println("obo repository is converted\n");
		System.out.println("\n");
		System.out.println("Example:\n");
		System.out.println(" obolib-obo2owl -o file://`pwd`/my.owl my.obo\n");
		System.out.println("Example:\n");
		System.out.println(" obolib-obo2owl -b file://`pwd`\n");
		System.out.println("Example:\n");
		System.out.println(" obolib-obo2owl -b file://`pwd` --download FBBT\n");

	}

	private static void addVersion(OWLOntology ontology, String version, OWLOntologyManager manager){
		OWLDataFactory fac = manager.getOWLDataFactory();
		
		OWLAnnotationProperty ap = fac.getOWLAnnotationProperty( Obo2Owl.trTagToIRI(OboFormatTag.TAG_REMARK.getTag()));
		OWLAnnotation ann = fac.getOWLAnnotation(ap, fac.getOWLLiteral(version));
		
		OWLAxiom ax = fac.getOWLAnnotationAssertionAxiom(ontology.getOntologyID().getOntologyIRI(), ann);
		
		manager.applyChange(new AddAxiom(ontology, ax));

		
	}
	
	
	/*public static void main(String args[]) throws Exception{
		Vector<String> cols = new Vector<String>();
	
		String version = null;
		boolean obo2owl = true;
		for(int i=0;i<args.length;i++){
			String arg = args[i];
			if("--version".equals(arg)){
				i++;
				version = args[i];
				continue;
			}
			
			if("--ow2obo".equals(arg)){
				obo2owl = false;
			}
			
			cols.add(arg);
		}
		
		args = new String[cols.size()];
		cols.toArray(args);
		

	//	OBORunner.main(args);
		
		System.out.println("Version: " + version);
		System.out.println("Obo 2 owl: " + obo2owl);
		
		
	}*/
}
