package owltools.ontologyrelease;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.util.List;
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
import org.semanticweb.owlapi.model.OWLDataFactory;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyFormat;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;
import owltools.InferenceBuilder;
import owltools.graph.OWLGraphWrapper;

/**
 * This class is a command line utility which builds an ontology release. The command line argument --h or --help provides
 * usage documentation of this utility. This tool called through ant script. See the target 'make-obo-release' in the build.xml project.
 * The command line arguments of this utility are configured in the conf/obo-release.prperties file. 
 * @author Shahid Manzoor
 *
 */
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
		
		int i=0;
		Vector<String> paths = new Vector<String>();
		while (i < args.length) {
			String opt = args[i];
			i++;

			if(opt.trim().length()==0)
				continue;
			
			System.out.println("processing arg: "+opt);
			if (opt.equals("--h") || opt.equals("--help")) {
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
						buildInferences(new OWLGraphWrapper(simpleOnt));
						
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
					buildInferences(graphWrapper);
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
				
				
	}


	private static List<OWLAxiom>  buildInferences(OWLGraphWrapper graph){
		InferenceBuilder infBuilder = new InferenceBuilder(graph);
		
		return infBuilder.buildInferences();
	}	
	
	
	private static void usage() {
		System.out.println("This utility builds an ontology release");
		System.out.println("\n");
		System.out.println("[OPTIONS] -outdir DIRPATH ONTOLOGIES-FILES");
		System.out.println("Multiple obo or owl files are separated by a space character in the place of the ONTOLOGIES-FILES arguments.");
		System.out.println("\n");
		System.out.println("OPTIONS:");
		System.out.println("\t\t (-owlversion 20110310) This option provides version id of the ontology.");
		System.out.println("\t\t (-reasoner pellet) This option provides name of reasoner to be used to build inference computation.");
		System.out.println("\t\t (--asserted) This uniary option produces ontology without inferred assertions");
		System.out.println("\t\t (--simple) This uniary option produces ontology without included/supported ontologies");
	}

	private static void addVersion(OWLOntology ontology, String version, OWLOntologyManager manager){
		OWLDataFactory fac = manager.getOWLDataFactory();
		
		OWLAnnotationProperty ap = fac.getOWLAnnotationProperty( Obo2Owl.trTagToIRI(OboFormatTag.TAG_REMARK.getTag()));
		OWLAnnotation ann = fac.getOWLAnnotation(ap, fac.getOWLLiteral(version));
		
		OWLAxiom ax = fac.getOWLAnnotationAssertionAxiom(ontology.getOntologyID().getOntologyIRI(), ann);
		
		manager.applyChange(new AddAxiom(ontology, ax));

		
	}
	
}
