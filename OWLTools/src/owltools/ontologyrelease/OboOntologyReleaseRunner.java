package owltools.ontologyrelease;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.List;
import java.util.Properties;
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
 * This class is a command line utility which builds an ontology release. The
 * command line argument --h or --help provides usage documentation of this
 * utility. This tool called through bin/ontology-release-runner.
 * 
 * @author Shahid Manzoor
 * 
 */
public class OboOntologyReleaseRunner {

	protected final static Logger logger = Logger
			.getLogger(OboOntologyReleaseRunner.class);

	private static SimpleDateFormat dtFormat = new SimpleDateFormat(
			"yyyy-MM-dd");

	
	/**
	 * Build OWL ontology from the path supplied in the arguments. The ontology path could 
	 * point either to obo ontology or owl ontology file.  
	 * @param path
	 * @return
	 * @throws IOException
	 * @throws OWLOntologyCreationException
	 */
	private static OWLOntology getOntology(String path) throws IOException,
			OWLOntologyCreationException {

		OWLOntology ontology = null;

		if (path.endsWith(".obo")) {
			OBOFormatParser p = new OBOFormatParser();
			OBODoc obodoc = p.parse(path);

			Obo2Owl bridge = new Obo2Owl();
			ontology = bridge.convert(obodoc);
		} else {
			OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
			ontology = manager.loadOntologyFromOntologyDocument(new File(path));

		}

		return ontology;

	}

	private static void makeDir(File path) {
		if (!path.exists())
			path.mkdir();
	}

	/**
	 * Build Ontology version id for a particular release.
	 * @param base
	 * @return
	 * @throws IOException
	 */
	private static String buildVersionInfo(File base) throws IOException {

		File versionInfo = new File(base, "VERSION-INFO");

		Properties prop = new Properties();

		String version = dtFormat.format(Calendar.getInstance().getTime());

		prop.setProperty("version", version);

		FileOutputStream propFile = new FileOutputStream(versionInfo);

		prop.store(propFile,
				"Auto Generate Version Number. Please do not edit it");

		return version;
	}

	private static void cleanBase(File base) {
		/*for (File f : base.listFiles()) {
			if (f.getName().endsWith(".obo"))
				f.delete();
			else if (f.getName().endsWith(".owl"))
				f.delete();
			else if (f.isDirectory() && f.getName().equals("subsets"))
				f.delete();
			else if (f.isDirectory() && f.getName().equals("extensions"))
				f.delete();
		}*/
	}

	public static void main(String[] args) throws IOException,
			OWLOntologyCreationException, OWLOntologyStorageException,
			OBOFormatDanglingReferenceException {

		String path = null;
		OWLOntologyFormat format = new RDFXMLOntologyFormat();
		// String outPath = ".";
		String reasoner = "pellet";
		boolean asserted = false;
		boolean simple = false;
		String baseDirectory = ".";

		int i = 0;
		Vector<String> paths = new Vector<String>();
		while (i < args.length) {
			String opt = args[i];
			i++;

			if (opt.trim().length() == 0)
				continue;

			System.out.println("processing arg: " + opt);
			if (opt.equals("--h") || opt.equals("--help")) {
				usage();
				System.exit(0);
			}
			
			 else if (opt.equals("-outdir")) { baseDirectory = args[i]; i++; }
			 
			/*
			 * else if (opt.equals("-owlversion")) { version = args[i]; i++; }
			 */
			else if (opt.equals("-reasoner")) {
				reasoner = args[i];
				i++;
			}
			/*
			 * else if (opt.equals("-oboincludes")) { oboIncludes = args[i];
			 * i++; }
			 */
			else if (opt.equals("--asserted")) {
				asserted = true;
			} else if (opt.equals("--simple")) {
				simple = true;
			}

			else {

				String tokens[] = opt.split(" ");
				for (String token : tokens)
					paths.add(token);
			}
		}

		File base = new File(baseDirectory);

		System.out.println("Base directory path " + base.getAbsolutePath());

		if (!base.exists())
			throw new FileNotFoundException("The base directory at "
					+ baseDirectory + " does not exist");

		if (!base.canRead())
			throw new IOException("Cann't read the base directory at "
					+ baseDirectory);

		if (!base.canWrite())
			throw new IOException("Cann't write in the base directory "
					+ baseDirectory);

		File releases = new File(base, "releases");
		makeDir(releases);

		cleanBase(base);

		File todayRelease = new File(releases, dtFormat.format(Calendar
				.getInstance().getTime()));
		todayRelease = todayRelease.getCanonicalFile();
		makeDir(todayRelease);

		String version = buildVersionInfo(base);

		File subsets = new File(base, "subsets");
		makeDir(subsets);

		File extensions = new File(base, "extensions");
		makeDir(extensions);

		if (paths.size() > 0)
			path = paths.get(0);

		System.out.println("Processing Ontologies: " + paths);

		OWLOntology ontology = getOntology(path);
		OWLGraphWrapper graphWrapper = new OWLGraphWrapper(ontology);
		// if(oboIncludes != null){
		// String paths[] = oboIncludes.split(" ");
		// for(String p: paths){
		for (int k = 1; k < paths.size(); k++) {
			// if(p.length()>0){
			OWLOntology ont = getOntology(paths.get(k));
			graphWrapper.mergeOntology(ont);
			// }
		}
		// }

		OWLOntologyManager manager = graphWrapper.getManager();

		// if (iri.endsWith(".obo")) {
		// showMemory();

		if (version != null) {
			addVersion(ontology, version, manager);
		}

		String ontologyId = Owl2Obo.getOntologyId(ontology);

		if (asserted) {
			System.out.println("Creating Asserted Ontology");

			String outputURI = new File(base, ontologyId + "-asserted.owl")
					.getAbsolutePath();

			System.out.println("saving to " + outputURI);
			FileOutputStream os = new FileOutputStream(new File(outputURI));
			manager.saveOntology(ontology, format, os);
			os.close();

			Owl2Obo owl2obo = new Owl2Obo();
			OBODoc doc = owl2obo.convert(ontology);

			outputURI = new File(base, ontologyId + "-asserted.obo")
					.getAbsolutePath();
			System.out.println("saving to " + outputURI);

			OBOFormatWriter writer = new OBOFormatWriter();

			BufferedWriter bwriter = new BufferedWriter(new FileWriter(
					new File(outputURI)));

			writer.write(doc, bwriter);

			bwriter.close();

			System.out.println("Asserted Ontolog Creationg Completed");
		}

		if (simple) {

			System.out.print("Creating simple ontology");

			OWLOntologyManager simpleManager = OWLManager
					.createOWLOntologyManager();

			OWLOntology simpleOnt = getOntology(path);

			if (version != null) {
				addVersion(simpleOnt, version, simpleManager);
			}

			System.out.println("Creating Inferences");
			if (reasoner != null) {
				// buildInferredOntology(simpleOnt, manager, reasoner);
				buildInferences(new OWLGraphWrapper(simpleOnt));

			}
			System.out.println("Inferences creation completed");

			String outputURI = new File(base, ontologyId + "-simple.owl")
					.getAbsolutePath();

			System.out.println("saving to " + ontologyId + "," + outputURI
					+ " via " + format);
			FileOutputStream os = new FileOutputStream(new File(outputURI));
			simpleManager.saveOntology(simpleOnt, format, os);
			os.close();

			Owl2Obo owl2obo = new Owl2Obo();
			OBODoc doc = owl2obo.convert(simpleOnt);

			outputURI = new File(base, ontologyId + "-simple.obo")
					.getAbsolutePath();
			System.out.println("saving to " + outputURI);

			OBOFormatWriter writer = new OBOFormatWriter();

			BufferedWriter bwriter = new BufferedWriter(new FileWriter(
					new File(outputURI)));

			writer.write(doc, bwriter);

			bwriter.close();

			System.out.println("Creating simple ontology completed");

		}

		System.out.println("Creating basic ontology");

		System.out.println("Creating inferences");
		if (reasoner != null)
			buildInferences(graphWrapper);
		// ontology= buildInferredOntology(ontology, manager, reasoner);

		System.out.println("Inferences creation completed");

		String outputURI = new File(base, ontologyId + ".owl")
				.getAbsolutePath();

		// IRI outputStream = IRI.create(outputURI);
		// format = new OWLXMLOntologyFormat();
		// OWLXMLOntologyFormat owlFormat = new OWLXMLOntologyFormat();
		System.out.println("saving to " + ontologyId + "," + outputURI
				+ " via " + format);
		FileOutputStream os = new FileOutputStream(new File(outputURI));
		manager.saveOntology(ontology, format, os);
		os.close();

		Owl2Obo owl2obo = new Owl2Obo();
		OBODoc doc = owl2obo.convert(ontology);

		outputURI = new File(base, ontologyId + ".obo").getAbsolutePath();
		System.out.println("saving to " + outputURI);

		OBOFormatWriter writer = new OBOFormatWriter();

		BufferedWriter bwriter = new BufferedWriter(new FileWriter(new File(
				outputURI)));

		writer.write(doc, bwriter);

		bwriter.close();

		System.out.println("Copying files to release "
				+ todayRelease.getAbsolutePath());

		for (File f : base.listFiles()) {
			if (f.getName().endsWith(".obo") || f.getName().endsWith(".owl")
					|| f.getName().equals("VERSION-INFO")
					|| (f.isDirectory() && f.getName().equals("subsets"))
					|| (f.isDirectory() && f.getName().equals("extensions")))

				 copy(f.getCanonicalFile(), todayRelease);
		}

	}

	private static List<OWLAxiom> buildInferences(OWLGraphWrapper graph) {
		InferenceBuilder infBuilder = new InferenceBuilder(graph);

		return infBuilder.buildInferences();
	}

	private static void usage() {
		System.out.println("This utility builds an ontology release");
		System.out.println("\n");
		System.out.println("bin/ontology-release-runner [OPTIONAL OPTIONS] ONTOLOGIES-FILES");
		System.out
				.println("Multiple obo or owl files are separated by a space character in the place of the ONTOLOGIES-FILES arguments.");
		System.out.println("\n");
		System.out.println("OPTIONS:");
		System.out
				.println("\t\t (-outdir ~/work/myontology) The path where the release will be produced.");
		System.out
				.println("\t\t (-reasoner pellet) This option provides name of reasoner to be used to build inference computation.");
		System.out
				.println("\t\t (--asserted) This uniary option produces ontology without inferred assertions");
		System.out
				.println("\t\t (--simple) This uniary option produces ontology without included/supported ontologies");
	}

	private static void addVersion(OWLOntology ontology, String version,
			OWLOntologyManager manager) {
		OWLDataFactory fac = manager.getOWLDataFactory();

		OWLAnnotationProperty ap = fac.getOWLAnnotationProperty(Obo2Owl
				.trTagToIRI(OboFormatTag.TAG_REMARK.getTag()));
		OWLAnnotation ann = fac
				.getOWLAnnotation(ap, fac.getOWLLiteral(version));

		OWLAxiom ax = fac.getOWLAnnotationAssertionAxiom(ontology
				.getOntologyID().getOntologyIRI(), ann);

		manager.applyChange(new AddAxiom(ontology, ax));

	}

	
	/**
	 * Copies file/directory to destination from source.
	 * @param fromFile
	 * @param toFile
	 * @throws IOException
	 */
	public static void copy(File fromFile, File toFile) throws IOException {

		if (toFile.isDirectory())
			toFile = new File(toFile, fromFile.getName());
		
		if(fromFile.isDirectory()){
			makeDir(toFile);
			for(File f: fromFile.listFiles()){
				if(f.getName().equals(".") || f.getName().equals(".."))
					continue;
				
				copy(f, toFile);
			}
			
			return;
		}
		
		FileInputStream from = null;
		FileOutputStream to = null;
		try {
			from = new FileInputStream(fromFile);
			to = new FileOutputStream(toFile);
			byte[] buffer = new byte[4096];
			int bytesRead;

			while ((bytesRead = from.read(buffer)) != -1)
				to.write(buffer, 0, bytesRead); // write
		} finally {
			if (from != null)
				try {
					from.close();
				} catch (IOException e) {
					;
				}
			if (to != null)
				try {
					to.close();
				} catch (IOException e) {
					;
				}
		}
	}

}
