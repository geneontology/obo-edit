package owltools.cli;

import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.obolibrary.oboformat.model.FrameMergeException;
import org.semanticweb.owlapi.model.OWLAxiom;
import org.semanticweb.owlapi.model.OWLClass;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyStorageException;

import owltools.gfx.OWLGraphLayoutRenderer;
import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.io.GraphClosureWriter;
import owltools.io.ParserWrapper;
import owltools.mireot.Mireot;
import owltools.sim.SimEngine;
import owltools.sim.JaccardSimilarity;
import owltools.sim.SimEngine.SimilarityAlgorithmException;
import owltools.sim.Similarity;

public class CommandLineInterface {
	
	private static class Opts {
		int i = 0;
		String[] args;
		
		public Opts(String[] args) {
			super();
			this.i = 0;
			this.args = args;
		}
		
		public boolean hasArgs() {
			return i < args.length;
		}
		public boolean hasOpts() {
			return hasArgs() && args[i].startsWith("-");
		}
		
		public String nextOpt() {
			String opt = args[i];
			i++;
			return opt;
		}
	}
	
	public static void main(String[] args) throws OWLOntologyCreationException, IOException, FrameMergeException, SimilarityAlgorithmException, OWLOntologyStorageException {

		List<String> paths = new ArrayList<String>();
		//Integer i=0;
                // REDUNDANT: see new method
		String reasonerClassName = "uk.ac.manchester.cs.factplusplus.owlapiv3.Reasoner";
		String reasonerName = null;
		boolean createNamedRestrictions = false;
		boolean createDefaultInstances = false;
		boolean merge = false;
		
		String similarityAlgorithmName = "JaccardSimilarity";


		OWLGraphWrapper g = null;
		ParserWrapper pw = new ParserWrapper();
		
		Opts opts = new Opts(args);
		
		while (opts.hasArgs()) {
			String opt = opts.nextOpt();
			System.out.println("processing arg: "+opt);
			if (opt.equals("--pellet")) {
				reasonerClassName = "com.clarkparsia.pellet.owlapiv3.Reasoner";
				reasonerName = "pellet";
			}
			else if (opt.equals("--hermit")) {
				reasonerClassName = "org.semanticweb.HermiT.Reasoner";
				reasonerName = "hermit";
			}
			else if (opt.equals("--no-reasoner")) {
				reasonerClassName = "";
				reasonerName = "";
			}
			else if (opt.equals("-r") || opt.equals("--namerestr")) {
				createNamedRestrictions = true;
			}
			else if (opt.equals("-i") || opt.equals("--inst")) {
				createDefaultInstances = true;
			}
			else if (opt.equals("--list-classes")) {
				Set<OWLClass> clss = g.getOntology().getClassesInSignature();
				for (OWLClass c : clss) {
					System.out.println(c);
				}
			}
			else if (opt.equals("-merge")) {
				merge = true;
			}
			else if (opt.equals("-m") || opt.equals("--mireot")) {
				mergeOntologies(g,opts);
			}
			else if (opt.equals("--save-closure")) {
				GraphClosureWriter gcw = new GraphClosureWriter(opts.nextOpt());
				gcw.saveClosure(g);				
			}
			else if (opt.equals("-a") || opt.equals("--ancestors")) {
				//System.out.println("i= "+i);
				OWLObject obj = resolveEntity(g, opts);
				System.out.println(obj+ " "+obj.getClass());
				Set<OWLGraphEdge> edges = g.getOutgoingEdgesClosureReflexive(obj);
				showEdges(edges);
			}
			else if (opt.equals("--descendant-edges")) {
				//System.out.println("i= "+i);
				OWLObject obj = resolveEntity(g, opts);
				System.out.println(obj+ " "+obj.getClass());
				Set<OWLGraphEdge> edges = g.getIncomingEdgesClosure(obj);
				showEdges(edges);
			}
			else if (opt.equals("--descendants")) {
				//System.out.println("i= "+i);
				OWLObject obj = resolveEntity(g, opts);
				System.out.println(obj+ " "+obj.getClass());
				Set<OWLObject> ds = g.getDescendants(obj);
				for (OWLObject d : ds)
					System.out.println(d);
			}
			else if (opt.equals("-d") || opt.equals("--draw")) {
				//System.out.println("i= "+i);
				OWLObject obj = resolveEntity(g, opts);
				System.out.println(obj);
				OWLGraphLayoutRenderer r = new OWLGraphLayoutRenderer(g);
				
				r.addObject(obj);
				r.renderImage("png", new FileOutputStream("tmp.png"));
				Set<OWLGraphEdge> edges = g.getOutgoingEdgesClosureReflexive(obj);
				showEdges(edges);
			}
			else if (opt.equals("--all-class-ic")) {
				SimEngine se = new SimEngine(g);
				Similarity sa = se.getSimilarityAlgorithm(similarityAlgorithmName);
				for (OWLObject obj : g.getAllOWLObjects()) {
										System.out.print(obj);
					if (se.hasInformationContent(obj)) {
						System.out.println(" IC:"+se.getInformationContent(obj));
					}
					else {
						System.out.println("n/a");
					}
					
				}
			}
			else if (opt.equals("--sim-method")) {
				similarityAlgorithmName = opts.nextOpt();
			}
			else if (opt.equals("--sim-all")) {
				SimEngine se = new SimEngine(g);
				Similarity metric = se.getSimilarityAlgorithm(similarityAlgorithmName);
				//SimilarityAlgorithm metric = se.new JaccardSimilarity();
				se.calculateSimilarityAllByAll(metric);
				System.out.println(metric.getClass().getName());
			}
			else if (opt.equals("--sim")) {
				String a = opts.nextOpt();
				if (a.equals("-m")) {
					similarityAlgorithmName = opts.nextOpt();
					a = opts.nextOpt();
				}
				String b = opts.nextOpt();
				SimEngine se = new SimEngine(g);
				Similarity metric = se.getSimilarityAlgorithm(similarityAlgorithmName);
				OWLObject oa = resolveEntity(g,a);
				OWLObject ob = resolveEntity(g,b);
				System.out.println("comparing: "+oa+" vs "+ob);
				Similarity r = se.calculateSimilarity(metric, oa, ob);
				//System.out.println(metric+" = "+r);
				metric.print();
			}
			else if (opt.equals("-o") || opt.equals("--output")) {
				pw.saveOWL(g.getSourceOntology(), opts.nextOpt());
			}
			else {
				g =	pw.parseToOWLGraph(opt,true);
				System.out.println("parsed "+g);
				//paths.add(opt);
			}
		}
		
		/*
		
		OWLGraphWrapper g;
		if (paths.size() == 0) {
			throw new Error("must specify at least one file");
		}
		
		if (paths.size() > 1) {
			if (merge) {
				// note: currently we can only merge obo files
				pw.parseOBOFiles(paths);
			}
			else {
				throw new Error("cannot load multiple files unless --merge is set");
			}
		}
		else {
			g =	pw.parseToOWLGraph(paths.get(0));
		}
		*/
		
	}

	private static void mergeOntologies(OWLGraphWrapper g, Opts opts) throws OWLOntologyCreationException, IOException {
		Mireot m = new Mireot(g);
		ParserWrapper pw = new ParserWrapper();
		if (opts.hasOpts()) {
			String opt = opts.nextOpt();
			if (opt.equals("-r") || opt.equals("--ref-ont")) {
				String f = opts.nextOpt();
				m.addReferencedOntology(pw.parseOWL(f));
			}
			else if (opt.equals("-s") || opt.equals("--src-ont")) {
				m.setOntology(pw.parseOWL(opts.nextOpt()));
			}
			else if (opt.equals("-i") || opt.equals("--use-imports")) {
				System.out.println("using everything in imports closure");
				m.useImportsClosureAsReferencedOntologies();
			}
			else {
				// TODO
			}
		}
		for (OWLAxiom ax : m.getClosureAxioms()) {
			System.out.println("M_AX:"+ax);
		}

		m.mergeOntologies();
	}

	private static void showEdges(Set<OWLGraphEdge> edges) {
		for (OWLGraphEdge e : edges) {
			System.out.println(e);
		}
	}

	// todo - move to util
	public static OWLObject resolveEntity(OWLGraphWrapper g, Opts opts) {
		OWLObject obj = null;
		String id = opts.nextOpt(); // in future we will allow resolution by name etc
		return resolveEntity(g,id);
	}
	
	public static OWLObject resolveEntity(OWLGraphWrapper g, String id) {
		OWLObject obj = null;
		obj = g.getOWLObjectByLabel(id);
		if (obj != null)
			return obj;
		obj = g.getOWLObject(id);
		if (obj != null)
			return obj;		
		obj = g.getOWLObjectByIdentifier(id);
		return obj;
	}


	private static String[] runMireot(String[] args) {
		int i = 0;
	
		while (i < args.length) {
			
		}
		return null;
	}

}
