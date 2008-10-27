package owltools;

import org.semanticweb.owl.apibinding.OWLManager;
import org.semanticweb.owl.inference.OWLReasoner;
import org.semanticweb.owl.inference.OWLReasonerAdapter;
import org.semanticweb.owl.inference.OWLReasonerException;
import org.semanticweb.owl.model.*;
import org.semanticweb.owl.util.DLExpressivityChecker;
import org.semanticweb.owl.util.InferredAxiomGenerator;
import org.semanticweb.owl.util.InferredAxiomGeneratorException;
import org.semanticweb.owl.util.InferredOntologyGenerator;
import org.semanticweb.owl.util.InferredSubClassAxiomGenerator;
import org.semanticweb.owl.vocab.OWLRDFVocabulary;


import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
/*
 * Copyright (C) 2007, University of Manchester
 *
 * Modifications to the initial code base are copyright of their
 * respective authors, or their employers as appropriate.  Authorship
 * of the modifications may be determined from the ChangeLog placed at
 * the end of this file.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.

 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.

 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */


/**
 * Author: Matthew Horridge<br>
 * The University Of Manchester<br>
 * Bio-Health Informatics Group<br>
 * Date: 20-Jun-2007<br><br>
 *
 * An example which shows how to interact with a reasoner.  In this example
 * Pellet is used as the reasoner.  You must get hold of the pellet libraries
 * from pellet.owldl.com.
 */
public class OWLReasonerRunner {

	public static final String PHYSICAL_URI = "http://purl.org/obo/owl/CARO";

	public static void main(String[] args) {

		Collection<String> paths = new ArrayList<String>();
		int i=0;
		String reasonerClassName = "uk.ac.manchester.cs.factplusplus.owlapi.Reasoner";
		boolean createNamedRestrictions = false;

		while (i < args.length) {
			String opt = args[i];
			i++;
			if (opt.equals("--pellet")) {
				reasonerClassName = "org.mindswap.pellet.owlapi.Reasoner";
			}
			else if (opt.equals("-r") || opt.equals("--namerestr")) {
				createNamedRestrictions = true;
			}
			else {
				paths.add(opt);
			}
		}

		if (paths.size() == 0) {
			paths.add(PHYSICAL_URI);
		}

		try {
			for (String uri : paths) {
				// Create our ontology manager in the usual way.
				OWLOntologyManager manager = OWLManager.createOWLOntologyManager();

				// Load a copy of the pizza ontology.  We'll load the ontology from the web.
				OWLOntology ont = manager.loadOntologyFromPhysicalURI(URI.create(uri));
				System.out.println("Loaded " + ont.getURI());



				// We need to create an instance of OWLReasoner.  OWLReasoner provides the basic
				// query functionality that we need, for example the ability obtain the subclasses
				// of a class etc.  See the createReasoner method implementation for more details
				// on how to instantiate the reasoner
				OWLReasoner reasoner = createReasoner(manager,reasonerClassName);

				// We now need to load some ontologies into the reasoner.  This is typically the
				// imports closure of an ontology that we're interested in.  In this case, we want
				// the imports closure of the pizza ontology.  Note that no assumptions are made
				// about the dependency of one ontology on another ontology.  This means that if
				// we loaded just the pizza ontology (using a singleton set) then any imported ontologies
				// would not automatically be loaded.
				// Obtain and load the imports closure of the pizza ontology
				Set<OWLOntology> importsClosure = manager.getImportsClosure(ont);
				reasoner.loadOntologies(importsClosure);

				OWLDataFactory owlFactory = manager.getOWLDataFactory();


				if (createNamedRestrictions) {

					Collection<OWLEquivalentClassesAxiom> newAxioms = new ArrayList<OWLEquivalentClassesAxiom>();
					Set<OWLClass> owlClasses = ont.getReferencedClasses();
					Set<OWLObjectProperty> owlProperties = ont.getReferencedObjectProperties();
					for (OWLObjectProperty property : owlProperties) {
						URI pURI = property.getURI();
						if (!property.isTransitive(ont))
							continue;
						for (OWLClass cls : owlClasses) {
							OWLObjectSomeRestriction restr = 
								owlFactory.getOWLObjectSomeRestriction(property, cls);
							URI rURI = URI.create(pURI+"/"+cls.getURI().getFragment());
							OWLClass ec = 
								owlFactory.getOWLClass(rURI);
							//OWLLabelAnnotation label = 
							//	owlFactory.getOWLLabelAnnotation(pURI.getFragment(), cls.getURI().getFragment());
							OWLEquivalentClassesAxiom ecAxiom = owlFactory.getOWLEquivalentClassesAxiom(ec,restr);
							newAxioms.add(ecAxiom);

						}
					}
					for (OWLEquivalentClassesAxiom ecAxiom : newAxioms) {
						manager.addAxiom(ont, ecAxiom);							
					}


				}

				long initTime = System.nanoTime();
				reasoner.classify();
				long totalTime = System.nanoTime() - initTime;
				System.out.println("   Total reasoner time = "
						+ (totalTime / 1000000d) + " ms");


				// We can examine the expressivity of our ontology (some reasoners do not support
				// the full expressivity of OWL)
				DLExpressivityChecker checker = new DLExpressivityChecker(importsClosure);
				System.out.println("Expressivity: " + checker.getDescriptionLogicName());

				// We can determine if the pizza ontology is actually consistent.  (If an ontology is
				// inconsistent then owl:Thing is equivalent to owl:Nothing - i.e. there can't be any
				// models of the ontology)
				boolean consistent = reasoner.isConsistent(ont);
				System.out.println("Consistent: " + consistent);
				System.out.println("\n");

				// We can easily get a list of inconsistent classes.  (A class is inconsistent if it
				// can't possibly have any instances).  Note that the getInconsistentClasses method
				// is really just a convenience method for obtaining the classes that are equivalent
				// to owl:Nothing.
				Set<OWLClass> inconsistentClasses = reasoner.getInconsistentClasses();
				if (!inconsistentClasses.isEmpty()) {
					System.out.println("The following classes are inconsistent: ");
					for(OWLClass cls : inconsistentClasses) {
						if (cls.toString().equals("Nothing"))
							continue;
						System.out.println("    INCONSISTENT: " + getLabel(cls,ont));
					}
				}
				else {
					System.out.println("There are no inconsistent classes");
				}
				System.out.println("\n");

				for (OWLClass cls : ont.getReferencedClasses()) {
					for (OWLClass ec : reasoner.getEquivalentClasses(cls)) {
						if (cls.toString().compareTo(ec.toString()) > 0) // equivalence is symmetric: report each pair once
							System.out.println("  INFERRED: equivalent "+getLabel(cls,ont)+" "+getLabel(ec,ont));
					}
					//System.out.println("  "+cls);
					Set<Set<OWLClass>> scs = reasoner.getSuperClasses(cls);
					for (Set<OWLClass> scSet : scs) {
						for (OWLClass sc : scSet) {
							if (sc.toString().equals("Thing")) {
								continue;
							}
							//ont.get
							//System.out.println("    super: "+sc);
							Set<OWLDescription> ascs = cls.getSuperClasses(ont);

							boolean isAsserted = false;
							for (OWLDescription asc : ascs) {
								if (asc.equals(sc)) {
									//System.out.println("    ASC: "+asc);
									isAsserted = true;								
								}
							}
							for (OWLDescription ec : cls.getEquivalentClasses(ont)) {
								if (ec instanceof OWLObjectIntersectionOf) {
									OWLObjectIntersectionOf io = (OWLObjectIntersectionOf)ec;
									for (OWLDescription op : io.getOperands()) {
										if (op.equals(sc)) {
											isAsserted = true;
										}
									}
								}
							}
							if (!isAsserted) {
								System.out.println("  INFERRED:  "+getLabel(cls,ont)+" subClassOf "+getLabel(sc,ont));
							}
						}
					}
				}
				// To generate an inferred ontology we use implementations of inferred axiom generators
				// to generate the parts of the ontology we want (e.g. subclass axioms, equivalent classes
				// axioms, class assertion axiom etc. - see the org.semanticweb.owl.util package for more
				// implementations).  
				// Set up our list of inferred axiom generators
				List<InferredAxiomGenerator<? extends OWLAxiom>> gens = new ArrayList<InferredAxiomGenerator<? extends OWLAxiom>>();
				gens.add(new InferredSubClassAxiomGenerator());

				// Put the inferred axiomns into a fresh empty ontology - note that there
				// is nothing stopping us stuffing them back into the original asserted ontology
				// if we wanted to do this.
				OWLOntology infOnt = manager.createOntology(URI.create(ont.getURI() + "_inferred"));

				// Now get the inferred ontology generator to generate some inferred axioms
				// for us (into our fresh ontology).  We specify the reasoner that we want
				// to use and the inferred axiom generators that we want to use.
				InferredOntologyGenerator iog = new InferredOntologyGenerator(reasoner, gens);
				iog.fillOntology(manager, infOnt);

				// Save the inferred ontology. (Replace the URI with one that is appropriate for your setup)
				manager.saveOntology(infOnt, URI.create("file:///tmp/inferredont.owl"));


			}
		}


		catch(UnsupportedOperationException exception) {
			System.out.println("Unsupported reasoner operation.");
		}
		catch(OWLReasonerException ex) {
			System.out.println("Reasoner error: " + ex.getMessage());
		}
		catch (OWLOntologyCreationException e) {
			System.out.println("Could not load the pizza ontology: " + e.getMessage());
		} catch (InferredAxiomGeneratorException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (OWLOntologyChangeException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (UnknownOWLOntologyException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (OWLOntologyStorageException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private static String getLabel(OWLClass cls, OWLOntology ont) {
		String label = cls.toString();
		for (OWLAnnotation a : cls.getAnnotations(ont, OWLRDFVocabulary.RDFS_LABEL.getURI())) {
			label = "["+cls.toString()+" ! "+a.getAnnotationValueAsConstant().getLiteral()+"]";
		}
		return label;
	}

	private static OWLReasoner createReasoner(OWLOntologyManager man, String reasonerClassName) {
		try {
			// The following code is a little overly complicated.  The reason for using
			// reflection to create an instance of pellet is so that there is no compile time
			// dependency (since the pellet libraries aren't contained in the OWL API repository).
			// Normally, one would simply create an instance using the following incantation:
			//
			//     OWLReasoner reasoner = new Reasoner()
			//
			// Where the full class name for Reasoner is org.mindswap.pellet.owlapi.Reasoner
			//
			// Pellet requires the Pellet libraries  (pellet.jar, aterm-java-x.x.jar) and the
			// XSD libraries that are bundled with pellet: xsdlib.jar and relaxngDatatype.jar
			//String reasonerClassName = "org.mindswap.pellet.owlapi.Reasoner";
			//String reasonerClassName = "uk.ac.manchester.cs.factplusplus.owlapi.Reasoner";
			Class reasonerClass = Class.forName(reasonerClassName);
			Constructor<OWLReasoner> con = reasonerClass.getConstructor(OWLOntologyManager.class);
			return con.newInstance(man);
		}
		catch (ClassNotFoundException e) {
			throw new RuntimeException(e);
		}
		catch (IllegalAccessException e) {
			throw new RuntimeException(e);
		}
		catch (NoSuchMethodException e) {
			throw new RuntimeException(e);
		}
		catch (InvocationTargetException e) {
			throw new RuntimeException(e);
		}
		catch (InstantiationException e) {
			throw new RuntimeException(e);
		}
	}
}
