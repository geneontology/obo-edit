package owltools.io;

import java.io.BufferedOutputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.PrintStream;

import org.semanticweb.owlapi.model.OWLClassExpression;
import org.semanticweb.owlapi.model.OWLNamedObject;
import org.semanticweb.owlapi.model.OWLObject;
import org.semanticweb.owlapi.model.OWLProperty;

import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.graph.OWLQuantifiedProperty;

public class GraphClosureWriter {

	protected PrintStream stream;

	public GraphClosureWriter(PrintStream stream) {
		super();
		this.stream = stream;
	}

	public GraphClosureWriter(String file) {
		super();
		FileOutputStream fos;
		try {
			fos = new FileOutputStream(file);
			this.stream = new PrintStream(new BufferedOutputStream(fos));
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}


	public void saveClosure(OWLGraphWrapper g) {
		for (OWLObject obj : g.getAllOWLObjects()) {
			//System.err.println(obj);
			for (OWLGraphEdge e : g.getOutgoingEdgesClosure(obj)) {
				//System.err.println("  E:"+e);
				stream.print(g.getIdentifier(obj));
				sep();
				//stream.print("[");
				int n = 0;
				for (OWLQuantifiedProperty qp : e.getQuantifiedPropertyList()) {
					if (n>0) {
						stream.print(", ");
					}
					if (qp.hasProperty()) {
						print(qp.getProperty());
						stream.print(" ");
					}
					stream.print(qp.getQuantifier());

					n++;
				}
				//stream.print("]");
				sep();
				if (!(e.getTarget() instanceof OWLNamedObject)) {
					//System.err.println("undefined behavior: "+e.getTarget());
					continue;
				}
				stream.print(g.getIdentifier(e.getTarget()));
				nl();
			}
		}
		stream.close();
	}

	protected void print(OWLNamedObject obj) {
		// TODO: prefixes
		if (obj.getIRI().toString() == null || obj.getIRI().toString().equals("")) {
			System.err.println("uh oh:"+obj);
		}
		stream.print(obj.getIRI().toString());
	}

	protected void print(OWLClassExpression obj) {
		stream.print(obj.toString());
	}

	protected void sep() {
		stream.print("\t");
	}

	protected void nl() {
		stream.print("\n");
	}
}
