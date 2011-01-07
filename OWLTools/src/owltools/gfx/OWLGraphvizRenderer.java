package owltools.gfx;


import javax.imageio.*;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLNamedObject;
import org.semanticweb.owlapi.model.OWLObject;

import owltools.gfx.GraphStyle;
import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.graph.OWLQuantifiedProperty;

import java.awt.*;
import java.awt.geom.*;
import java.awt.image.*;
import java.io.*;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import uk.ac.ebi.interpro.graphdraw.*;

// todo - make a common abstract class for both of these
public class OWLGraphvizRenderer extends OWLGraphLayoutRenderer {

	public OWLGraphvizRenderer(OWLGraphWrapper owlGraphWrapper) {
		super(owlGraphWrapper);
	}


	public String renderDot() {
		StringBuilder s = new StringBuilder();
		s.append("digraph g {\n");
		
		for (OWLGraphLayoutNode node : g.nodes) {
			s.append("  "+safe(node)+" [");
			s.append("];\n");
		}
		for (OWLGraphStrokeEdge edge : g.edges) {
			s.append("  "+safe(edge.getChild())+" -> "+safe(edge.getParent())+" [");
			s.append("];\n");
		}

		
		s.append("}\n");
		
		
		
		return s.toString();
	}
	
	private String safe(OWLGraphLayoutNode node) {
		return node.getOwlObject().toString().replaceAll("[^a-ZA-Z_]", "");
	}


	public String renderImage(String fmt, OutputStream fos) throws FileNotFoundException, IOException {


		return "TODO";

	}

}



