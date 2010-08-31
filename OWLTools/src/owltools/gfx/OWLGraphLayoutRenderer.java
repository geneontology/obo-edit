package owltools.gfx;


import javax.imageio.*;

import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLNamedObject;
import org.semanticweb.owlapi.model.OWLObject;

import owltools.graph.OWLGraphEdge;
import owltools.graph.OWLGraphWrapper;
import owltools.graph.OWLQuantifiedProperty;

import java.awt.*;
import java.awt.geom.*;
import java.awt.image.*;
import java.io.*;
import java.util.HashMap;
import java.util.Map;

import uk.ac.ebi.interpro.graphdraw.*;

/**
 * Draw an example graph.
 * <p/>
 * After running see: <a href="../../../../../../hierarchicalGraph.html">results</a>.
 */
public class OWLGraphLayoutRenderer {

	private OWLGraphWrapper owlGraphWrapper;

	private StandardGraph<RectangularNode, StrokeEdge<RectangularNode>> g = new StandardGraph<RectangularNode, StrokeEdge<RectangularNode>>();
	HierarchicalLayout.Orientation orientation = HierarchicalLayout.Orientation.TOP;
	Stroke thinStroke = new BasicStroke(1);
	Stroke fatStroke = new BasicStroke(3);
	HashMap<OWLObject, RectangularNode> nodemap = new HashMap<OWLObject,RectangularNode>();

	Shape parent=StrokeEdge.standardArrow(10,8,0);
	Shape child=StrokeEdge.standardArrow(10,8,5);


	public OWLGraphLayoutRenderer(OWLGraphWrapper owlGraphWrapper) {
		super();
		this.owlGraphWrapper = owlGraphWrapper;
	}
	
	public RectangularNode makeNode(OWLObject ob) {
		String label = owlGraphWrapper.getLabel(ob);
		IRI iri = ((OWLNamedObject)ob).getIRI();
		RectangularNode node = 
			new RectangularNode(100, 30, label,
					iri.toString(), null, 
					label, Color.RED, Color.BLACK,
					thinStroke);
		return node;
	}
	
	public StrokeEdge<RectangularNode> makeEdge(OWLGraphEdge e) {
		RectangularNode n1 = nodemap.get(e.getSource());
		RectangularNode n2 = nodemap.get(e.getTarget());
		OWLQuantifiedProperty qr = e.getSingleQuantifiedProperty();
		Color color = Color.RED;
		if (qr.isSubClassOf()) {
			color = Color.GREEN;
		}
		StrokeEdge<RectangularNode> ge = 
			new StrokeEdge<RectangularNode>(n1, n2, color, fatStroke,parent,child);	
		return ge;
	}
	

	public void prepare () {


		for (OWLObject ob : owlGraphWrapper.getAllOWLObjects()) {
			RectangularNode node = makeNode(ob);
			g.nodes.add(node);
			nodemap.put(ob, node);
		}
		for (OWLObject ob : owlGraphWrapper.getAllOWLObjects()) {
			for (OWLGraphEdge e : owlGraphWrapper.getOutgoingEdges(ob)) {
				g.edges.add(makeEdge(e));
			}
		}
	}

	public void renderHTML() throws FileNotFoundException, IOException {
		prepare();

		PrintWriter pw = new PrintWriter(new FileWriter("hierarchicalGraph.html"));
		pw.println("<html><body>");


		HierarchicalLayout<RectangularNode, StrokeEdge<RectangularNode>> layout =
			new HierarchicalLayout<RectangularNode, StrokeEdge<RectangularNode>>(g, orientation);
		layout.betweenLevelExtraGap=20;
		layout.edgeLengthHeightRatio=1;
		layout.layout();

		final int width = layout.getWidth();
		final int height = layout.getHeight();
		BufferedImage image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);

		final Graphics2D g2 = image.createGraphics();

		g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);

		g2.setColor(Color.white);

		g2.fillRect(0, 0, width, height);

		g2.setColor(Color.black);

		for (StrokeEdge<RectangularNode> edge : g.edges) {
			edge.render(g2);
		}

		StringBuilder sb = new StringBuilder();

		for (RectangularNode node : g.nodes) {
			node.render(g2);
			sb.append(node.getImageMap());
		}

		ImageIO.write(image, "png", new FileOutputStream("hierarchicalGraph"+orientation+".png"));


		pw.println("<img src='hierarchicalGraph"+orientation+".png' usemap='#bob' /><map name='bob'>" + sb.toString() + "</map>");

		pw.println("</body></html>");
		pw.close();
	}

}



