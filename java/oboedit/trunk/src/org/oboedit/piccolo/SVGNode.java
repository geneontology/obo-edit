package org.oboedit.piccolo;

import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;
import java.io.IOException;

import org.apache.batik.bridge.BridgeContext;
import org.apache.batik.bridge.GVTBuilder;
import org.apache.batik.bridge.UserAgentAdapter;
import org.apache.batik.dom.svg.SAXSVGDocumentFactory;
import org.apache.batik.gvt.GraphicsNode;
import org.apache.batik.util.XMLResourceDescriptor;
import org.oboedit.util.SVGUtil;
import org.w3c.dom.Document;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.util.PBounds;
import edu.umd.cs.piccolo.util.PPaintContext;
import edu.umd.cs.piccolox.PFrame;

public class SVGNode extends PNode {

	protected GraphicsNode node;

	public SVGNode(String uri) throws IOException {
		setURI(uri);
	}

	public void setURI(String uri) throws IOException {
		node = SVGUtil.getSVG(uri);
		setNode(node);
	}

	public void setNode(GraphicsNode node) {
		this.node = node;
		setBounds(node.getBounds());
	}

	@Override
	protected void paint(PPaintContext paintContext) {
		Rectangle2D svgBounds = node.getBounds();
		PBounds b = getBoundsReference();
		Graphics2D g2 = paintContext.getGraphics();

		if (b.x != 0 || b.y != 0 || b.width != svgBounds.getWidth()
				|| b.height != svgBounds.getHeight()) {
			g2.translate(b.x, b.y);
			g2.scale(b.width / svgBounds.getWidth(), b.height
					/ svgBounds.getHeight());
			node.paint(g2);
			// g2.drawImage(image, 0, 0, null);
			g2.scale(svgBounds.getWidth() / b.width, svgBounds.getHeight()
					/ b.height);
			g2.translate(-b.x, -b.y);
		} else {
			node.paint(g2);
		}
	}

	public static void main(String[] args) throws IOException {
		PFrame frame = new PFrame();
		PNode node1 = new SVGNode("file:/Users/jrichter/downloads/lion.svg");
		PNode node2 = new SVGNode("file:/Users/jrichter/downloads/lion.svg");
		PNode node3 = new SVGNode("file:/Users/jrichter/drawing.svg");
		PNode node4 = new SVGNode("file:/Users/jrichter/drawing.svg");
		node2.scale(.5);
		node2.setOffset(100, 100);
		node3.setOffset(200, 100);
		node4.setBounds(0, 0, 12, 12);
		node4.setOffset(200, 200);
		frame.getCanvas().getLayer().addChild(node1);
		frame.getCanvas().getLayer().addChild(node2);
		frame.getCanvas().getLayer().addChild(node3);
		frame.getCanvas().getLayer().addChild(node4);
		frame.show();
	}
}
