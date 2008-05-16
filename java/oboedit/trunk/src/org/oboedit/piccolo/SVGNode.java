package org.oboedit.piccolo;

import java.io.IOException;
import java.net.URL;

import javax.swing.ImageIcon;

import org.oboedit.gui.SVGIcon;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolox.PFrame;

import org.apache.log4j.*;

public class SVGNode extends IconNode {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SVGNode.class);
	
	public SVGNode(String uri) throws IOException {
		super(new SVGIcon(uri));
	}

	public static void main(String[] args) throws Exception {
		PFrame frame = new PFrame();
		PNode node1 = new SVGNode("file:/Users/jrichter/downloads/lion.svg");
		PNode node2 = new SVGNode("file:/Users/jrichter/downloads/lion.svg");
		PNode node3 = new SVGNode("file:/Users/jrichter/drawing.svg");
		PNode node4 = new SVGNode("file:/Users/jrichter/drawing.svg");
		PNode node5 = new IconNode(new ImageIcon(
				new URL("http://www.google.com/intl/en_ALL/images/logo.gif")));
		node5.setWidth(1000);
		node5.setHeight(50);
		node5.setOffset(30, 300);
		
		node2.setWidth(node2.getWidth() / 2);
		node2.setHeight(node2.getHeight() / 2);
		node2.setOffset(100, 100);
		
		node3.setOffset(200, 100);
		node4.setBounds(0, 0, 12, 12);
		node4.setOffset(200, 200);
		frame.getCanvas().getLayer().addChild(node1);
		frame.getCanvas().getLayer().addChild(node2);
		frame.getCanvas().getLayer().addChild(node3);
		frame.getCanvas().getLayer().addChild(node4);
		frame.getCanvas().getLayer().addChild(node5);
		frame.getCanvas().repaint();
		frame.show();
	}
}
