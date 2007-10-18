package org.oboedit.graph;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.print.PageFormat;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.util.LinkedList;
import java.util.List;

import javax.swing.JMenuItem;

import org.apache.batik.dom.GenericDOMImplementation;
import org.apache.batik.svggen.SVGGraphics2D;
import org.apache.batik.svggen.SVGGraphics2DIOException;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.piccolo.FullPaintCamera;
import org.w3c.dom.DOMImplementation;
import org.w3c.dom.Document;

import edu.umd.cs.piccolo.event.PInputEvent;
import edu.umd.cs.piccolo.util.PPaintContext;

public class SaveScreenMenuFactory implements RightClickMenuFactory {

	public List<JMenuItem> getMenuItems(final LinkDatabaseCanvas canvas,
			PInputEvent e) {
		JMenuItem item = new JMenuItem("Save current graph as SVG");
		item.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				saveCanvas(canvas);
			}

		});
		List<JMenuItem> items = new LinkedList<JMenuItem>();
		items.add(item);
		return items;
	}

	protected void saveCanvas(LinkDatabaseCanvas canvas) {
        // Get a DOMImplementation.
        try {
        DOMImplementation domImpl =
            GenericDOMImplementation.getDOMImplementation();

        // Create an instance of org.w3c.dom.Document.
        String svgNS = "http://www.w3.org/2000/svg";
        Document document = domImpl.createDocument(svgNS, "svg", null);

        // Create an instance of the SVG Generator.
        SVGGraphics2D svgGenerator = new SVGGraphics2D(document);
        FullPaintCamera camera = (FullPaintCamera) canvas.getCamera();
        camera.paintUnclipped(svgGenerator);

        // Finally, stream out SVG to the standard output using
        // UTF-8 encoding.
        boolean useCSS = true; // we want to use CSS style attributes
            Writer out = new OutputStreamWriter(new FileOutputStream("/Users/jrichter/graph.svg"), "UTF-8");
			svgGenerator.stream(out, useCSS);
		} catch (SVGGraphics2DIOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (Throwable t) {
			t.printStackTrace();
		}
	}
	
}
