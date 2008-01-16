package org.oboedit.util;

import java.io.IOException;
import java.net.URL;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JEditorPane;
import javax.swing.JFrame;

import org.apache.batik.bridge.BridgeContext;
import org.apache.batik.bridge.GVTBuilder;
import org.apache.batik.bridge.UserAgentAdapter;
import org.apache.batik.dom.svg.SAXSVGDocumentFactory;
import org.apache.batik.gvt.GraphicsNode;
import org.apache.batik.util.XMLResourceDescriptor;
import org.bbop.swing.HTMLLabel;
import org.bbop.swing.HyperlinkLabel;
import org.bbop.swing.IconFactory;
import org.bbop.swing.PluggableImageHTMLEditorKit;

import org.oboedit.gui.SVGIcon;
import org.w3c.dom.Document;


public class SVGUtil {

	public static GraphicsNode getSVG(String uri) throws IOException {
		String parser = XMLResourceDescriptor.getXMLParserClassName();
		SAXSVGDocumentFactory f = new SAXSVGDocumentFactory(parser);
		Document doc = f.createDocument(uri);
		GVTBuilder builder = new GVTBuilder();
		GraphicsNode node = builder.build(new BridgeContext(
				new UserAgentAdapter() {
				}), doc);
		return node;
	}

	public static void editorPaneMain(String[] args) {
		JEditorPane pane = new JEditorPane();
		PluggableImageHTMLEditorKit kit = new PluggableImageHTMLEditorKit();
		kit.installFactory("svg", new IconFactory() {

			public Icon createIcon(URL url, int width, int height) {
				try {
					return new SVGIcon(url.toString(), width, height);
				} catch (IOException e) {
					// TODO Auto-generated catch block
					return null;
				}
			}

		});
		pane.setEditorKit(kit);
		pane
				.setText("<html>Hey there: <img width='30' src='file:/Users/jrichter/downloads/lion.svg'></html>");
		JFrame frame = new JFrame();
		frame.setContentPane(pane);
		frame.pack();
		frame.setVisible(true);
	}

	public static void main(String[] args) {
		HyperlinkLabel label = new HyperlinkLabel();
		label.installIconFactory("svg", new IconFactory() {

			public Icon createIcon(URL url, int width, int height) {
				try {
					return new SVGIcon(url.toString(), width, height);
				} catch (IOException e) {
					// TODO Auto-generated catch block
					return null;
				}
			}
		});
		label
				.setText("<html>Hey there: <a href='click'><img src='file:/Users/jrichter/downloads/lion.svg'></a></html>");
		JFrame frame = new JFrame();
		frame.getContentPane().add(label);
		frame.pack();
		frame.setVisible(true);
	}
}
