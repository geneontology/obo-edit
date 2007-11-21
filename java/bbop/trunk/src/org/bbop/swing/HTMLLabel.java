package org.bbop.swing;

import java.io.Reader;
import java.io.StringReader;
import java.net.URL;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.plaf.LabelUI;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.text.Document;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import javax.swing.text.html.HTMLDocument;

import org.bbop.swing.ExtensibleLabelUI.BasicEditorKit;
import org.bbop.swing.ExtensibleLabelUI.Renderer;

public class HTMLLabel extends JLabel {

	protected ExtensibleLabelUI labelui;
	protected boolean htmlEnabled = true;

	public HTMLLabel() {
		super();
	}

	public HTMLLabel(String text) {
		super(text);
	}

	@Override
	public void setUI(LabelUI ui) {
		labelui = new ExtensibleLabelUI();
		super.setUI(labelui);
	}

	public void installIconFactory(String suffix, IconFactory factory) {
		labelui.installFactory(suffix, factory);
	}

	public void uninstallIconFactory(String suffix, IconFactory factory) {
		labelui.uninstallFactory(suffix, factory);
	}

	public boolean isHtmlEnabled() {
		return htmlEnabled;
	}

	public void setHtmlEnabled(boolean htmlEnabled) {
		this.htmlEnabled = htmlEnabled;
		labelui.setDisableHTML(!htmlEnabled);
	}
	
	public BasicEditorKit getEditorKit() {
		return labelui.getEditorKit();
	}

	public static View createHTMLView(BasicEditorKit kit, JComponent c,
			String html) {
		// BasicEditorKit kit = editorKit;
		Document doc = kit
				.createDefaultDocument(c.getFont(), c.getForeground());
		Object base = c.getClientProperty(BasicHTML.documentBaseKey);
		if (base instanceof URL) {
			((HTMLDocument) doc).setBase((URL) base);
		}
		Reader r = new StringReader(html);
		try {
			kit.read(r, doc, 0);
		} catch (Throwable e) {
		}
		ViewFactory f = kit.getViewFactory();
		View hview = f.create(doc.getDefaultRootElement());
		View v = new Renderer(c, f, hview);
		return v;
	}
}
