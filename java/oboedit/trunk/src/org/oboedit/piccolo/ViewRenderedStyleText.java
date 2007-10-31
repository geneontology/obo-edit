package org.oboedit.piccolo;

import java.awt.Container;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.geom.Rectangle2D;
import java.io.StringReader;

import javax.swing.JTextField;
import javax.swing.text.AbstractDocument;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.BoxView;
import javax.swing.text.ComponentView;
import javax.swing.text.Document;
import javax.swing.text.EditorKit;
import javax.swing.text.Element;
import javax.swing.text.IconView;
import javax.swing.text.LabelView;
import javax.swing.text.StyleConstants;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import javax.swing.text.html.BlockView;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.InlineView;
import javax.swing.text.html.ListView;
import javax.swing.text.html.ParagraphView;

import org.bbop.swing.RootView;

import com.sun.java.swing.SwingUtilities2;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.util.PPaintContext;

public class ViewRenderedStyleText extends PNode {

	protected EditorKit editorKit;

	protected ViewFactory viewFactory;

	protected Document document;

	protected View view;

	protected RootView rootView;

	protected String text = "";

	protected boolean useFixedWidth = true;

	public ViewRenderedStyleText() {
		this(new HTMLEditorKit(), null);
	}

	public ViewRenderedStyleText(EditorKit editorKit, ViewFactory viewFactory) {
		this.editorKit = editorKit;
		if (viewFactory == null)
			this.viewFactory = editorKit.getViewFactory();
		else
			this.viewFactory = viewFactory;
		document = editorKit.createDefaultDocument();
		((AbstractDocument) document).setAsynchronousLoadPriority(-1);
		JTextField field = new JTextField();
		field.putClientProperty(SwingUtilities2.AA_TEXT_PROPERTY_KEY, Boolean.TRUE);
		rootView = new RootView(field, editorKit);
	}

	public EditorKit getEditorKit() {
		return editorKit;
	}

	public String getLabel() {
		try {
			return document.getText(0, document.getLength());
		} catch (BadLocationException e) {
			return text;
		}
	}

	public String getText() {
		return text;
	}

	public void setText(String text) {
		try {
			this.text = text;
			// document = editorKit.createDefaultDocument();
			((AbstractDocument) document).setAsynchronousLoadPriority(-1);

			document.remove(0, document.getLength());
			editorKit.read(new StringReader(text), document, 0);

			view = editorKit.getViewFactory().create(
					document.getRootElements()[0]);
			rootView.setView(view);
			updateBounds();
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}

	protected Rectangle rect = new Rectangle();

	protected void updateBounds() {

		if (useFixedWidth && view instanceof BoxView) {
			view.setSize((float) getWidth(), (float) getHeight());
			float startHeight = view.getPreferredSpan(View.Y_AXIS);

			Rectangle2D rect = new Rectangle2D.Double(0, 0, getWidth(),
					startHeight);

			int maxWidth = 0;
			for (int i = 0; i < view.getDocument().getLength(); i++) {
				try {
					Rectangle s = view.modelToView(i, rect).getBounds();
					int maxpos = (int) (s.getX() + s.getWidth());
					if (maxpos > maxWidth)
						maxWidth = maxpos;
				} catch (BadLocationException ex) {
				}
			}

			view.setSize(maxWidth, startHeight);
			/*
			 * view .setSize((float) getWidth(), view
			 * .getPreferredSpan(View.Y_AXIS));
			 */

			setWidth(maxWidth);
			setHeight(startHeight);
		}
	}

	protected void paint(PPaintContext paintContext) {
		rect.width = (int) getWidth();
		rect.height = (int) getHeight();
		Graphics2D g = paintContext.getGraphics();
		/*
		 * try { System.err.println("document " + document.getText(0,
		 * document.getLength())); } catch (BadLocationException e) { // TODO
		 * Auto-generated catch block e.printStackTrace(); }
		 */
		view.paint(g, rect);
		// setPaint(Color.red);
		// super.paint(paintContext);
	}
}
