package org.oboedit.piccolo;

import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.Rectangle2D;
import java.io.StringReader;

import javax.swing.JTextField;
import javax.swing.text.AbstractDocument;
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
		rootView = new RootView(new JTextField(), editorKit);
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

	protected static void layoutMinorAxis(View view, int targetSpan, int axis,
			int[] offsets, int[] spans) {
		int n = view.getViewCount();
		for (int i = 0; i < n; i++) {
			View v = view.getView(i);
			int max = (int) v.getMaximumSpan(axis);
			if (max < targetSpan) {
				// can't make the child this wide, align it
				float align = v.getAlignment(axis);
				offsets[i] = (int) ((targetSpan - max) * align);
				spans[i] = max;
			} else {
				// make it the target width, or as small as it can get.
				int min = (int) v.getMinimumSpan(axis);
				offsets[i] = 0;
				spans[i] = Math.max(min, targetSpan);
			}
		}
	}

	public static int getMinorAxisSize(View view, int targetSize, int axis) {
		int[] offsets = new int[view.getViewCount()];
		int[] spans = new int[view.getViewCount()];
		layoutMinorAxis(view, targetSize, axis, offsets, spans);
		int out = 0;
		for (int i : spans)
			if (i > out)
				out = i;
		return out;
	}

	protected static int getMajorAxisSize(View view, int targetSize, int axis) {
		int[] offsets = new int[view.getViewCount()];
		int[] spans = new int[view.getViewCount()];
		layoutMajorAxis(view, targetSize, axis, offsets, spans);
		int out = 0;
		for (int i : spans)
			out += i;
		return out;
	}

	protected static void layoutMajorAxis(View view, int targetSpan, int axis,
			int[] offsets, int[] spans) {
		/*
		 * first pass, calculate the preferred sizes and the flexibility to
		 * adjust the sizes.
		 */
		long preferred = 0;
		int n = view.getViewCount();
		for (int i = 0; i < n; i++) {
			View v = view.getView(i);
			spans[i] = (int) v.getPreferredSpan(axis);
			preferred += spans[i];
		}

		/*
		 * Second pass, expand or contract by as much as possible to reach the
		 * target span.
		 */

		// determine the adjustment to be made
		long desiredAdjustment = targetSpan - preferred;
		float adjustmentFactor = 0.0f;
		int[] diffs = null;

		if (desiredAdjustment != 0) {
			long totalSpan = 0;
			diffs = new int[n];
			for (int i = 0; i < n; i++) {
				View v = view.getView(i);
				int tmp;
				if (desiredAdjustment < 0) {
					tmp = (int) v.getMinimumSpan(axis);
					diffs[i] = spans[i] - tmp;
				} else {
					tmp = (int) v.getMaximumSpan(axis);
					diffs[i] = tmp - spans[i];
				}
				totalSpan += tmp;
			}

			float maximumAdjustment = Math.abs(totalSpan - preferred);
			adjustmentFactor = desiredAdjustment / maximumAdjustment;
			adjustmentFactor = Math.min(adjustmentFactor, 1.0f);
			adjustmentFactor = Math.max(adjustmentFactor, -1.0f);
		}

		// make the adjustments
		int totalOffset = 0;
		for (int i = 0; i < n; i++) {
			offsets[i] = totalOffset;
			if (desiredAdjustment != 0) {
				float adjF = adjustmentFactor * diffs[i];
				spans[i] += Math.round(adjF);
			}
			totalOffset = (int) Math.min((long) totalOffset + (long) spans[i],
					Integer.MAX_VALUE);
		}
	}

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

			view.setSize(maxWidth + 10, startHeight);
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
