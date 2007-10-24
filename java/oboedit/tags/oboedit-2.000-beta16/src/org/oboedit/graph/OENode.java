package org.oboedit.graph;

import java.awt.Shape;
import java.io.IOException;
import java.io.StringReader;
import java.util.Iterator;

import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;

import org.bbop.swing.ShapeUtil;
import org.obo.datamodel.LinkedObject;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.piccolo.Morphable;
import org.oboedit.piccolo.PiccoloUtil;
import org.oboedit.piccolo.TransitionText;
import org.oboedit.piccolo.ViewRenderedStyleText;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;

public class OENode extends PCNode implements Morphable {

	protected static final Object KEY_LABEL = "KEY";

	// protected ViewRenderedStyleText field = new TransitionText();
	protected ViewRenderedStyleText field = new ViewRenderedStyleText();

	protected int preferredWidth = -1;

	protected int preferredHeight = -1;

	protected static final int MARGIN = 10;

	protected int roundingSize = 10;

	// protected PText field = new PText();
	protected NamedChildProvider provider;

	protected HTMLEditorKit kit = new HTMLEditorKit();

	public OENode(LinkedObject lo, LinkDatabaseCanvas canvas, Shape s) {
		this(lo, canvas, DefaultNamedChildProvider.getInstance(), s);
	}

	public OENode(LinkedObject lo, LinkDatabaseCanvas canvas,
			NamedChildProvider provider, Shape s) {
		s = ShapeUtil.createRoundRectangle(null, roundingSize, (float) s
				.getBounds().getX(), (float) s.getBounds().getY(), (float) s
				.getBounds().getWidth(), (float) s.getBounds().getHeight());
		initialize(lo, provider, s);
		// field.setConstrainWidthToTextWidth(true);
		setNamedChild(KEY_LABEL, field);
		field.setPickable(false);
		setLabel(lo.getName());
	}

	public int getRoundingSize() {
		return roundingSize;
	}

	protected static String escapeHTML(String s) {
		StringBuffer out = new StringBuffer();
		for (int i = 0; i < s.length(); i++) {
			if (s.charAt(i) == '<')
				out.append("&lt;");
			else if (s.charAt(i) == '>')
				out.append("&gt;");
			else
				out.append(s.charAt(i));
		}
		return out.toString();
	}

	public static String getLabelAsHTML(String field) {
		field = "<html><center><font face='Arial' color='black'>"
				+ escapeHTML(field) + "</font></center></html>";
		return field;
	}

	public static int getInitialNodeWidth() {
		return 180;
	}

	public String getLabel() {
		return this.field.getLabel();
	}

	public void setLabel(String field) {
		this.field.setWidth(getInitialNodeWidth());
		String s = getLabelAsHTML(field);
		this.field.setText(s);

		// if (this.field.getHeight() > getPreferredHeight()
		// || this.field.getWidth() > getPreferredWidth()) {
		setShape(ShapeUtil.createRoundRectangle((float) 0, (float) 0,
				(float) getPreferredWidth(), (float) getPreferredHeight()));
		// }
	}

	@Override
	protected void internalUpdateBounds(double x, double y, double width,
			double height) {
		PiccoloUtil.centerInParent(this.field, true, true, true);
		field.setOffset(field.getXOffset() - 4, field.getYOffset());
		super.internalUpdateBounds(x, y, width, height);
	}

	public PActivity animateSetLabel(String text, long duration) {
		if (field instanceof TransitionText)
			return ((TransitionText) field).animateTextChange(text, duration);
		else {
			setLabel(text);
			return new PActivity(0);
		}
	}

	public int getDefaultPreferredWidth() {
		int width = (int) field.getWidth() + MARGIN;
		return width;
	}

	protected int getDefaultPreferredHeight() {
		return (int) field.getHeight() + MARGIN;
	}

	public int getPreferredHeight() {
		if (preferredHeight == -1)
			return getDefaultPreferredHeight();
		else
			return preferredHeight;
	}

	public void setPreferredHeight(int preferredHeight) {
		// this.preferredHeight = preferredHeight;
	}

	public int getPreferredWidth() {
		if (preferredWidth == -1)
			return getDefaultPreferredWidth();
		else
			return preferredWidth;
	}

	public void setPreferredWidth(int preferredWidth) {
		// this.preferredWidth = preferredWidth;
	}

	public String toString() {
		return lo.toString();
	}

	public boolean doDefaultMorph() {
		return true;
	}

	public PActivity morphTo(PNode node, long duration) {
		if (node instanceof OENode) {
			return animateSetLabel(((OENode) node).getLabel(), duration);
		} else {
			return new PActivity(0);
		}
	}

}
