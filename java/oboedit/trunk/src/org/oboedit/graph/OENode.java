package org.oboedit.graph;

import java.awt.Shape;
import javax.swing.text.html.HTMLEditorKit;

import org.bbop.swing.ShapeUtil;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.LinkedObject;
import org.obo.util.HTMLUtil;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.gui.components.TermImageDisplayComponent;
import org.oboedit.piccolo.Morphable;
import org.oboedit.piccolo.PiccoloUtil;
import org.oboedit.piccolo.TransitionText;
import org.oboedit.piccolo.ViewRenderedStyleText;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;

public class OENode extends PCNode implements Morphable {

	protected static final Object KEY_LABEL = "KEY";

	//protected ViewRenderedStyleText field = new TransitionText();
	protected ViewRenderedStyleText field = new ViewRenderedStyleText();

	protected int roundingSize = 10;

	// protected PText field = new PText();
	protected NamedChildProvider provider;

	protected HTMLEditorKit kit = new HTMLEditorKit();
	
//	int x_margin = 20;
//	int y_margin = 20;

	public OENode(LinkedObject lo, LinkDatabaseCanvas canvas, Shape s) {
		this(lo, canvas, DefaultNamedChildProvider.getInstance(), s);
	}

	public OENode(LinkedObject lo, LinkDatabaseCanvas canvas,
			NamedChildProvider provider, Shape s) {
		field.setWidth(s.getBounds().getWidth()+1);
		s = ShapeUtil.createRoundRectangle(null, roundingSize, (float) s
				.getBounds().getX(), (float) s.getBounds().getY(), (float) s
				.getBounds().getWidth(), (float) s.getBounds().getHeight());
		initialize(lo, provider, s);
		// field.setConstrainWidthToTextWidth(true);
		System.err.println("CREATED NODE FOR " + lo.getID() + " width="
				+ s.getBounds().getWidth() + ", height="
				+ s.getBounds().getHeight());
		setNamedChild(KEY_LABEL, field);
		field.setPickable(false);
		setLabel(canvas.generateLabel(lo));
	}

	public int getRoundingSize() {
		return roundingSize;
	}

	public static String getLabelAsHTML(String field) {
		// String imgURL = TermImageDisplayComponent.getFile((LinkedObject) io);
		// if (imgURL == null) {
		// field = "<html><center><font face='Arial' color='black'>"
		// + HTMLUtil.escapeHTML(field) + "</font></center></html>";
		// } else {
		// field = "<html><table border=0><tr valign=center><td><img align=left
		// src='" + imgURL
		// + "' width='50'></td><td><font face='Arial' color='black'>"
		// + HTMLUtil.escapeHTML(field) + "</font></td></table></html>";
		// }
		// return field;
		return "<html><center>" + field + "</center></html>";
	}

	public String getLabel() {
		return this.field.getText();
	}

	public void setLabel(String field) {

		this.field.setText(field);

		// if (this.field.getHeight() > getPreferredHeight()
		// || this.field.getWidth() > getPreferredWidth()) {
		setShape(ShapeUtil.createRoundRectangle((float) 0, (float) 0,
				(float) getPreferredWidth(), (float) getPreferredHeight()));
		PiccoloUtil.centerInParent(this.field, true, true, false);
		// }
	}

	@Override
	protected void internalUpdateBounds(double x, double y, double width,
			double height) {
//		field.setOffset(field.getXOffset(), field.getYOffset());
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
		int width = (int) field.getWidth();
		return width;
	}

	protected int getDefaultPreferredHeight() {
		return (int) field.getHeight();
	}

	public int getPreferredHeight() {
		return getDefaultPreferredHeight();
	}

	public int getPreferredWidth() {
		return getDefaultPreferredWidth();

	}

	public String toString() {
		return lo.toString();
	}

	public boolean doDefaultMorph() {
		return true;
	}

	public PActivity morphTo(PNode node, long duration) {
		if (node instanceof OENode) {
//			return new PActivity(0);
			return animateSetLabel(((OENode) node).getLabel(), duration);
		} else {
			return new PActivity(0);
		}
	}

}
