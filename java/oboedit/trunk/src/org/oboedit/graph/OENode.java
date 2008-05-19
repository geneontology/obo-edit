package org.oboedit.graph;

import java.awt.Color;
import java.awt.Paint;
import java.awt.Shape;
import javax.swing.text.html.HTMLEditorKit;

import org.bbop.swing.ShapeUtil;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.LinkedObject;
import org.obo.util.FilterUtil;
import org.obo.util.HTMLUtil;
import org.oboedit.controller.FilterManager;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.gui.filter.BackgroundColorSpecField;
import org.oboedit.gui.filter.ColorProvider;
import org.oboedit.gui.filter.ConfiguredColor;
import org.oboedit.gui.filter.GeneralRendererSpec;
import org.oboedit.gui.filter.GeneralRendererSpecField;
import org.oboedit.gui.filter.RenderSpec;
import org.oboedit.piccolo.Morphable;
import org.oboedit.piccolo.PiccoloUtil;
import org.oboedit.piccolo.TransitionText;
import org.oboedit.piccolo.ViewRenderedStyleText;
import org.oboedit.util.GUIUtil;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;
import edu.umd.cs.piccolo.activities.PActivity.PActivityDelegate;

import org.apache.log4j.*;

public class OENode extends PCNode implements Morphable {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OENode.class);

	protected static final Object KEY_LABEL = "KEY";

	// protected ViewRenderedStyleText field = new TransitionText();
	protected ViewRenderedStyleText field = new ViewRenderedStyleText();

	protected int roundingSize = 10;

	// protected PText field = new PText();
	protected NamedChildProvider provider;

	protected HTMLEditorKit kit = new HTMLEditorKit();

	protected int x_label_padding = 10;
	protected int y_label_padding = 0;
	protected int x_margin = 0;
	protected int y_margin = 5;

	protected Color nodeBackgroundColor = new Color(230, 230, 230); // very light gray

	public OENode(LinkedObject lo, LinkDatabaseCanvas canvas, Shape s) {
		this(lo, canvas, DefaultNamedChildProvider.getInstance(), s);
	}

	public OENode(LinkedObject lo, LinkDatabaseCanvas canvas,
			NamedChildProvider provider, Shape s) {
		field.setWidth(s.getBounds().getWidth());
		field.setHeight(s.getBounds().getHeight());
		try {
			s = ShapeUtil.createRoundRectangle(null, roundingSize, (float) s
					.getBounds().getX(), (float) s.getBounds().getY(),
					(float) s.getBounds().getWidth() + x_margin
							+ x_label_padding, (float) s.getBounds()
							.getHeight()
							+ y_margin + y_label_padding);
			initialize(lo, provider, s);
			setNamedChild(KEY_LABEL, field);
			field.setPickable(false);
			setLabel(canvas.generateLabel(lo));
			RenderSpec spec = GUIUtil
					.getSpec(canvas, lo, FilterManager.getManager()
							.getGlobalTermRenderers(), canvas
							.getObjectRenderers(), canvas
							.getAutomaticObjectRenderers());
//			setPaint(Color.lightGray);
			setPaint(nodeBackgroundColor);
			if (spec instanceof GeneralRendererSpec) {
				ColorProvider c = ((GeneralRendererSpec) spec)
						.getValue(BackgroundColorSpecField.FIELD);
				if (c != null) {
					setPaint(c.getColor(canvas, lo));
				}
			}
		} catch (Throwable e) {
			e.printStackTrace();
		}
	}

	public int getRoundingSize() {
		return roundingSize;
	}

	@Override
	public void setPathTo(Shape shape) {
		super.setPathTo(shape);
	}

	public String getLabel() {
		return this.field.getText();
	}

	public void setLabel(String field) {

		this.field.setText(field, false);
		updateFieldDimensions();
		// setShape(ShapeUtil.createRoundRectangle((float) 0, (float) 0,
		// (float) getPreferredWidth(), (float) getPreferredHeight()));
	}

	@Override
	protected void internalUpdateBounds(double x, double y, double width,
			double height) {
		super.internalUpdateBounds(x, y, width, height);
		updateFieldDimensions();
	}

	protected void updateFieldDimensions() {
		field.setWidth(getWidth() + x_label_padding);
		field.setHeight(getHeight() + y_label_padding);
		PiccoloUtil.centerInParent(this.field, true, true, false);
	}

	public PActivity animateSetLabel(String text, long duration) {
		if (field instanceof TransitionText)
			return ((TransitionText) field).animateTextChange(text, duration);
		else {
			setLabel(text);
			PActivity act = new PActivity(0);
			return act;
		}
	}

	@Override
	public void setPaint(Paint arg0) {
		// TODO Auto-generated method stub
		super.setPaint(arg0);
	}

	public int getPreferredHeight() {
		return (int) field.getHeight();
	}

	public int getPreferredWidth() {
		return (int) field.getWidth();

	}

	public String toString() {
		return lo.toString();
	}

	public boolean doDefaultMorph() {
		return true;
	}

	public PActivity morphTo(PNode node, long duration) {
		if (node instanceof OENode) {
			// return new PActivity(0);
			return animateSetLabel(((OENode) node).getLabel(), duration);
		} else {
			return new PActivity(0);
		}
	}

}
