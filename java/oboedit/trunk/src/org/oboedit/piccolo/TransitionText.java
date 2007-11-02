package org.oboedit.piccolo;

import java.awt.Color;
import java.awt.geom.Point2D;

import javax.swing.text.Document;
import javax.swing.text.ViewFactory;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.parser.ParserDelegator;

import org.bbop.swing.CustomLinebreakHTMLEditorKit;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;
import edu.umd.cs.piccolo.util.PPaintContext;
import edu.umd.cs.piccolox.nodes.PStyledText;

public class TransitionText extends ViewRenderedStyleText implements Morphable {

	protected boolean disablePaint = false;
	protected ViewRenderedStyleText dummyOld;
	protected ViewRenderedStyleText dummyNew;

	public TransitionText() {
		super();
	}
	
	public boolean doDefaultMorph() {
		return false;
	}
	
	public TransitionText(HTMLEditorKit editorKit, ViewFactory factory) {
		super(editorKit, factory);
		
		// setPaint(Color.black);
	}

	public PActivity animateTextChange(final String newText, long duration) {
		dummyOld = new ViewRenderedStyleText(getEditorKit(), viewFactory);
		dummyNew = new ViewRenderedStyleText(getEditorKit(), viewFactory);
		dummyOld.setTransparency(1);
		dummyNew.setTransparency(0);
		dummyOld.setPaint(getPaint());
		dummyNew.setPaint(getPaint());

		PCompoundActivity activity = new PCompoundActivity() {
			protected Point2D center = null;

			@Override
			protected void activityStarted() {
				super.activityStarted();
				disablePaint = true;
				setText(newText, false);

				dummyOld.setWidth(getWidth());
				dummyNew.setWidth(getWidth());
				dummyOld.setText(getText(), true);
				dummyNew.setText(newText, true);

				center = new Point2D.Double(getXOffset() + getWidth() / 2,
						getYOffset() + getHeight() / 2);
				// recomputeLayout();
				addChild(dummyOld);
				addChild(dummyNew);
				//PiccoloUtil.centerInParent(dummyOld, true, true);
				// PiccoloUtil.centerInParent(dummyNew, true, true);
			}

			@Override
			protected void activityFinished() {
				removeChild(dummyOld);
				removeChild(dummyNew);
				disablePaint = false;
				super.activityFinished();
			}
		};
		activity.addActivity(dummyOld.animateToTransparency(0, duration));
		activity.addActivity(dummyNew.animateToTransparency(1, duration));
		return activity;
		
		/*
		setText(newText);
		return new PActivity(0);
		*/
	}

	@Override
	protected void paint(PPaintContext arg0) {
		if (!disablePaint)
			super.paint(arg0);
	}

	public PActivity morphTo(PNode node, long duration) {
		if (node instanceof ViewRenderedStyleText) {
			return animateTextChange(((ViewRenderedStyleText) node).getLabel(),
					duration);
		}
		return null;
	}
}
