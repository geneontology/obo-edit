package org.oboedit.graph;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.geom.GeneralPath;
import java.awt.geom.Point2D;

import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;

import org.bbop.swing.ShapeUtil;
import org.bbop.util.StringUtil;
import org.obo.datamodel.Link;
import org.obo.datamodel.PathCapable;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.util.ExplanationUtil;
import org.obo.util.HTMLUtil;
import org.obo.util.TermUtil;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.piccolo.PiccoloUtil;
import org.oboedit.piccolo.StyledText;
import org.oboedit.piccolo.ViewRenderedStyleText;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.nodes.PPath;
import edu.umd.cs.piccolo.nodes.PText;
import edu.umd.cs.piccolox.nodes.P3DRect;
import edu.umd.cs.piccolox.nodes.PStyledText;

// TODO Modify OELink to create a distinct path node so decorations can appear behind it6

public class OELink extends PCNode {

	protected static final int ICON_PANEL_MARGIN = 2;

	protected static final int ICON_PANEL_HEIGHT = 20;

	protected static final int ICON_PANEL_WIDTH = 20;

	protected static final Object KEY_ICON_PANEL = new Object();

	protected static final Object KEY_ARROWHEAD = new Object();

	protected TypeIconManager iconManager;

	protected TypeColorManager colorManager;

	protected NamedChildProvider provider;

	protected float arrowheadHeight = 24f;

	protected float arrowheadWidth = 12f;

	public OELink(Link link, TypeIconManager iconManager,
			TypeColorManager colorManager, Shape s) {
		this(link, iconManager, colorManager, DefaultNamedChildProvider
				.getInstance(), s);
	}

	public OELink(Link link, TypeIconManager iconManager,
			TypeColorManager colorManager, NamedChildProvider provider, Shape s) {
		this.colorManager = colorManager;
		this.iconManager = iconManager;
		initialize(link, provider, s);

		setLineWeight(3);
		Paint typeColor = colorManager.getColor(link.getType());
		getPathDelegate().setStrokePaint(typeColor);
		getPathDelegate().setPaint(null);

		PNode iconPanel = createIconPanel();
		PNode arrowhead = createArrowhead();
		iconPanel.setPaint(typeColor);
		arrowhead.setPaint(typeColor);

		setNamedChild(KEY_ARROWHEAD, arrowhead);
		setNamedChild(KEY_ICON_PANEL, iconPanel);

	}

	public PNode createArrowhead() {
		GeneralPath s = new GeneralPath();
		s.moveTo(0, arrowheadHeight);
		s.lineTo(arrowheadWidth / 2, 0);
		s.lineTo(arrowheadWidth, arrowheadHeight);
		s.closePath();
		Point2D attachmentPoint = new Point2D.Double(arrowheadWidth / 2,
				arrowheadHeight);
		PPath arrowhead = new PPath(s);
		arrowhead.setStroke(null);
		double len = ShapeUtil.getLength(getPathDelegate().getPathReference(),
				.5, 8);
		double arrowheadPlacementRatio = (len - arrowheadHeight) / len;
		double[] temp = ShapeUtil.getPosAndAngleAtRatio(getPathDelegate()
				.getPathReference(), arrowheadPlacementRatio, .5, 8);
		arrowhead.setOffset(temp[0] - attachmentPoint.getX(), temp[1]
				- attachmentPoint.getY());
		arrowhead.rotateAboutPoint(temp[2] + Math.PI / 2, attachmentPoint);
		return arrowhead;
	}

	public PNode createIconPanel() {
		Shape s = ShapeUtil.createRoundRectangle(0, 0, ICON_PANEL_WIDTH,
				ICON_PANEL_HEIGHT);
		PPath iconPanel = new PPath(s);
		iconPanel.setChildrenPickable(false);
		iconPanel.setStroke(null);
		Point2D panelLoc = ShapeUtil.getPointAtRatio(getPathDelegate()
				.getPathReference(), .5, .01, 5);
		iconPanel.setOffset(panelLoc.getX() - iconPanel.getWidth() / 2,
				panelLoc.getY() - iconPanel.getHeight() / 2);
		PNode icon = iconManager.getIcon(getLink().getType());
		iconPanel.addChild(icon);
		icon.centerFullBoundsOnPoint(iconPanel.getWidth() / 2, iconPanel
				.getHeight() / 2);

		double zoom = Math.min(ICON_PANEL_WIDTH - ICON_PANEL_MARGIN / 2,
				ICON_PANEL_HEIGHT - ICON_PANEL_MARGIN / 2)
				/ Math.max(icon.getFullBoundsReference().getWidth(), icon
						.getFullBoundsReference().getHeight());
		icon.scaleAboutPoint(zoom,
				icon.getFullBoundsReference().getWidth() / 2, icon
						.getFullBoundsReference().getHeight() / 2);
		return iconPanel;
	}

	public Link getLink() {
		return (Link) getObject();
	}

	public void setLineWeight(int lineWeight) {
		if (TermUtil.isImplied(getObject())) {
			float[] dashArr = { 1, 10 };
			getPathDelegate().setStroke(
					new BasicStroke(lineWeight, BasicStroke.CAP_ROUND,
							BasicStroke.JOIN_ROUND, 10f, dashArr, 0f));
		} else
			getPathDelegate().setStroke(new BasicStroke(lineWeight));
	}
}
