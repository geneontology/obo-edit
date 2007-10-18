package org.oboedit.graph;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Shape;
import java.awt.Stroke;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.GeneralPath;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;

import org.bbop.swing.ShapeUtil;
import org.bbop.util.CollectionUtil;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.util.TermUtil;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.piccolo.PCompoundActivity;
import org.oboedit.piccolo.PZNodeCache;
import org.oboedit.piccolo.PiccoloUtil;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;
import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEvent;
import edu.umd.cs.piccolo.event.PInputEventListener;
import edu.umd.cs.piccolo.nodes.PPath;
import edu.umd.cs.piccolo.nodes.PText;
import edu.umd.cs.piccolo.util.PBounds;

public class LinkoutMeter extends PNode {

	protected final static int LINK_WIDTH = 2;

	protected final static int LINK_HEIGHT = 6;

	protected PCompoundActivity currentButtonDisplayActivity;

	protected Color buttonColor = new Color(100, 149, 237);

	LinkedObject term;

	CollapsibleLinkDatabase collapsibleLinkDatabase;

	protected ExpandCollapseListener expandListener = new ExpandCollapseListener() {
		public void expandStateChanged(ExpansionEvent e) {
			sortAndRecolor(true);
			hideButtons(false);
		}
	};

	protected PInputEventListener closeListener = new PBasicInputEventHandler() {
		public void mouseClicked(PInputEvent event) {
			if (canvas.getCamera().getViewScale() < getViewScaleThreshold())
				return;
			collapsibleLinkDatabase.setVisible(term, false);
			canvas.fireExpandEvent(new ExpansionEvent(this));
		}
	};

	protected PInputEventListener expandAllListener = new PBasicInputEventHandler() {
		public void mouseClicked(PInputEvent event) {
			if (canvas.getCamera().getViewScale() < getViewScaleThreshold())
				return;
			if (children) {
				Set expand = new HashSet();
				Iterator it = canvas.getLinkProviderDatabase()
						.getChildren(term).iterator();
				while (it.hasNext()) {
					Link link = (Link) it.next();
					expand.add(link.getChild());
				}
				collapsibleLinkDatabase.setChildSetVisible(expand, true);
				canvas.fireExpandEvent(new ExpansionEvent(this));
			} else {
				Set expand = new HashSet();
				Iterator it = canvas.getLinkProviderDatabase().getParents(term)
						.iterator();
				while (it.hasNext()) {
					Link link = (Link) it.next();
					expand.add(link.getParent());
				}
				collapsibleLinkDatabase.setParentSetVisible(expand, true);
				canvas.fireExpandEvent(new ExpansionEvent(this));
			}
		}
	};

	protected PInputEventListener collapseAllListener = new PBasicInputEventHandler() {
		public void mouseClicked(PInputEvent event) {
			if (canvas.getCamera().getViewScale() < getViewScaleThreshold())
				return;
			if (children) {
				Set expand = new HashSet();
				Iterator it = canvas.getLinkProviderDatabase()
						.getChildren(term).iterator();
				while (it.hasNext()) {
					Link link = (Link) it.next();
					expand.add(link.getChild());
				}
				collapsibleLinkDatabase.setChildSetVisible(expand, false);
				canvas.fireExpandEvent(new ExpansionEvent(this));
			} else {
				Set expand = new HashSet();
				Iterator it = canvas.getLinkProviderDatabase().getParents(term)
						.iterator();
				while (it.hasNext()) {
					Link link = (Link) it.next();
					expand.add(link.getParent());
				}
				collapsibleLinkDatabase.setParentSetVisible(expand, false);
				canvas.fireExpandEvent(new ExpansionEvent(this));
			}
		}
	};

	protected PInputEventListener mouseoverListener = new PBasicInputEventHandler() {
		boolean isInside = false;

		public void mouseEntered(PInputEvent event) {
			if (canvas.getCamera().getViewScale() < getViewScaleThreshold())
				hideButtons(false);
			else {
				if (!isInside && event.getPath().acceptsNode(LinkoutMeter.this)) {
					isInside = true;
					showButtons(true);
				}
			}
		}

		public void mouseExited(PInputEvent event) {
			if (canvas.getCamera().getViewScale() < getViewScaleThreshold())
				hideButtons(false);
			else {
				if (!getFullBounds().contains(event.getPosition())) {
					isInside = false;
					hideButtons(true);
				}
			}
		}
	};

	protected float getViewScaleThreshold() {
		return 1;
	}

	protected static TooltipFactory tooltipFactory = new AbstractTooltipFactory() {
		public long getDelay() {
			return 0;
		}

		public PNode getTooltip(LinkDatabaseCanvas canvas, PNode node) {
			Link link = (Link) node.getAttribute("link");
			if (link != null) {
				return new PText(link.toString());
			} else
				return null;
		}
	};

	List<PPath> parentList;

	boolean children;

	protected PPath expandAllButton;

	protected PPath collapseAllButton;

	protected PPath closeButton;

	protected boolean empty = false;
	
	protected Set currentVisibleParents;

	protected final Comparator typeComparator = new Comparator() {
		public int compare(Object o1, Object o2) {
			if (o1 instanceof PNode && o2 instanceof PNode) {
				Link l1 = (Link) ((PNode) o1).getAttribute("link");
				Link l2 = (Link) ((PNode) o2).getAttribute("link");
				boolean l1IsVisible = currentVisibleParents.contains(l1);
				boolean l2IsVisible = currentVisibleParents.contains(l2);
				if (l1IsVisible != l2IsVisible) {
					if (l1IsVisible)
						return 1;
					else
						return -1;
				} else
					return l1.getType().getID().compareTo(l2.getType().getID());
			} else
				return 0;
		}
	};

	public LinkoutMeter(LinkedObject lo, LinkDatabaseCanvas canvas,
			boolean children) {
		super();
		addInputEventListener(mouseoverListener);
		setCanvas(canvas);
		setVisible(true);
		setPickable(true);
		term = lo;
		this.children = children;
		expandAllButton = buildExpandButton();
		collapseAllButton = buildCollapseButton();
		closeButton = buildCloseButton();
		setHeight(LINK_HEIGHT);
		rebuildParentList();
	}

	protected long fadeupDuration = 500;

	protected void showButtons(boolean animate) {
		if (empty)
			return;
		if (currentButtonDisplayActivity != null)
			currentButtonDisplayActivity
					.terminate(PActivity.TERMINATE_WITHOUT_FINISHING);
		if (animate) {
			currentButtonDisplayActivity = new PCompoundActivity();
			if (expandAllButton.getTransparency() < 1)
				currentButtonDisplayActivity.addActivity(expandAllButton
						.animateToTransparency(1, getFadeupDuration()));
			if (collapseAllButton.getTransparency() < 1)
				currentButtonDisplayActivity.addActivity(collapseAllButton
						.animateToTransparency(1, getFadeupDuration()));
			if (closeButton.getTransparency() < 1)
				currentButtonDisplayActivity.addActivity(closeButton
						.animateToTransparency(1, getFadeupDuration()));
			canvas.getRoot().addActivity(currentButtonDisplayActivity);
		} else {
			expandAllButton.setTransparency(1);
			collapseAllButton.setTransparency(1);
			closeButton.setTransparency(1);
		}
	}

	protected void hideButtons(boolean animate) {
		if (empty)
			return;
		if (currentButtonDisplayActivity != null)
			currentButtonDisplayActivity
					.terminate(PActivity.TERMINATE_WITHOUT_FINISHING);
		if (animate) {
			currentButtonDisplayActivity = new PCompoundActivity();
			if (expandAllButton.getTransparency() > 0)
				currentButtonDisplayActivity.addActivity(expandAllButton
						.animateToTransparency(0, getFadeupDuration()));
			if (collapseAllButton.getTransparency() > 0)
				currentButtonDisplayActivity.addActivity(collapseAllButton
						.animateToTransparency(0, getFadeupDuration()));
			if (closeButton.getTransparency() > 0)
				currentButtonDisplayActivity.addActivity(closeButton
						.animateToTransparency(0, getFadeupDuration()));
			canvas.getRoot().addActivity(currentButtonDisplayActivity);
		} else {
			expandAllButton.setTransparency(0);
			collapseAllButton.setTransparency(0);
			closeButton.setTransparency(0);
		}
	}

	protected PPath buildExpandButton() {
		PPath expandAllButton = PPath.createRectangle(0, 0, LINK_HEIGHT,
				LINK_HEIGHT);
		expandAllButton.setPaint(buttonColor);
		expandAllButton.setStroke(lineStroke);
		expandAllButton.addInputEventListener(expandAllListener);
		expandAllButton.setTransparency(0);
		Shape s = getTriangle(LINK_HEIGHT - 2, !children);
		PPath icon = new PPath(s);
		icon.setPaint(Color.white);
		icon.setStroke(null);
		expandAllButton.addChild(icon);
		icon.setPickable(false);
		icon.setOffset(1, LINK_HEIGHT / 2 - 1);
		expandAllButton.addAttribute("tooltipFactory", SimpleTooltipFactory
				.getInstance());
		expandAllButton.addAttribute("tooltipText", "Expand all "
				+ (children ? "children" : "parents"));
		return expandAllButton;
	}

	protected PPath buildCollapseButton() {
		PPath collapseAllButton = PPath.createRectangle(0, 0, LINK_HEIGHT,
				LINK_HEIGHT);
		collapseAllButton.setPaint(buttonColor);
		collapseAllButton.setStroke(lineStroke);
		collapseAllButton.addInputEventListener(collapseAllListener);
		collapseAllButton.setTransparency(0);
		Shape s = getTriangle(LINK_HEIGHT - 2, children);
		PPath icon = new PPath(s);
		icon.setPaint(Color.white);
		icon.setStroke(null);
		collapseAllButton.addChild(icon);
		icon.setPickable(false);
		icon.setOffset(1, LINK_HEIGHT / 2 - 1);
		collapseAllButton.addAttribute("tooltipFactory", SimpleTooltipFactory
				.getInstance());
		collapseAllButton.addAttribute("tooltipText", "Collapse all "
				+ (children ? "children" : "parents"));
		return collapseAllButton;
	}

	protected PPath buildCloseButton() {
		GeneralPath semicircle = new GeneralPath();
		semicircle.moveTo(0, LINK_HEIGHT);
		semicircle.lineTo(LINK_HEIGHT * 2, LINK_HEIGHT);
		semicircle.curveTo(LINK_HEIGHT * 2, LINK_HEIGHT / 2, LINK_HEIGHT
				+ LINK_HEIGHT / 2, 0, LINK_HEIGHT, 0);
		semicircle.curveTo(LINK_HEIGHT - LINK_HEIGHT / 2, 0, 0,
				LINK_HEIGHT / 2, 0, LINK_HEIGHT);
		semicircle.closePath();
		if (!children) {
			semicircle.transform(AffineTransform.getRotateInstance(Math.PI,
					LINK_HEIGHT, LINK_HEIGHT / 2));
		}
		PPath closeButton = new PPath(semicircle);
		closeButton.setPaint(Color.red);
		closeButton.setStroke(lineStroke);
		closeButton.setTransparency(0);
		closeButton.addInputEventListener(closeListener);
		Shape s = getPlusShape(LINK_HEIGHT - 2);
		PPath plusPath = new PPath(s);
		plusPath.setStroke(null);
		plusPath.setPaint(Color.white);
		plusPath.setPickable(false);
		closeButton.addChild(plusPath);
		// PiccoloUtil.centerInParent(plusPath, true, true);
		plusPath.setOffset(1 + LINK_HEIGHT / 2, 1);
		closeButton.addAttribute("tooltipFactory", SimpleTooltipFactory
				.getInstance());
		closeButton.addAttribute("tooltipText", "Hide only this term");
		return closeButton;
	}

	protected Shape getTriangle(float size, boolean up) {
		GeneralPath s = new GeneralPath();
		s.moveTo(0, 0);
		s.lineTo(10, 0);
		s.lineTo(5, 5);
		s.closePath();
		AffineTransform t = new AffineTransform();
		if (up)
			t.rotate(Math.PI);
		double scale = size / s.getBounds2D().getWidth();
		t.scale(scale, scale);
		s.transform(t);
		ShapeUtil.normalize((Shape) s.clone(), s);
		return s;
	}

	protected Shape getPlusShape(float size) {
		GeneralPath s = new GeneralPath();
		s.moveTo(2, 0);
		s.lineTo(5, 3);
		s.lineTo(8, 0);
		s.lineTo(10, 2);
		s.lineTo(7, 5);
		s.lineTo(10, 8);
		s.lineTo(8, 10);
		s.lineTo(5, 7);
		s.lineTo(2, 10);
		s.lineTo(0, 8);
		s.lineTo(3, 5);
		s.lineTo(0, 2);
		s.closePath();
		double scale = size / s.getBounds2D().getHeight();
		return s.createTransformedShape(AffineTransform.getScaleInstance(scale,
				scale));
	}

	protected PInputEventListener inputListener = new PBasicInputEventHandler() {

		public void mouseClicked(PInputEvent event) {
			PNode linkBar = event.getPickedNode();
			Link link = (Link) linkBar.getAttribute("link");
			boolean newState = !getVisibleParents(term).contains(link);
			if (children)
				collapsibleLinkDatabase.setVisible(link.getChild(), newState);
			else
				collapsibleLinkDatabase.setVisible(link.getParent(), newState);
		}
	};

	protected LinkDatabaseCanvas canvas;

	public void setCanvas(LinkDatabaseCanvas canvas) {
		this.canvas = canvas;
		collapsibleLinkDatabase = canvas.getCollapsibleLinkDatabase();
		collapsibleLinkDatabase.addListener(expandListener);

		//setCamera(canvas.getCamera());
	}

	protected void rebuildParentList() {
		empty = false;
		removeAllChildren();
		parentList = new ArrayList();
		Collection relIt;
		if (children)
			relIt = canvas.getLinkProviderDatabase().getChildren(term);
		else
			relIt = canvas.getLinkProviderDatabase().getParents(term);
		if (relIt.isEmpty()) {
			empty = true;
			return;
		}
		List<Link> linkList = new LinkedList<Link>();
		Iterator it = relIt.iterator();
		while (it.hasNext()) {
			Link link = (Link) it.next();
			if (TermUtil.isIntersection(link))
				continue;
			linkList.add(link);
		}
		Set visibleParents = getVisibleParents(term);
		for(Link link : linkList) {
			PPath linkBar = PPath.createRectangle(0, 0, getLinkWidth(),
					LINK_HEIGHT);
			linkBar.addAttribute("tooltipFactory", tooltipFactory);
			linkBar.addAttribute("link", link);
			linkBar.setStroke(lineStroke);
			Paint paint = getColor(link, visibleParents.contains(link));

			addChild(linkBar);
			linkBar.addInputEventListener(inputListener);

			linkBar.setPaint(paint);
			parentList.add(linkBar);
		}
		addChild(expandAllButton);
		addChild(collapseAllButton);
		addChild(closeButton);
		sortAndRecolor(false);
		// setWidth(parentList.size() * getLinkWidth());
	}

	@Override
	public boolean setBounds(double x, double y, double width, double height) {
		// TODO Auto-generated method stub
		return super.setBounds(x, y, width, height);
	}

	public void dispose() {
		collapsibleLinkDatabase.removeListener(expandListener);
	}

	protected float getLinkWidth() {
		PNode parent = getParent();
		if (parent == null)
			return LINK_WIDTH;
		return (float) Math.min(LINK_WIDTH, parent.getWidth()
				/ Math.max(1, parentList.size()));
	}

	protected Color getColor(Link link, boolean isVisible) {
		Color c = (Color) canvas.getColorManager().getColor(link.getType());
		if (isVisible) {
			c = new Color(c.getRed(), c.getBlue(), c.getGreen(), 40);
		}
		return c;
	}

	protected PCompoundActivity linkBarSortActivity = new PCompoundActivity();

	public void sortAndRecolor(boolean animate) {
		if (empty)
			return;
		if (linkBarSortActivity != null) {
			linkBarSortActivity
					.terminate(PActivity.TERMINATE_WITHOUT_FINISHING);
			linkBarSortActivity = new PCompoundActivity();
		}
		double yoffset = 0;
		/*
		 * if (children) yoffset = closeButton.getHeight();
		 */
		expandAllButton.setOffset(-expandAllButton.getWidth(), yoffset);
		collapseAllButton
				.setOffset(parentList.size() * getLinkWidth(), yoffset);
		closeButton.setOffset((parentList.size() * getLinkWidth() - closeButton
				.getWidth()) / 2, (children ? -LINK_HEIGHT : LINK_HEIGHT));

		currentVisibleParents = getVisibleParents(term);
		Collections.sort(parentList, typeComparator);
		for (int i = 0; i < parentList.size(); i++) {
			PPath linkBar = (PPath) parentList.get(i);
			double xoffset = i * getLinkWidth();
			Link link = (Link) linkBar.getAttribute("link");
			Color paint = getColor(link, currentVisibleParents.contains(link));

			if (animate) {
				if (xoffset != linkBar.getXOffset()) {
					linkBar.moveToFront();
					linkBar.setOffset(xoffset, yoffset);
					/*
					linkBarSortActivity.addActivity(linkBar
							.animateToPositionScaleRotation(xoffset, yoffset,
									1, 0, getFadeupDuration()));
									*/
				}
				
/*
				linkBarSortActivity.addActivity(linkBar.animateToColor(paint,
						getFadeupDuration()));
				canvas.getRoot().addActivity(linkBarSortActivity);
*/
				linkBar.setPaint(paint);
			} else {
				linkBar.setOffset(xoffset, yoffset);
				linkBar.setPaint(paint);
			}
		}
	}

	protected Stroke lineStroke = new BasicStroke(1.0f);

	protected Set getVisibleParents(LinkedObject lo) {
		Set visibleParents = new HashSet();
		if (children)
			visibleParents.addAll(collapsibleLinkDatabase.getChildren(term));
		else
			visibleParents.addAll(collapsibleLinkDatabase.getParents(term));
		return visibleParents;
	}

	public long getFadeupDuration() {
		return fadeupDuration;
	}

	public void setFadeupDuration(long fadeupDuration) {
		this.fadeupDuration = fadeupDuration;
	}
}
