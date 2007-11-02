package org.oboedit.graph;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Composite;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.geom.AffineTransform;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;

import org.bbop.swing.BackgroundEventQueue;
import org.bbop.swing.ShapeUtil;
import org.bbop.util.AbstractTaskDelegate;
import org.bbop.util.ObjectUtil;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.PathCapable;
import org.obo.util.TermUtil;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.gui.event.HistoryAppliedEvent;
import org.oboedit.gui.event.HistoryListener;
import org.oboedit.piccolo.PiccoloUtil;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;
import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEvent;
import edu.umd.cs.piccolo.event.PInputEventListener;
import edu.umd.cs.piccolo.nodes.PPath;
import edu.umd.cs.piccolo.util.PPickPath;

public class LinkButtonBehavior implements ViewBehavior {

	public static enum ButtonLocations {
		EXPAND_PARENTS(true, false, false), EXPAND_CHILDREN(true, true, false), COLLAPSE_PARENTS(
				false, false, false), COLLAPSE_CHILDREN(false, true, false), CLOSE(
				false, false, true);

		protected int height = 30;

		protected int width = 30;

		protected boolean expand;

		protected boolean children;

		protected boolean close;

		private ButtonLocations(boolean expand, boolean children, boolean close) {
			this.expand = expand;
			this.children = children;
			this.close = close;
		}

		public int getWidth() {
			return width;
		}

		public int getHeight() {
			return height;
		}

		public boolean isExpand() {
			return expand;
		}

		public boolean isChildren() {
			return children;
		}

		public boolean isClose() {
			return close;
		}
	};

	protected LinkDatabaseCanvas canvas;

	protected OENode currentNode;

	protected BackgroundEventQueue queue;

	public LinkButtonBehavior() {
		queue = new BackgroundEventQueue();
	}

	public void install(LinkDatabaseCanvas canvas) {
		this.canvas = canvas;
		canvas.addInputEventListener(new PBasicInputEventHandler() {
			@Override
			public void mouseMoved(PInputEvent event) {
				if (LinkButtonBehavior.this.canvas.isLayingOut())
					return;
				OENode oldNode = currentNode;
				currentNode = PiccoloUtil.getNodeOfClass(event.getPath(),
						OENode.class);
				if (!ObjectUtil.equals(oldNode, currentNode)) {
					queue.cancelAll();
					if (oldNode != null) {
						removeButtons(oldNode);
					}
					if (currentNode != null) {
						addButtons(currentNode);
					}
				}
				super.mouseMoved(event);
			}
		});
	}

	public void uninstall(LinkDatabaseCanvas canvas) {
		this.canvas = null;
	}

	public void removeButtons(OENode oenode) {
		for (ButtonLocations loc : ButtonLocations.values()) {
			oenode.setNamedChild(loc, null);
		}

	}

	public void addButtons(OENode oenode) {
		for (ButtonLocations loc : ButtonLocations.values()) {
			queue.scheduleTask(new ButtonPlacementTask(loc, oenode));
		}
	}

	protected static Shape getPlusShape(float size) {
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

	protected PBasicInputEventHandler buttonListener = new PBasicInputEventHandler() {
		@Override
		public void mouseClicked(PInputEvent event) {

			PPath button = (PPath) event.getPickedNode();
			LinkedObject lo = (LinkedObject) button.getAttribute("node");
			final ButtonLocations loc = (ButtonLocations) button
					.getAttribute("buttonType");
			if (loc.isClose()) {
				if (event.isLeftMouseButton())
					canvas.removeVisibleObjects(Collections.singleton(lo));
				return;
			}

			if (event.isRightMouseButton()) {
				MouseEvent me = (MouseEvent) event.getSourceSwingEvent();
				JPanel panel = new JPanel() {

					@Override
					public void paint(Graphics g) {
						super.paint(g);
					}

					@Override
					protected void paintChildren(Graphics g) {
						super.paintChildren(g);
					}

					public void paintComponent(Graphics g) {
						Graphics2D g2 = (Graphics2D) g;
						Composite c = g2.getComposite();
						g2.setComposite(AlphaComposite.getInstance(
								AlphaComposite.SRC_OVER, .8f));
						super.paintComponent(g);
						g2.setComposite(c);
					}
				};
				panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
				List<Link> showThese = new ArrayList<Link>(getLinksToShow(lo,
						loc.isChildren()));
				Collections.sort(showThese, new Comparator<Link>() {

					public int compare(Link o1, Link o2) {
						LinkedObject lo1;
						LinkedObject lo2;
						if (loc.isChildren()) {
							lo1 = o1.getChild();
							lo2 = o2.getChild();
						} else {
							lo1 = o1.getParent();
							lo2 = o2.getParent();
						}
						return lo1.getName().compareToIgnoreCase(lo2.getName());
					}

				});
				for (Link link : showThese) {
					LinkedObject obj;
					if (loc.isChildren())
						obj = link.getChild();
					else
						obj = link.getParent();
					final LinkedObject finalObj = obj;
					final JCheckBox checkBox = new JCheckBox(finalObj
							.toString()
							+ " (via " + link.getType() + ")", canvas
							.getVisibleObjects().contains(link));
					checkBox.setOpaque(false);
					checkBox.addActionListener(new ActionListener() {

						public void actionPerformed(ActionEvent e) {
							if (checkBox.isSelected()) {
								canvas.addVisibleObjects(Collections
										.singleton(finalObj));
							} else
								canvas.removeVisibleObjects(Collections
										.singleton(finalObj));
						}
					});
					panel.add(checkBox);
				}
				canvas.popupInFrame(panel, "Select "
						+ (loc.isChildren() ? "child" : "parent")
						+ " nodes to display", me.getX(), me.getY());

			} else if (loc.isExpand()) {
				canvas.addVisibleObjects(getLinksToShow(lo, loc.isChildren()));
			} else {
				Collection<LinkedObject> removeUs = new LinkedList<LinkedObject>();
				if (loc.isChildren()) {
					// Collection<Link> links
					removeUs = TermUtil.getDescendants(lo, canvas
							.getLinkDatabase(), false);
					/*
					 * for (Link link : links) { removeUs.add(link.getChild()); }
					 */
				} else {
					removeUs = TermUtil.getAncestors(lo, canvas
							.getLinkDatabase(), false);
					/*
					 * Collection<Link> links =
					 * canvas.getLinkProviderDatabase() .getParents(lo); for
					 * (Link link : links) { removeUs.add(link.getParent()); }
					 */
				}
				canvas.removeVisibleObjects(removeUs);

			}
		}
	};

	protected Collection<Link> getLinksToShow(LinkedObject lo,
			boolean isChildren) {
		Collection<Link> showThese;
		if (isChildren)
			showThese = canvas.getLinkProviderDatabase().getChildren(lo);
		else
			showThese = canvas.getLinkProviderDatabase().getParents(lo);
		return showThese;
	}

	protected class ButtonPlacementTask extends AbstractTaskDelegate<Void> {

		protected ButtonLocations loc;

		protected OENode oenode;

		public ButtonPlacementTask(ButtonLocations loc, OENode node) {
			this.loc = loc;
			this.oenode = node;
		}

		@Override
		public void execute() throws Exception {
			int buttonSize = 12;
			int width = buttonSize;
			int height = buttonSize;
			final PPath button = new PPath(new Ellipse2D.Double(0, 0, width,
					height));
			button.addAttribute(PiccoloUtil.NO_RIGHT_CLICK_MENU, true);
			Shape iconShape;
			if (!loc.isClose()) {
				iconShape = getTriangle(height * 2 / 3, loc.isChildren() != loc
						.isExpand());
			} else {
				iconShape = getPlusShape(height * 2 / 3);
			}
			PPath icon = new PPath(iconShape);
			icon.setPaint(Color.white);
			icon.setStroke(null);
			button.addChild(icon);
			icon.setPickable(false);
			PiccoloUtil.centerInParent(icon, true, true);
			button.addAttribute(TooltipFactory.KEY, SimpleTooltipFactory
					.getInstance());
			button.addAttribute("buttonType", loc);
			button.addAttribute("node", oenode.getObject());
			button.addInputEventListener(buttonListener);
			button.setStroke(null);
			button.setPaint(Color.blue);
			button.setTransparency(.5f);
			if (!loc.isClose()) {
				boolean grayedOut = true;
				Collection<LinkedObject> expandThese = new LinkedList<LinkedObject>();
				Collection<Link> links = null;

				// boolean tooManyLinks = canvas.getReasoner() != null
				// && ((loc.isChildren() && canvas.getReasoner()
				// .getChildren((LinkedObject) oenode.getObject())
				// .size() > 100) || (!loc.isChildren() && canvas
				// .getReasoner().getParents(
				// (LinkedObject) oenode.getObject())
				// .size() > 100));
				boolean tooManyLinks = false;
				if (!tooManyLinks) {
					if (loc.isChildren()) {
						if (isCancelled())
							return;
						links = canvas.getLinkProviderDatabase().getChildren(
								(LinkedObject) oenode.getObject());
						if (links.isEmpty()) {
							button.setVisible(false);
							return;
						}
						for (Link link : links) {
							expandThese.add(link.getChild());
						}
					} else {
						if (isCancelled())
							return;
						links = canvas.getLinkProviderDatabase().getParents(
								(LinkedObject) oenode.getObject());
						if (links.isEmpty()) {
							button.setVisible(false);
							return;
						}
						for (Link link : links) {
							expandThese.add(link.getParent());
						}
					}
				}
				Collection<PathCapable> visible = canvas.getVisibleObjects();
				if (loc.isExpand()) {
					Iterator<LinkedObject> it = expandThese.iterator();
					while (it.hasNext()) {
						LinkedObject lo = it.next();
						if (isCancelled())
							return;
						if (visible.contains(lo))
							it.remove();
					}
				} else {
					Iterator<LinkedObject> it = expandThese.iterator();
					while (it.hasNext()) {
						LinkedObject lo = it.next();
						if (isCancelled())
							return;
						if (!visible.contains(lo))
							it.remove();
					}
				}
				if (!tooManyLinks && expandThese.isEmpty())
					button.setPaint(Color.gray);
				else {
					String connectingWord;
					if (tooManyLinks) {
						connectingWord = " many ";
					} else if (expandThese.size() == links.size()) {
						if (expandThese.size() == 1)
							connectingWord = " 1 ";
						else if (expandThese.size() == 2)
							connectingWord = " both ";
						else
							connectingWord = " all " + expandThese.size() + " ";
					} else if (loc.isExpand()) {
						connectingWord = " " + expandThese.size()
								+ " hidden (of " + links.size() + " total) ";
					} else {
						connectingWord = " " + expandThese.size()
								+ " visible (of " + links.size() + " total) ";
					}
					String tooltip;
					if (tooManyLinks)
						tooltip = (loc.isExpand() ? "Expand" : "Collapse")
								+ " "
								+ (loc.isChildren() ? "children" : "parents");
					else
						tooltip = (loc.isExpand() ? "Expand" : "Collapse")
								+ connectingWord
								+ (loc.isChildren() ? (expandThese.size() == 1 ? "child"
										: "children")
										: (expandThese.size() == 1 ? "parent"
												: "parents"));
					button.addAttribute(TooltipFactory.TEXT_KEY, tooltip);

				}
			}

			if (loc.isClose()) {
				button.setOffset((oenode.getWidth() - width) / 2, oenode
						.getHeight()
						- height);
				button.setPaint(Color.red);
				button.addAttribute(TooltipFactory.TEXT_KEY,
						"Hide only this term");
			} else {
				if (loc.isChildren())
					button.setOffset(0, oenode.getHeight() - height);
				else
					button.setOffset(0, 0);
				if (loc.isExpand())
					button.setOffset(oenode.getWidth() - width, button
							.getYOffset());
			}
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					if (!isCancelled())
						oenode.setNamedChild(loc, button);
				}
			});
		}
	}

	protected void placeButton(ButtonLocations loc, final OENode oenode) {
		// int buttonSize = (int) (Math.min(oenode.getFullBoundsReference()
		// .getWidth(), oenode.getFullBoundsReference().getHeight()) / 3);
	}

	protected static Shape getTriangle(float size, boolean up) {
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

	public boolean onlyDecorateAfterLayout() {
		return true;
	}
}
