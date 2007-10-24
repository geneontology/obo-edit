package org.oboedit.graph;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Cursor;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.geom.Line2D;
import java.awt.geom.Point2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedList;

import javax.swing.Icon;
import javax.swing.JComboBox;
import javax.swing.JComponent;

import org.bbop.util.ObjectUtil;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.OBOProperty;
import org.obo.datamodel.PathCapable;
import org.obo.datamodel.RootAlgorithm;
import org.obo.datamodel.impl.DanglingLinkImpl;
import org.obo.datamodel.impl.OBORestrictionImpl;
import org.obo.util.TermUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.GestureTarget;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.Selection;
import org.oboedit.gui.actions.CopyAction;
import org.oboedit.gui.actions.SpecificCopyAction;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.piccolo.PiccoloUtil;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.event.PBasicInputEventHandler;
import edu.umd.cs.piccolo.event.PInputEvent;
import edu.umd.cs.piccolo.event.PInputEventListener;
import edu.umd.cs.piccolo.event.PPanEventHandler;
import edu.umd.cs.piccolo.nodes.PPath;
import edu.umd.cs.piccolo.util.PPickPath;

public class LinkingButtonBehavior implements ToolbarButtonBehavior {

	protected static final Object ARROW_GHOST_KEY = new Object();

	public static final long PAN_DURATION = 1000;

	protected LinkDatabaseCanvas canvas;

	protected OBOProperty property;

	protected Collection<Link> dummyLinks = new LinkedList<Link>();

	protected SpecificCopyAction copyAction;

	protected PInputEventListener mouseListener = new PBasicInputEventHandler() {

		protected Collection<OENode> originNodes = new LinkedList<OENode>();

		protected PPanEventHandler panHandler = null;

		protected Line2D line = new Line2D.Double();

		protected Selection tempSelection;

		protected GestureTarget tempTarget;

		protected OENode destNode;

		@Override
		public void mousePressed(PInputEvent event) {
			if (event.isLeftMouseButton()) {
				panHandler = canvas.getPanEventHandler();
				OENode originNode = (OENode) PiccoloUtil.getNodeOfClass(event
						.getPath(), OENode.class);
				dummyLinks.clear();
				originNodes.clear();
				if (originNode != null) {
					if (canvas.getSelection().getTerms().contains(
							originNode.getObject())) {
						for (LinkedObject lo : canvas.getSelection().getTerms()) {
							OENode node = (OENode) canvas.getNode(lo);
							originNodes.add(node);
						}
						tempSelection = canvas.getSelection();
					} else {
						originNodes.add(originNode);
						tempSelection = SelectionManager.createSelection(
								canvas, originNode.getObject(),
								RootAlgorithm.GREEDY, canvas
										.getLinkProviderDatabase());
					}
					canvas.setPanEventHandler(null);
					for (OENode node : originNodes) {
						Link dummyLink = new OBORestrictionImpl((OBOClass) node
								.getObject(), property, (OBOClass) null);
						dummyLinks.add(dummyLink);
					}
				}
			}
		}

		@Override
		public void mouseDragged(PInputEvent event) {
			PNode arrowGhosts = new PNode();
			if (event.isLeftMouseButton() && originNodes.size() > 0) {
				Point2D dragPos = event.getPosition();
				for (Link dummyLink : dummyLinks) {
					OENode originNode = (OENode) canvas.getNode(dummyLink
							.getChild());
					Point2D startPoint = originNode.getFullBoundsReference()
							.getCenter2D();
					line.setLine(startPoint, dragPos);
					PPath arrowGhost = new OELink(dummyLink, canvas
							.getIconManager(), canvas.getColorManager(), canvas
							.getNamedChildProvider(), line);
					arrowGhost.setPickable(false);
					arrowGhost.setChildrenPickable(false);
					MouseEvent me = (MouseEvent) event.getSourceSwingEvent();
					if (!canvas.getCamera().getViewBounds().contains(dragPos))
						canvas.getCamera().animateViewToCenterBounds(
								new Rectangle2D.Double(dragPos.getX(), dragPos
										.getY(), 1, 1), false, PAN_DURATION);
					OENode thisDestNode = (OENode) PiccoloUtil.getNodeOfClass(
							canvas.getCamera().pick(me.getX(), me.getY(), 1),
							OENode.class);

					if (!ObjectUtil.equals(thisDestNode, destNode)) {
						this.destNode = thisDestNode;
						if (destNode != null) {
							tempTarget = SelectionManager.createGestureTarget(
									canvas, canvas.getLinkDatabase(), canvas
											.getRootAlgorithm(), destNode
											.getObject());
							copyAction.clickInit(tempSelection, tempTarget);
						}
					}
					if (thisDestNode == null || !copyAction.isLegal()) {
						arrowGhost.setTransparency(.5f);
					}
					arrowGhosts.addChild(arrowGhost);
				}
				NamedChildProvider provider = canvas.getNamedChildProvider();
				provider.setNamedChild(ARROW_GHOST_KEY, canvas.getLayer(),
						arrowGhosts);
			}
		}

		@Override
		public void mouseEntered(PInputEvent event) {
			OENode node = (OENode) PiccoloUtil.getNodeOfClass(event.getPath(),
					OENode.class);
			if (node != null) {
				canvas.setCursor(Cursor
						.getPredefinedCursor(Cursor.CROSSHAIR_CURSOR));
			}
		}

		@Override
		public void mouseExited(PInputEvent event) {
			canvas.setCursor(Cursor.getDefaultCursor());
		}

		@Override
		public void mouseReleased(PInputEvent event) {
			if (event.isLeftMouseButton() && originNodes.size() > 0) {
				NamedChildProvider provider = canvas.getNamedChildProvider();
				PNode arrowGhost = (PNode) provider.getNamedChild(
						ARROW_GHOST_KEY, canvas.getLayer());
				if (arrowGhost != null) {
					provider.setNamedChild(ARROW_GHOST_KEY, canvas.getLayer(),
							null);
					canvas.decorate();
				}
				canvas.setPanEventHandler(panHandler);
				if (destNode != null && copyAction.isLegal()) {
					SessionManager.getManager().apply(copyAction.execute());
				}
				originNodes.clear();
				destNode = null;
				dummyLinks.clear();
			}
		}
	};

	public LinkingButtonBehavior() {
		setProperty(OBOProperty.IS_A);
	}

	public void activate(LinkDatabaseCanvas canvas) {
		this.canvas = canvas;
		canvas.addInputEventListener(mouseListener);
	}

	public void deactivate(LinkDatabaseCanvas canvas) {
		this.canvas.removeInputEventListener(mouseListener);
		this.canvas = null;
	}

	public Icon getButtonIcon() {
		return Preferences.loadLibraryIcon("draw_link.gif");
	}

	public String getButtonLabel() {
		return null;
	}

	public JComponent getConfigurationPanel() {
		final JComboBox typeSelector = new JComboBox();
		for (OBOProperty type : TermUtil.getRelationshipTypes(SessionManager
				.getManager().getSession())) {
			typeSelector.addItem(type);
		}
		typeSelector.setSelectedItem(property);
		typeSelector.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setProperty((OBOProperty) typeSelector.getSelectedItem());
			}
		});
		return typeSelector;
	}

	protected void setProperty(OBOProperty property) {
		this.property = property;
		copyAction = new SpecificCopyAction(property, false);
		copyAction.setCopyChild(true);
	}

	public String getTooltip() {
		return "<html><b>Link creation tool</b><br><hr>\n"
				+ "<table width=300><tr><td>Click and drag to create new links between terms. "
				+ "If a group of terms is selected and the drag is "
				+ "initiated from one of the selected terms, links "
				+ "will be added between all selected terms and the target term."
				+ "</td></table></html>";
	}
}
