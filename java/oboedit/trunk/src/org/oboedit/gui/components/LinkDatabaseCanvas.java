package org.oboedit.gui.components;

import java.awt.AlphaComposite;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Composite;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.Image;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetListener;
import java.awt.event.ActionListener;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JInternalFrame;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JViewport;
import javax.swing.SwingUtilities;
import javax.swing.Timer;
import javax.swing.event.InternalFrameAdapter;
import javax.swing.event.InternalFrameEvent;
import javax.swing.event.InternalFrameListener;
import javax.swing.tree.TreePath;

import org.bbop.framework.GUIManager;
import org.bbop.swing.DropTargetListenerMulticaster;
import org.bbop.swing.FocusHierarchyListener;
import org.bbop.swing.FocusHierarchyManager;
import org.bbop.util.AbstractTaskDelegate;
import org.bbop.util.TaskDelegate;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.Link;
import org.obo.datamodel.LinkDatabase;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.PathCapable;
import org.obo.datamodel.RootAlgorithm;
import org.obo.datamodel.impl.FilteredLinkDatabase;
import org.obo.filters.Filter;
import org.obo.filters.FilterPair;
import org.obo.reasoner.ReasonedLinkDatabase;
import org.obo.reasoner.impl.TrimmedLinkDatabase;
import org.oboedit.controller.FilterManager;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.graph.CollapsibleLinkDatabase;
import org.oboedit.graph.DefaultNodeFactory;
import org.oboedit.graph.DefaultTypeColorManager;
import org.oboedit.graph.ExpandCollapseListener;
import org.oboedit.graph.ExpansionEvent;
import org.oboedit.graph.FocusedNodeListener;
import org.oboedit.graph.GraphLayout;
import org.oboedit.graph.LinkDatabaseLayoutEngine;
import org.oboedit.graph.NamedChildProvider;
import org.oboedit.graph.NodeDecorator;
import org.oboedit.graph.NodeSizeProvider;
import org.oboedit.graph.OELink;
import org.oboedit.graph.OENode;
import org.oboedit.graph.PCNode;
import org.oboedit.graph.RelayoutListener;
import org.oboedit.graph.RightClickMenuBehavior;
import org.oboedit.graph.RightClickMenuFactory;
import org.oboedit.graph.SingleCameraPanHandler;
import org.oboedit.graph.TypeColorManager;
import org.oboedit.graph.TypeIconManager;
import org.oboedit.graph.ViewBehavior;
import org.oboedit.gui.Filterable;
import org.oboedit.gui.GestureTarget;
import org.oboedit.gui.ObjectSelector;
import org.oboedit.gui.RightClickMenuProvider;
import org.oboedit.gui.Selection;
import org.oboedit.gui.event.SelectionEvent;
import org.oboedit.gui.event.SelectionListener;
import org.oboedit.piccolo.ExtensibleCanvas;
import org.oboedit.piccolo.ExtensibleRoot;
import org.oboedit.piccolo.FullPaintCamera;
import org.oboedit.piccolo.NamedChildMorpher;
import org.oboedit.piccolo.PCompoundActivity;
import org.oboedit.piccolo.PiccoloUtil;
import org.oboedit.util.PathUtil;

import edu.umd.cs.piccolo.PCamera;
import edu.umd.cs.piccolo.PCanvas;
import edu.umd.cs.piccolo.PInputManager;
import edu.umd.cs.piccolo.PLayer;
import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.PRoot;
import edu.umd.cs.piccolo.PRoot.InputSource;
import edu.umd.cs.piccolo.activities.PActivity;
import edu.umd.cs.piccolo.activities.PActivityScheduler;
import edu.umd.cs.piccolo.activities.PInterpolatingActivity;
import edu.umd.cs.piccolo.activities.PActivity.PActivityDelegate;
import edu.umd.cs.piccolo.event.PInputEvent;
import edu.umd.cs.piccolo.util.PBounds;
import edu.umd.cs.piccolo.util.PDebug;
import edu.umd.cs.piccolo.util.PNodeFilter;
import edu.umd.cs.piccolo.util.PPickPath;

public class LinkDatabaseCanvas extends ExtensibleCanvas implements
		ObjectSelector, RightClickMenuProvider, Filterable {

	/**
	 * 
	 */
	private static final long serialVersionUID = 3863061306003913893L;

	protected static final Object CURRENT_DECORATOR_ANIMATIONS = new Object();

	public static final long DEFAULT_LAYOUT_DURATION = 1000;

	protected long layoutDuration = new Long(System.getProperty(
			"LAYOUT_DURATION", DEFAULT_LAYOUT_DURATION + ""));

	protected LinkDatabaseLayoutEngine layoutEngine = new LinkDatabaseLayoutEngine();

	protected CollapsibleLinkDatabase linkDatabase;

	protected FilteredLinkDatabase filteredLinkDatabase;

	protected NamedChildMorpher morpher = new NamedChildMorpher();

	protected Collection<ExpandCollapseListener> expansionListeners = new LinkedList<ExpandCollapseListener>();

	protected Collection<SelectionListener> selectionListeners = new LinkedList<SelectionListener>();

	protected DefaultNodeFactory nodeFactory = new DefaultNodeFactory();

	protected LinkDatabase linkProviderDatabase;

	protected Collection<NodeDecorator> decorators = new LinkedList<NodeDecorator>();

	protected List<ViewBehavior> viewBehaviors = new LinkedList<ViewBehavior>();

	protected Collection<RelayoutListener> layoutListeners = new LinkedList<RelayoutListener>();

	protected Collection<FocusedNodeListener> focusedNodeListeners = new LinkedList<FocusedNodeListener>();

	protected RightClickMenuBehavior rightClickBehavior = new RightClickMenuBehavior();

	protected JPanel placementPanel = new JPanel();

	protected boolean isLayingOut = false;

	protected PActivity relayoutActivity;

	protected PNode newLayer;

	protected boolean isLive = true;

	protected Selection selection = SelectionManager.createEmptySelection(this);

	protected SessionManager sessionManager = SessionManager.getManager();

	protected DropTargetListenerMulticaster dropMulticaster = new DropTargetListenerMulticaster();

	protected DropTarget dropTarget = new DropTarget(this, dropMulticaster);

	protected LinkedList<Runnable> postLayoutQueue = new LinkedList<Runnable>();

	protected boolean disableAnimations = false;

	protected ExpandCollapseListener expandCollapseListener = new ExpandCollapseListener() {
		public void expandStateChanged(ExpansionEvent e) {
			relayout();
		}
	};

	public void addFocusedNodeListener(FocusedNodeListener listener) {
		focusedNodeListeners.add(listener);
	}

	public void removeFocusedNodeListener(FocusedNodeListener listener) {
		focusedNodeListeners.remove(listener);
	}

	protected void fireFocusedNodeChanged(PathCapable oldNode,
			PathCapable newNode) {
		for (FocusedNodeListener listener : focusedNodeListeners) {
			listener.focusedChanged(oldNode, newNode);
		}
	}

	public void addRelayoutListener(RelayoutListener listener) {
		layoutListeners.add(listener);
	}

	public void removeRelayoutListener(RelayoutListener listener) {
		layoutListeners.remove(listener);
	}

	protected void fireRelayoutCompleteEvent() {
		for (RelayoutListener listener : new LinkedList<RelayoutListener>(
				layoutListeners))
			listener.relayoutComplete();
	}

	protected void fireRelayoutStartingEvent() {
		for (RelayoutListener listener : new LinkedList<RelayoutListener>(
				layoutListeners))
			listener.relayoutStarting();
	}

	public int getInitialNodeWidth() {
		return layoutEngine.getInitialNodeWidth();
	}

	public int getInitialNodeHeight() {
		return layoutEngine.getInitialNodeHeight();
	}

	public void setInitialNodeWidth(int width) {
		layoutEngine.setInitialNodeWidth(width);
	}

	public void setInitialNodeHeight(int height) {
		layoutEngine.setInitialNodeHeight(height);
	}

	public void addViewBehavior(ViewBehavior viewBehavior) {
		viewBehaviors.add(viewBehavior);
		viewBehavior.install(this);
	}

	protected JInternalFrame internalFrame;
	protected FocusHierarchyListener internalFrameFocusListener = new FocusHierarchyListener() {

		public void focusGained(FocusEvent e) {
		}

		public void focusLost(FocusEvent e) {
			destroyPopupFrame();
		}

	};

	public void destroyPopupFrame() {
		if (internalFrame != null) {
			FocusHierarchyManager.removeFocusHierarchyListener(internalFrame,
					internalFrameFocusListener);
			remove(internalFrame);
			internalFrame.dispose();
			internalFrame = null;
			repaint();
		}
	}

	public void popupInFrame(JComponent component, String title, int x, int y) {
		destroyPopupFrame();

		internalFrame = new JInternalFrame(title, true, true, false, false) {
			/*
			 * @Override public void paint(Graphics g) { Graphics2D g2 =
			 * (Graphics2D) g; Composite c = g2.getComposite();
			 * g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER,
			 * .5f)); super.paint(g); g2.setComposite(c); paintChildren(g); }
			 */
		};
		internalFrame
				.setDefaultCloseOperation(JInternalFrame.DO_NOTHING_ON_CLOSE);
		internalFrame.addInternalFrameListener(new InternalFrameAdapter() {

			public void internalFrameClosing(InternalFrameEvent e) {
				destroyPopupFrame();
			}
		});
		FocusHierarchyManager.addFocusHierarchyListener(internalFrame,
				internalFrameFocusListener);
		JScrollPane pane = new JScrollPane(
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		pane.getViewport().setScrollMode(JViewport.SIMPLE_SCROLL_MODE);
		pane.setViewportView(component);
		internalFrame.getRootPane().setOpaque(false);
		pane.setOpaque(false);
		pane.setCorner(JScrollPane.LOWER_RIGHT_CORNER, new JPanel());
		pane.getViewport().setOpaque(false);
		internalFrame.setContentPane(pane);
		internalFrame.pack();
		Dimension d = new Dimension(Math.min(internalFrame.getWidth(),
				getWidth()), Math.min(internalFrame.getHeight(), getHeight()));
		internalFrame.setSize(d);
		if (x + d.getWidth() > getWidth())
			x -= (x + d.getWidth()) - getWidth();
		if (y + d.getHeight() > getHeight())
			y -= (y + d.getHeight()) - getHeight();
		internalFrame.setLocation(x, y);
		add(internalFrame);
		internalFrame.setVisible(true);
	}

	public void removeViewBehavior(ViewBehavior viewBehavior) {
		viewBehaviors.remove(viewBehavior);
		viewBehavior.uninstall(this);
	}

	public List<ViewBehavior> getViewBehaviors() {
		return viewBehaviors;
	}

	public void addSizeProvider(NodeSizeProvider provider) {
		layoutEngine.addSizeProvider(provider);
	}

	public void removeSizeProvider(NodeSizeProvider provider) {
		layoutEngine.removeSizeProvider(provider);
	}

	public void addDecorator(NodeDecorator decorator) {
		decorators.add(decorator);
	}

	public void removeDecorator(NodeDecorator decorator) {
		decorators.remove(decorator);
	}

	public void decorate() {
		decorateNode(getRoot(), getLayer(), decorators, false, false);
	}

	@SuppressWarnings("unchecked")
	public static void decorateNode(PRoot root, PNode canvas,
			Collection<NodeDecorator> decorators, boolean noAnimation,
			boolean postLayout) {
		Collection<PActivity> currentActivities = (Collection<PActivity>) canvas
				.getAttribute(CURRENT_DECORATOR_ANIMATIONS);
		if (currentActivities == null) {
			currentActivities = new LinkedList<PActivity>();
			canvas
					.addAttribute(CURRENT_DECORATOR_ANIMATIONS,
							currentActivities);
		} else {
			for (PActivity activity : currentActivities) {
				activity.terminate(PActivity.TERMINATE_WITHOUT_FINISHING);
			}
		}
		for (int i = 0; i < canvas.getChildrenCount(); i++) {
			PNode node = canvas.getChild(i);
			for (NodeDecorator decorator : decorators) {
				if (postLayout || !decorator.onlyDecorateAfterLayout()) {
					PActivity activity = decorator.decorate(node, noAnimation);
					if (activity != null) {
						root.addActivity(activity);
						currentActivities.add(activity);
					}
				}
			}
		}
	}

	public void updateDatasources() {
		LinkDatabase newLinkDB = SessionManager.getManager().getReasoner();
		if (newLinkDB == null)
			newLinkDB = SessionManager.getManager().getSession()
					.getLinkDatabase();
		setLinkDatabase(newLinkDB);
		setLinkProviderDatabase(new TrimmedLinkDatabase(filteredLinkDatabase));
		relayout();
		setFocusedObject(focusedObject);
	}

	public LinkDatabaseCanvas(GraphLayout graphLayout) {
		super();
		setPanEventHandler(new SingleCameraPanHandler());
		getPanEventHandler().setAutopan(false);
		setAutoscrolls(false);
		// setLayout(new BorderLayout());
		placementPanel.setOpaque(false);
		nodeFactory.setCanvas(this);
		layoutEngine.setFactory(nodeFactory);
		setGraphLayout(graphLayout);
		setColorManager(new DefaultTypeColorManager());
		setIconManager(new DefaultTypeColorManager());

		/*
		 * setZoomEventHandler(new PZoomEventHandler() { @Override public void
		 * mouseWheelRotated(PInputEvent event) { } });
		 */
		installListeners();
		installRightClickBehaviors();
		addDefaultBehaviors();
	}

	private MouseListener mouseListener;
	private KeyListener keyListener;
	private MouseWheelListener mouseWheelListener;
	private MouseMotionListener mouseMotionListener;
	private boolean isButton1Pressed;
	private boolean isButton2Pressed;
	private boolean isButton3Pressed;

	/**
	 * This method installs mouse and key listeners on the canvas that forward
	 * those events to piccolo.
	 */
	protected void installInputSources() {
		if (mouseListener == null) {
			mouseListener = new MouseListener() {
				public void mouseClicked(MouseEvent e) {
					sendInputEventToInputManager(e, MouseEvent.MOUSE_CLICKED);
				}

				public void mouseEntered(MouseEvent e) {
					MouseEvent simulated = null;

					if ((e.getModifiersEx() & (InputEvent.BUTTON1_DOWN_MASK
							| InputEvent.BUTTON2_DOWN_MASK | InputEvent.BUTTON3_DOWN_MASK)) != 0) {
						simulated = new MouseEvent((Component) e.getSource(),
								MouseEvent.MOUSE_DRAGGED, e.getWhen(), e
										.getModifiers(), e.getX(), e.getY(), e
										.getClickCount(), e.isPopupTrigger(), e
										.getButton());
					} else {
						simulated = new MouseEvent((Component) e.getSource(),
								MouseEvent.MOUSE_MOVED, e.getWhen(), e
										.getModifiers(), e.getX(), e.getY(), e
										.getClickCount(), e.isPopupTrigger(), e
										.getButton());
					}

					sendInputEventToInputManager(e, MouseEvent.MOUSE_ENTERED);
					sendInputEventToInputManager(simulated, simulated.getID());
				}

				public void mouseExited(MouseEvent e) {

					// MouseEvent simulated = null;
					//
					// if ((e.getModifiersEx() & (InputEvent.BUTTON1_DOWN_MASK
					// | InputEvent.BUTTON2_DOWN_MASK |
					// InputEvent.BUTTON3_DOWN_MASK)) != 0) {
					// simulated = new MouseEvent((Component) e.getSource(),
					// MouseEvent.MOUSE_DRAGGED, e.getWhen(), e
					// .getModifiers(), e.getX(), e.getY(), e
					// .getClickCount(), e.isPopupTrigger(), e
					// .getButton());
					// } else {
					// simulated = new MouseEvent((Component) e.getSource(),
					// MouseEvent.MOUSE_MOVED, e.getWhen(), e
					// .getModifiers(), e.getX(), e.getY(), e
					// .getClickCount(), e.isPopupTrigger(), e
					// .getButton());
					// }
					// sendInputEventToInputManager(simulated,
					// simulated.getID());
					// sendInputEventToInputManager(e, MouseEvent.MOUSE_EXITED);

				}

				public void mousePressed(MouseEvent e) {
					requestFocus();

					boolean shouldBalanceEvent = false;

					if (e.getButton() == MouseEvent.NOBUTTON) {
						if ((e.getModifiers() & MouseEvent.BUTTON1_MASK) == MouseEvent.BUTTON1_MASK) {
							e = new MouseEvent((Component) e.getSource(),
									MouseEvent.MOUSE_PRESSED, e.getWhen(), e
											.getModifiers(), e.getX(),
									e.getY(), e.getClickCount(), e
											.isPopupTrigger(),
									MouseEvent.BUTTON1);
						} else if ((e.getModifiers() & MouseEvent.BUTTON2_MASK) == MouseEvent.BUTTON2_MASK) {
							e = new MouseEvent((Component) e.getSource(),
									MouseEvent.MOUSE_PRESSED, e.getWhen(), e
											.getModifiers(), e.getX(),
									e.getY(), e.getClickCount(), e
											.isPopupTrigger(),
									MouseEvent.BUTTON2);
						} else if ((e.getModifiers() & MouseEvent.BUTTON3_MASK) == MouseEvent.BUTTON3_MASK) {
							e = new MouseEvent((Component) e.getSource(),
									MouseEvent.MOUSE_PRESSED, e.getWhen(), e
											.getModifiers(), e.getX(),
									e.getY(), e.getClickCount(), e
											.isPopupTrigger(),
									MouseEvent.BUTTON3);
						}
					}

					switch (e.getButton()) {
					case MouseEvent.BUTTON1:
						if (isButton1Pressed) {
							shouldBalanceEvent = true;
						}
						isButton1Pressed = true;
						break;

					case MouseEvent.BUTTON2:
						if (isButton2Pressed) {
							shouldBalanceEvent = true;
						}
						isButton2Pressed = true;
						break;

					case MouseEvent.BUTTON3:
						if (isButton3Pressed) {
							shouldBalanceEvent = true;
						}
						isButton3Pressed = true;
						break;
					}

					if (shouldBalanceEvent) {
						MouseEvent balanceEvent = new MouseEvent((Component) e
								.getSource(), MouseEvent.MOUSE_RELEASED, e
								.getWhen(), e.getModifiers(), e.getX(), e
								.getY(), e.getClickCount(), e.isPopupTrigger(),
								e.getButton());
						sendInputEventToInputManager(balanceEvent,
								MouseEvent.MOUSE_RELEASED);
					}

					sendInputEventToInputManager(e, MouseEvent.MOUSE_PRESSED);
				}

				public void mouseReleased(MouseEvent e) {
					boolean shouldBalanceEvent = false;

					if (e.getButton() == MouseEvent.NOBUTTON) {
						if ((e.getModifiers() & MouseEvent.BUTTON1_MASK) == MouseEvent.BUTTON1_MASK) {
							e = new MouseEvent((Component) e.getSource(),
									MouseEvent.MOUSE_RELEASED, e.getWhen(), e
											.getModifiers(), e.getX(),
									e.getY(), e.getClickCount(), e
											.isPopupTrigger(),
									MouseEvent.BUTTON1);
						} else if ((e.getModifiers() & MouseEvent.BUTTON2_MASK) == MouseEvent.BUTTON2_MASK) {
							e = new MouseEvent((Component) e.getSource(),
									MouseEvent.MOUSE_RELEASED, e.getWhen(), e
											.getModifiers(), e.getX(),
									e.getY(), e.getClickCount(), e
											.isPopupTrigger(),
									MouseEvent.BUTTON2);
						} else if ((e.getModifiers() & MouseEvent.BUTTON3_MASK) == MouseEvent.BUTTON3_MASK) {
							e = new MouseEvent((Component) e.getSource(),
									MouseEvent.MOUSE_RELEASED, e.getWhen(), e
											.getModifiers(), e.getX(),
									e.getY(), e.getClickCount(), e
											.isPopupTrigger(),
									MouseEvent.BUTTON3);
						}
					}

					switch (e.getButton()) {
					case MouseEvent.BUTTON1:
						if (!isButton1Pressed) {
							shouldBalanceEvent = true;
						}
						isButton1Pressed = false;
						break;

					case MouseEvent.BUTTON2:
						if (!isButton2Pressed) {
							shouldBalanceEvent = true;
						}
						isButton2Pressed = false;
						break;

					case MouseEvent.BUTTON3:
						if (!isButton3Pressed) {
							shouldBalanceEvent = true;
						}
						isButton3Pressed = false;
						break;
					}

					if (shouldBalanceEvent) {
						MouseEvent balanceEvent = new MouseEvent((Component) e
								.getSource(), MouseEvent.MOUSE_PRESSED, e
								.getWhen(), e.getModifiers(), e.getX(), e
								.getY(), e.getClickCount(), e.isPopupTrigger(),
								e.getButton());
						sendInputEventToInputManager(balanceEvent,
								MouseEvent.MOUSE_PRESSED);
					}

					sendInputEventToInputManager(e, MouseEvent.MOUSE_RELEASED);
				}
			};
			addMouseListener(mouseListener);
		}

		if (mouseMotionListener == null) {
			mouseMotionListener = new MouseMotionListener() {
				public void mouseDragged(MouseEvent e) {
					sendInputEventToInputManager(e, MouseEvent.MOUSE_DRAGGED);
				}

				public void mouseMoved(MouseEvent e) {
					sendInputEventToInputManager(e, MouseEvent.MOUSE_MOVED);
				}
			};
			addMouseMotionListener(mouseMotionListener);
		}

		if (mouseWheelListener == null) {
			mouseWheelListener = new MouseWheelListener() {
				public void mouseWheelMoved(MouseWheelEvent e) {
					sendInputEventToInputManager(e, e.getScrollType());
					if (!e.isConsumed() && getParent() != null) {
						getParent().dispatchEvent(e);
					}
				}
			};
			addMouseWheelListener(mouseWheelListener);
		}

		if (keyListener == null) {
			keyListener = new KeyListener() {
				public void keyPressed(KeyEvent e) {
					sendInputEventToInputManager(e, KeyEvent.KEY_PRESSED);
				}

				public void keyReleased(KeyEvent e) {
					sendInputEventToInputManager(e, KeyEvent.KEY_RELEASED);
				}

				public void keyTyped(KeyEvent e) {
					sendInputEventToInputManager(e, KeyEvent.KEY_TYPED);
				}
			};
			addKeyListener(keyListener);
		}
	}

	/**
	 * This method removes mouse and key listeners on the canvas that forward
	 * those events to piccolo.
	 */
	protected void removeInputSources() {
		if (mouseListener != null)
			removeMouseListener(mouseListener);
		if (mouseMotionListener != null)
			removeMouseMotionListener(mouseMotionListener);
		if (mouseWheelListener != null)
			removeMouseWheelListener(mouseWheelListener);
		if (keyListener != null)
			removeKeyListener(keyListener);

		mouseListener = null;
		mouseMotionListener = null;
		mouseWheelListener = null;
		keyListener = null;
	}

	protected void completeQueuedActions() {
		while (!postLayoutQueue.isEmpty()) {
			Runnable r = postLayoutQueue.removeFirst();
			r.run();
		}
	}

	public void addPostLayoutAction(Runnable r) {
		postLayoutQueue.add(r);
	}

	protected void installListeners() {
		addRelayoutListener(new RelayoutListener() {

			public void relayoutComplete() {
				getZoomEventHandler().setMaxScale(getMaxZoom());
				float minZoom = getMinZoom();
				getZoomEventHandler().setMinScale(minZoom);
				completeQueuedActions();
			}

			public void relayoutStarting() {
			}

		});
		getCamera().addPropertyChangeListener(PCamera.PROPERTY_VIEW_TRANSFORM,
				new PropertyChangeListener() {

					public void propertyChange(PropertyChangeEvent evt) {
						getZoomEventHandler().setMaxScale(getMaxZoom());
						float minZoom = getMinZoom();
						getZoomEventHandler().setMinScale(minZoom);
					}

				});
	}

	protected void addDefaultBehaviors() {
	}

	protected void installRightClickBehaviors() {
	}

	public JComponent getPlacementPanel() {
		return placementPanel;
	}

	public void zoomToObjects() {
		PBounds centerBounds = getLayer().getFullBoundsReference();
		getCamera().animateViewToCenterBounds(centerBounds, true,
				getLayoutDuration());
	}

	public void panToObjects() {
		PBounds centerBounds = getLayer().getFullBoundsReference();
		getCamera().animateViewToCenterBounds(centerBounds, false,
				getLayoutDuration());
	}

	public void setPlacementPanelVisible(boolean isVisible) {
		if (isVisible) {
			add(placementPanel, "Center");
		} else {
			remove(placementPanel);
		}
	}

	public void setVisibleObjects(Collection<? extends PathCapable> visible) {
		linkDatabase.setVisibleObjects(getLinkedObjectCollection(visible),
				false);
	}

	public Collection<PathCapable> getVisibleObjects() {
		Collection<PathCapable> out = new HashSet<PathCapable>();
		for (IdentifiedObject io : linkDatabase.getObjects()) {
			if (io instanceof LinkedObject) {
				LinkedObject lo = (LinkedObject) io;
				out.add(lo);
				for (Link link : linkDatabase.getParents(lo)) {
					out.add(link);
				}
			}
		}
		return out;
	}

	protected static Collection<IdentifiedObject> getLinkedObjectCollection(
			Collection<? extends PathCapable> pcs) {
		Collection<IdentifiedObject> out = new HashSet<IdentifiedObject>();
		for (PathCapable pc : pcs) {
			if (pc instanceof LinkedObject)
				out.add((LinkedObject) pc);
			else if (pc instanceof Link) {
				out.add(((Link) pc).getChild());
				if (((Link) pc).getParent() != null)
					out.add(((Link) pc).getParent());
			}
		}
		return out;
	}

	public void addVisibleObjects(Collection<? extends PathCapable> visible) {
		Collection<IdentifiedObject> current = new HashSet<IdentifiedObject>();
		for (IdentifiedObject io : linkDatabase.getObjects()) {
			if (io instanceof LinkedObject)
				current.add((LinkedObject) io);
		}
		current.addAll(getLinkedObjectCollection(visible));
		linkDatabase.setVisibleObjects(current, true);
	}

	public void removeVisibleObjects(Collection<? extends PathCapable> visible) {
		Collection<IdentifiedObject> current = new HashSet<IdentifiedObject>();
		for (IdentifiedObject io : linkDatabase.getObjects()) {
			if (io instanceof LinkedObject)
				current.add((LinkedObject) io);
		}
		current.removeAll(getLinkedObjectCollection(visible));
		linkDatabase.setVisibleObjects(current, true);
	}

	public Selection getPickedAsSelection(PInputEvent event) {
		PCNode originNode = (PCNode) PiccoloUtil.getNodeOfClass(
				event.getPath(), PCNode.class);
		if (originNode == null)
			return SelectionManager.createEmptySelection(this);
		else
			return SelectionManager.createSelection(this, originNode
					.getObject(), RootAlgorithm.GREEDY,
					getLinkProviderDatabase());
	}

	/**
	 * If the picked object from the given event is part of the selection,
	 * return the selection. Otherwise, return a selection consisting only of
	 * the picked object.
	 * 
	 * @param event
	 * @return
	 */
	public Selection getPickerSelection(PInputEvent event) {
		PCNode originNode = (PCNode) PiccoloUtil.getNodeOfClass(
				event.getPath(), OENode.class, OELink.class);
		if (originNode != null) {
			if (getSelection().getAllSelectedObjects().contains(
					originNode.getObject())) {
				return getSelection();
			} else {
				return SelectionManager.createSelection(this, originNode
						.getObject(), RootAlgorithm.GREEDY,
						getLinkProviderDatabase());
			}
		} else
			return null;
	}

	public void addExpansionListener(ExpandCollapseListener listener) {
		expansionListeners.add(listener);
	}

	public void removeExpansionListener(ExpandCollapseListener listener) {
		expansionListeners.remove(listener);
	}

	public void fireExpandEvent(ExpansionEvent e) {
		for (ExpandCollapseListener listener : expansionListeners) {
			listener.expandStateChanged(e);
		}
	}

	public LinkDatabase getLinkProviderDatabase() {
		return linkProviderDatabase;
	}

	public void setLinkProviderDatabase(LinkDatabase linkDatabase) {
		this.linkProviderDatabase = linkDatabase;
	}

	public void setGraphLayout(GraphLayout layout) {
		layoutEngine.setGraphLayout(layout);
		if (linkDatabase != null)
			layoutEngine.setLinkDatabase(linkDatabase);
	}

	public LinkDatabase getLinkDatabase() {
		return filteredLinkDatabase.getLinkDatabase();
	}

	public void setLinkDatabase(LinkDatabase linkDatabase) {
		if (this.linkDatabase == null) {
			this.filteredLinkDatabase = new FilteredLinkDatabase(linkDatabase);
			this.linkDatabase = new CollapsibleLinkDatabase(
					filteredLinkDatabase);
			this.linkDatabase.addListener(expandCollapseListener);
			if (layoutEngine != null)
				layoutEngine.setLinkDatabase(this.linkDatabase);
		} else {
			this.filteredLinkDatabase.setLinkDatabase(linkDatabase);
		}
		filteredLinkDatabase.setLinkFilter(FilterManager.getManager()
				.getAugmentedLinkFilter(getLinkFilter()));
		filteredLinkDatabase.setTermFilter(FilterManager.getManager()
				.getAugmentedTermFilter(getTermFilter()));
	}

	public float getMinZoom() {
		PBounds zoomDim = getLayer().getFullBounds();
		/*
		 * if (zoomDim.getWidth() > zoomDim.getHeight()) { return Math.min(1,
		 * (float) getCamera().getViewBounds().getWidth() / (float)
		 * zoomDim.getWidth()); } else return Math.min(1, (float)
		 * getCamera().getViewBounds().getHeight() / (float)
		 * zoomDim.getHeight());
		 */
		float viewWidth = (float) getCamera().getWidth();
		float zoomWidth = (float) zoomDim.getWidth();
		float viewHeight = (float) getCamera().getHeight();
		float zoomHeight = (float) zoomDim.getHeight();
		float minZoom = Math.min(Math.min(viewWidth / zoomWidth, viewHeight
				/ zoomHeight), 1f);
		return minZoom;
	}

	public float getMaxZoom() {
		return 1.5f;
	}

	public boolean isLayingOut() {
		return isLayingOut;
	}

	public PNode getFinalLayoutVersion(Object key) {
		if (!isLayingOut())
			return null;
		return morpher.getProvider().getNamedChild(key, newLayer);
	}

	public PNode getNewLayer() {
		if (!isLayingOut())
			throw new IllegalStateException(
					"getNewLayer() can only be called while the canvas is laying out");
		return newLayer;
	}

	/*
	 * public static boolean trackActivityScheduling = false;
	 * 
	 * protected static class SpecialRoot extends PRoot {
	 * 
	 * public static final String PROPERTY_INPUT_SOURCES = "inputSources";
	 * public static final int PROPERTY_CODE_INPUT_SOURCES = 1 << 14;
	 * 
	 * protected transient boolean processingInputs; protected transient boolean
	 * processInputsScheduled;
	 * 
	 * private PInputManager defaultInputManager; private transient List
	 * inputSources; private transient long globalTime; private
	 * PActivityScheduler activityScheduler;
	 * 
	 * public static interface InputSource { public void processInput(); }
	 * 
	 * public SpecialRoot() { super(); inputSources = new ArrayList();
	 * globalTime = System.currentTimeMillis(); activityScheduler = new
	 * PActivityScheduler(this) { @Override public void addActivity(PActivity
	 * activity) { if (trackActivityScheduling && activity instanceof
	 * PCompoundActivity) (new Exception("Scheduled activity
	 * "+activity)).printStackTrace(); super.addActivity(activity); }
	 * 
	 * @Override public void addActivity(PActivity activity, boolean
	 * processLast) { if (trackActivityScheduling && activity instanceof
	 * PCompoundActivity) (new Exception("Scheduled activity
	 * "+activity)).printStackTrace(); super.addActivity(activity, processLast); } }; }
	 * 
	 * public boolean addActivity(PActivity activity) {
	 * getActivityScheduler().addActivity(activity); return true; }
	 * 
	 * public PActivityScheduler getActivityScheduler() { return
	 * activityScheduler; }
	 * 
	 * public void waitForActivities() { PNodeFilter cameraWithCanvas = new
	 * PNodeFilter() { public boolean accept(PNode aNode) { return (aNode
	 * instanceof PCamera) && (((PCamera)aNode).getComponent() != null); }
	 * public boolean acceptChildrenOf(PNode aNode) { return true; } };
	 * 
	 * while (activityScheduler.getActivitiesReference().size() > 0) {
	 * processInputs(); Iterator i = getAllNodes(cameraWithCanvas,
	 * null).iterator(); while (i.hasNext()) { PCamera each = (PCamera)
	 * i.next(); each.getComponent().paintImmediately(); } } }
	 * 
	 * public PRoot getRoot() { return this; }
	 * 
	 * public PInputManager getDefaultInputManager() { if (defaultInputManager ==
	 * null) { defaultInputManager = new PInputManager();
	 * addInputSource(defaultInputManager); } return defaultInputManager; }
	 * 
	 * public void addInputSource(InputSource inputSource) {
	 * inputSources.add(inputSource);
	 * firePropertyChange(PROPERTY_CODE_INPUT_SOURCES ,PROPERTY_INPUT_SOURCES,
	 * null, inputSources); }
	 * 
	 * public void removeInputSource(InputSource inputSource) {
	 * inputSources.remove(inputSource);
	 * firePropertyChange(PROPERTY_CODE_INPUT_SOURCES ,PROPERTY_INPUT_SOURCES,
	 * null, inputSources); }
	 * 
	 * public Timer createTimer(int delay, ActionListener listener) { return new
	 * Timer(delay,listener); }
	 * 
	 * public long getGlobalTime() { return globalTime; }
	 * 
	 * public void processInputs() { PDebug.startProcessingInput();
	 * processingInputs = true;
	 * 
	 * globalTime = System.currentTimeMillis(); int count = inputSources == null ?
	 * 0: inputSources.size(); for (int i = 0; i < count; i++) { InputSource
	 * each = (InputSource) inputSources.get(i); each.processInput(); }
	 * 
	 * activityScheduler.processActivities(globalTime); validateFullBounds();
	 * validateFullPaint();
	 * 
	 * processingInputs = false; PDebug.endProcessingInput(); }
	 * 
	 * public void setFullBoundsInvalid(boolean fullLayoutInvalid) {
	 * super.setFullBoundsInvalid(fullLayoutInvalid);
	 * scheduleProcessInputsIfNeeded(); }
	 * 
	 * public void setChildBoundsInvalid(boolean childLayoutInvalid) {
	 * super.setChildBoundsInvalid(childLayoutInvalid);
	 * scheduleProcessInputsIfNeeded(); }
	 * 
	 * public void setPaintInvalid(boolean paintInvalid) {
	 * super.setPaintInvalid(paintInvalid); scheduleProcessInputsIfNeeded(); }
	 * 
	 * public void setChildPaintInvalid(boolean childPaintInvalid) {
	 * super.setChildPaintInvalid(childPaintInvalid);
	 * scheduleProcessInputsIfNeeded(); }
	 * 
	 * public void scheduleProcessInputsIfNeeded() { // The reason for the
	 * special case here (when not in the event dispatch thread) is that // the
	 * SwingUtilitiles.invokeLater code below only invokes later with respect to
	 * the // event dispatch thread, it will invoke concurrently with other
	 * threads. if (!SwingUtilities.isEventDispatchThread()) { // Piccolo is not
	 * thread safe and should amost always be called from the // Swing event
	 * dispatch thread. It should only reach this point when a new canvas // is
	 * being created. return; }
	 * 
	 * PDebug.scheduleProcessInputs();
	 * 
	 * if (!processInputsScheduled && !processingInputs &&
	 * (getFullBoundsInvalid() || getChildBoundsInvalid() || getPaintInvalid() ||
	 * getChildPaintInvalid())) {
	 * 
	 * processInputsScheduled = true; SwingUtilities.invokeLater(new Runnable() {
	 * public void run() { processInputs();
	 * SpecialRoot.this.processInputsScheduled = false; } }); } } }
	 * 
	 * @Override protected PCamera createDefaultCamera() { PRoot r = new
	 * SpecialRoot(); PLayer l = new PLayer(); PCamera c = new PCamera();
	 * 
	 * r.addChild(c); r.addChild(l); c.addLayer(l);
	 * 
	 * return c; }
	 */

	public NamedChildProvider getNamedChildProvider() {
		return morpher.getProvider();
	}

	protected TaskDelegate<Void> layoutTask;

	protected Image layoutCacheImage;

	public void dim() {
		setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));
	}

	public void undim() {
		setCursor(Cursor.getDefaultCursor());
	}

	public static final Comparator LAYOUT_ORDERING_COMPARATOR = new Comparator() {

		public int compare(Object o1, Object o2) {
			if (o1 instanceof PNode && o2 instanceof PNode) {
				PNode n1 = (PNode) o1;
				PNode n2 = (PNode) o2;
				n1.invalidatePaint();
				n2.invalidatePaint();
				if (o1 instanceof OENode && o2 instanceof OELink)
					return 1;
				else if (o2 instanceof OENode && o1 instanceof OELink)
					return -1;
				else
					return 0;
			} else
				return 0;
		}

	};

	@Override
	public void paint(Graphics g) {
		if (getDisableAnimations() && isLayingOut() && layoutCacheImage != null) {
			g.drawImage(layoutCacheImage, 0, 0, null);
		} else
			super.paint(g);
	}

	public void relayout() {
		isLayingOut = true;
		dim();
		if (getDisableAnimations()) {
			((ExtensibleRoot) getRoot()).setDisableUpdates(true);
			layoutCacheImage = new BufferedImage(getWidth(), getHeight(),
					BufferedImage.TYPE_INT_ARGB);
			paint(layoutCacheImage.getGraphics());
			repaint();
		}
		if (relayoutActivity != null) {
			relayoutActivity.terminate(PActivity.TERMINATE_WITHOUT_FINISHING);
			relayoutActivity = null;
		}

		if (layoutTask != null) {
			layoutTask.cancel();
			layoutTask = null;
		}

		linkDatabase.cleanupCache();

		newLayer = layoutEngine.getNewLayer(getLayer());
		decorateNode(getRoot(), newLayer, decorators, true, true);

		morpher.setNewNodeOriginNode(getFocusedNode());
		relayoutActivity = morpher.morph(getLayer(), newLayer,
				getLayoutDuration());
		if (relayoutActivity instanceof PInterpolatingActivity) {
			((PInterpolatingActivity) relayoutActivity).setSlowInSlowOut(false);
		}
		relayoutActivity.setDelegate(new PActivityDelegate() {

			public void activityFinished(PActivity activity) {
				// decorate();
				isLayingOut = false;
				newLayer = null;

				Collections.sort(getLayer().getChildrenReference(),
						LAYOUT_ORDERING_COMPARATOR);

				fireRelayoutCompleteEvent();
				decorateNode(getRoot(), getLayer(), decorators, true, true);
				undim();
			}

			public void activityStarted(PActivity activity) {
				// Collections.sort(getLayer().getChildrenReference(),
				// LAYOUT_ORDERING_COMPARATOR);

				// startupTrace.printStackTrace();
				fireRelayoutStartingEvent();
			}

			public void activityStepped(PActivity activity) {
			}

		});
		SwingUtilities.invokeLater(new Runnable() {

			public void run() {
				getRoot().addActivity(relayoutActivity);
				((ExtensibleRoot) getRoot()).setDisableUpdates(false);
			}
		});
	}

	public CollapsibleLinkDatabase getCollapsibleLinkDatabase() {
		return linkDatabase;
	}

	public ReasonedLinkDatabase getReasoner() {
		if (getLinkDatabase() instanceof ReasonedLinkDatabase) {
			return (ReasonedLinkDatabase) getLinkDatabase();
		} else
			return SessionManager.getManager().getReasoner();
	}

	public void addDropTargetListener(DropTargetListener listener) {
		dropMulticaster.addListener(listener);
	}

	public void removeDropTargetListener(DropTargetListener listener) {
		dropMulticaster.removeListener(listener);
	}

	public TypeIconManager getIconManager() {
		return nodeFactory.getIconManager();
	}

	public void setIconManager(TypeIconManager iconManager) {
		nodeFactory.setIconManager(iconManager);
	}

	public TypeColorManager getColorManager() {
		return nodeFactory.getColorManager();
	}

	public void setColorManager(TypeColorManager typeManager) {
		nodeFactory.setColorManager(typeManager);
	}

	public long getLayoutDuration() {
		return layoutDuration;
	}

	public void setLayoutDuration(long layoutDuration) {
		this.layoutDuration = layoutDuration;
	}

	public PNode getFocusedNode() {
		if (focusedObject == null)
			return null;
		else
			return getNode(focusedObject);
	}

	protected PathCapable focusedObject;

	public void setFocusedObject(PathCapable focusedObject) {
		PathCapable oldFocused = this.focusedObject;
		this.focusedObject = focusedObject;
		fireFocusedNodeChanged(oldFocused, focusedObject);
	}

	public RightClickMenuBehavior getRightClickBehavior() {
		return rightClickBehavior;
	}

	public void setRightClickBehavior(RightClickMenuBehavior rightClickBehavior) {
		this.rightClickBehavior = rightClickBehavior;
	}

	public NamedChildMorpher getMorpher() {
		return morpher;
	}

	public void setMorpher(NamedChildMorpher morpher) {
		this.morpher = morpher;
	}

	public void addSelectionListener(SelectionListener listener) {
		selectionListeners.add(listener);
	}

	public void removeSelectionListener(SelectionListener listener) {
		selectionListeners.remove(listener);
	}

	protected void fireSelectionEvent(SelectionEvent e) {
		for (SelectionListener listener : selectionListeners) {
			listener.selectionChanged(e);
		}
	}

	public RootAlgorithm getRootAlgorithm() {
		return RootAlgorithm.GREEDY;
	}

	public boolean hasCombinedTermsAndLinks() {
		return false;
	}

	public Selection getSelection(MouseEvent e) {
		PCNode node = getNode(e.getX(), e.getY());
		if (node == null)
			return SelectionManager.createEmptySelection(this);
		if (getSelection().getAllSelectedObjects().contains(node.getObject()))
			return getSelection();
		else
			return SelectionManager.createSelection(this, node.getObject(),
					getRootAlgorithm(), getLinkProviderDatabase());
	}

	public PCNode getNode(int x, int y) {
		PPickPath path = getCamera().pick(x, y, 1);
		PCNode node = (PCNode) PiccoloUtil.getNodeOfClass(path, OENode.class,
				OELink.class);
		return node;
	}

	public GestureTarget getTarget(int x, int y) {
		PCNode node = getNode(x, y);
		if (node == null)
			return SelectionManager.createEmptyTarget();

		return SelectionManager.createGestureTarget(this, getLinkDatabase(),
				getRootAlgorithm(), node.getObject());
	}

	public Selection getSelection() {
		return selection;
	}

	public void select(final Selection selection) {
		this.selection = selection;
		Collection<PathCapable> visible = getVisibleObjects();
		int lastCount = visible.size();
		for (PathCapable pc : selection.getAllSelectedObjects()) {
			visible.add(pc);
		}
		if (selection.getTermSubSelection() != null
				&& !selection.getAllSelectedObjects().contains(
						selection.getTermSubSelection()))
			System.err.println("weird selection");
		if (selection.getTermSubSelection() != null)
			visible.add(selection.getTermSubSelection());
		fireSelectionEvent(new SelectionEvent(this, selection));
		if (visible.size() != lastCount) {
			setVisibleObjects(visible);
		}
		if (selection.getTermSubSelection() != null) {
			setFocusedObject(selection.getTermSubSelection());
			/*
			 * final RelayoutListener listener = new RelayoutListener() { public
			 * void relayoutStarting() { PNode node =
			 * getNode(selection.getTermSubSelection()); setFocusedNode(node);
			 * removeRelayoutListener(this); }
			 * 
			 * public void relayoutComplete() { } };
			 * addRelayoutListener(listener);
			 */
		}
		if (isExpandSelectionPaths()) {
			TreePath[] paths = selection.getPaths();
			if (paths == null || paths.length == 0) {
				paths = new TreePath[1];
				paths = PathUtil.getBestPaths(
						selection.getAllSelectedObjects(), getRootAlgorithm(),
						getLinkDatabase());
			}
			Collection<PathCapable> pathc = new LinkedList<PathCapable>();
			for (TreePath path : paths) {
				Object[] os = path.getPath();
				for (Object o : os) {
					if (o instanceof PathCapable) {
						pathc.add((PathCapable) o);
					}
				}
			}
			addVisibleObjects(pathc);
		}
	}

	public boolean isLive() {
		return isLive;
	}

	public void setLive(boolean isLive) {
		if (isLive && !this.isLive == isLive) {
			SelectionManager.setGlobalSelection(getSelection());
		}
		this.isLive = isLive;
	}

	public PCNode getNode(PathCapable pc) {
		NamedChildProvider provider = getNamedChildProvider();
		return (PCNode) provider.getNamedChild(pc, getLayer());
	}

	protected List<RightClickMenuFactory> menuFactories = new ArrayList<RightClickMenuFactory>();

	public void addMenuFactory(RightClickMenuFactory menuFactory) {
		menuFactories.add(menuFactory);
	}

	public void removeMenuFactory(RightClickMenuFactory menuFactory) {
		menuFactories.remove(menuFactory);
	}

	public PathCapable getFocusedPathCapable() {
		PNode node = getFocusedNode();
		if (node instanceof PCNode) {
			return ((PCNode) node).getObject();
		} else
			return null;
	}

	public Collection<PCNode> getVisibleNodes() {
		Collection<PCNode> out = new LinkedList<PCNode>();
		for (PathCapable pc : getVisibleObjects()) {
			PNode node = getNode(pc);
			if (node != null && node instanceof PCNode)
				out.add((PCNode) node);
		}
		return out;
	}

	public void show(Collection<PathCapable> pcs, boolean zoom) {
		PBounds b = getBounds(pcs);
		getCamera().animateViewToCenterBounds(b, zoom, getLayoutDuration());
	}

	public PBounds getBounds(Collection<PathCapable> pcs) {
		PBounds bounds = null;
		for (PathCapable pc : pcs) {
			if (pc == null)
				continue;
			PNode node = getNode(pc);
			if (node == null)
				continue;
			if (bounds == null) {
				bounds = new PBounds(node.getXOffset(), node.getYOffset(), node
						.getWidth(), node.getHeight());
			} else
				bounds.add(new PBounds(node.getXOffset(), node.getYOffset(),
						node.getWidth(), node.getHeight()));
		}
		return bounds;
	}

	public void fillInMenu(MouseEvent e, JPopupMenu menu) {
		PInputEvent event = new PInputEvent(getRoot().getDefaultInputManager(),
				e);
		event.setPath(getCamera().pick(e.getX(), e.getY(), 1));
		for (RightClickMenuFactory factory : menuFactories) {
			Collection<JMenuItem> factories = factory.getMenuItems(this, event);
			if (factories == null)
				continue;
			for (JMenuItem item : factories) {
				if (item == null)
					continue;
				if (item == RightClickMenuFactory.SEPARATOR_ITEM)
					menu.addSeparator();
				else
					menu.add(item);
			}
		}
	}

	@Override
	protected PCamera createCamera() {
		return new FullPaintCamera();
	}

	public boolean getDisableAnimations() {
		return disableAnimations;
	}

	public void setDisableAnimations(boolean disableAnimations) {
		this.disableAnimations = disableAnimations;
		((ExtensibleRoot) getRoot()).setNoAnimations(disableAnimations);
	}

	protected Filter<?> termFilter;
	protected Filter<?> linkFilter;

	protected boolean expandSelectionPaths = true;

	public void setFilters(Filter<?> termFilter, Filter<?> linkFilter) {
		this.termFilter = termFilter;
		this.linkFilter = linkFilter;
		updateDatasources();
	}

	public Filter<?> getTermFilter() {
		return termFilter;
	}

	public void setTermFilter(Filter<?> termFilter) {
		this.termFilter = termFilter;
		updateDatasources();
	}

	public Filter<?> getLinkFilter() {
		return linkFilter;
	}

	public void setLinkFilter(Filter<?> linkFilter) {
		this.linkFilter = linkFilter;
		updateDatasources();
	}

	protected boolean isExpandSelectionPaths() {
		return expandSelectionPaths;
	}

	protected void setExpandSelectionPaths(boolean expandSelectionPaths) {
		this.expandSelectionPaths = expandSelectionPaths;
	}
}
