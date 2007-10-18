package org.oboedit.gui.components;

import java.awt.*;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DropTarget;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.event.*;
import java.awt.geom.*;
import java.awt.image.*;
import java.util.*;
import java.util.List;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.plaf.TreeUI;
import javax.swing.tree.*;

import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.ConfigurationPanel;
import org.bbop.framework.GUIManager;
import org.bbop.framework.GUIComponent;
import org.bbop.swing.*;
import org.bbop.swing.plaf.DragFriendlyTreeUI;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.filters.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.EditActionManager;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.AbstractInputHandlerBridge;
import org.oboedit.gui.DefaultTermModel;
import org.oboedit.gui.Filterable;
import org.oboedit.gui.FilteredRenderable;
import org.oboedit.gui.GestureTarget;
import org.oboedit.gui.InputHandlerI;
import org.oboedit.gui.LineRenderer;
import org.oboedit.gui.OBOCellRenderer;
import org.oboedit.gui.ObjectSelector;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.RightClickMenuProvider;
import org.oboedit.gui.Selection;
import org.oboedit.gui.SelectionTransferHandler;
import org.oboedit.gui.event.*;
import org.oboedit.util.GUIUtil;
import org.oboedit.util.PathUtil;

public class OBOTermPanel extends JTree implements ObjectSelector,
		FilteredRenderable, Filterable, GUIComponent, DragImageGenerator,
		RightClickMenuProvider {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected String title;

	public static Border EMPTY_BORDER = new EmptyBorder(1, 1, 1, 1);

	public static class OBOPanelConfiguration implements ComponentConfiguration {

		protected boolean isLive;

		protected boolean buttonsVisible = false;

		protected String filterPropID;

		protected String rootAlgorithm;

		protected FilterPair filter;

		public OBOPanelConfiguration() {
		}

		public void setFilterPropID(String filterPropID) {
			this.filterPropID = filterPropID;
		}

		public String getFilterPropID() {
			return filterPropID;
		}

		public void setLive(boolean isLive) {
			this.isLive = isLive;
		}

		public boolean isLive() {
			return isLive;
		}

		public boolean getButtonsVisible() {
			return buttonsVisible;
		}

		public void setRootAlgorithm(String rootAlgorithm) {
			this.rootAlgorithm = rootAlgorithm;
		}

		public String getRootAlgorithm() {
			return rootAlgorithm;
		}

		public void setButtonsVisible(boolean buttonsVisible) {
			this.buttonsVisible = buttonsVisible;
		}

		public void setFilter(FilterPair filter) {
			this.filter = filter;
		}

		public FilterPair getFilter() {
			return filter;
		}
	}

	// protected OBO_1_2_FileSerializer oboAdapter =
	// new OBO_1_2_FileSerializer();

	protected boolean ignoreSelection = false;

	protected boolean scrollOnSelection = true;

	protected int tabRow = -1;

	protected int highlightedRow = -1;

	protected int clickTarget = -1;

	protected int[] sortedSelectionRows = new int[0];

	protected TreePath lockedPath = null;

	protected InputListener inputListener = new InputListener();

	protected InputHandlerI currentDragHandler;

	protected DragSourceDragEvent currentDragEvent;

	protected String dragTitle = "";

	protected java.util.List<RenderedFilter> objectRenderers = new ArrayList<RenderedFilter>();

	protected java.util.List<RenderedFilter> linkRenderers = new ArrayList<RenderedFilter>();

	protected FilterPair filter;

	protected boolean dragging = false;

	protected SessionManager sessionManager = SessionManager.getManager();

	protected static final Color lockGray = new Color(200, 200, 200);

	protected static final Color secondaryGray = new Color(240, 240, 240);

	final static int HEADER_HEIGHT = 20;

	final static int HEADER_MARGIN = 2;

	protected String id;

	TreeSelectionListener treeSelectionListener = new TreeSelectionListener() {
		public void valueChanged(TreeSelectionEvent e) {
			sortedSelectionRows = getSortedSelectionRows();
		}
	};

	ReloadListener reloadListener = new ReloadListener() {
		public void reload(ReloadEvent e) {
			OBOTermPanel.this.reload();
			if (e.isRoot()) {
				Set<LinkedObject> roots = new HashSet<LinkedObject>();
				TermUtil.detectRoots(roots, getLinkDatabase(),
						getRootAlgorithm());
				TreePath rootPath = new TreePath(TermModel.ROOT);
				Iterator it = roots.iterator();
				while (it.hasNext()) {
					OBORestriction tr = new OBORestrictionImpl(
							(LinkedObject) it.next());

					TreePath path = rootPath.pathByAddingChild(tr);
					expandPath(path);
				}
			}
			// TODO Auto-generated method stub

		}
	};

	public ConfigurationPanel getConfigurationPanel() {
		return null;
	}

	SelectionListener selectionListener = new SelectionListener() {
		public void selectionChanged(SelectionEvent e) {
			if (isLive()) {
				OBOTermPanel.this.select(e.getSelection());
			}
		}
	};

	FocusListener focusListener = new FocusListener() {
		public void focusGained(FocusEvent e) {
			doUpdate(true);
		}

		protected void doUpdate(boolean focused) {
			JScrollPane parentScroller = (JScrollPane) SwingUtilities
					.getAncestorOfClass(JScrollPane.class, OBOTermPanel.this);
			if (parentScroller != null) {
				if (focused)
					parentScroller.setViewportBorder(new LineBorder(
							Color.black, 1));
				else
					parentScroller.setViewportBorder(EMPTY_BORDER);
				parentScroller.repaint();
			}
		}

		public void focusLost(FocusEvent e) {
			doUpdate(false);
		}
	};

	KeyListener focusKeyListener = new KeyAdapter() {
		@Override
		public void keyPressed(KeyEvent e) {
			if (e.getKeyCode() == KeyEvent.VK_F && e.isControlDown())
				requestFocus();
			else if (e.getKeyCode() == KeyEvent.VK_OPEN_BRACKET)
				tabBackward();
			else if (e.getKeyCode() == KeyEvent.VK_CLOSE_BRACKET)
				tabForward();
		}
	};

	ReconfigListener reconfigListener = new ReconfigListener() {
		public void configReloaded(ReconfigEvent e) {
			setToolTips();
		}
	};

	protected boolean isLive = true;

	public Icon getImage(DragSourceDragEvent e) {
		return new ImageIcon(getDragImage(getSelectionRows()));
	}

	protected void paintHeaderShape(Graphics2D graphics, int width, int height) {
		graphics
				.fill(new CubicCurve2D.Double(0, height, 0, 0, 0, 0, height, 0));
		graphics.fill(new CubicCurve2D.Double((width - height), 0, width, 0,
				width, 0, width, height));

		Polygon finalArea = new Polygon();
		finalArea.addPoint(0, height);
		finalArea.addPoint(height, 0);
		finalArea.addPoint(width - height, 0);
		finalArea.addPoint(width, height);
		graphics.fill(finalArea);
	}

	protected BufferedImage getDragImage(int row[]) {
		CellRendererPane rendererPane = new CellRendererPane();
		FontMetrics fm = OBOTermPanel.this.getFontMetrics(getFont());

		TreeCellRenderer rend = getCellRenderer();
		int width = (int) fm.getStringBounds(dragTitle, getGraphics())
				.getWidth()
				+ HEADER_HEIGHT;
		int height = 0;
		for (int i = 0; i < row.length; i++) {
			Component rendComponent = rend.getTreeCellRendererComponent(this,
					getPathForRow(row[i]).getLastPathComponent(), false, false,
					false, row[i], false);
			rendererPane.add(rendComponent);
			rendComponent.validate();
			Dimension dim = rendComponent.getPreferredSize();
			rendComponent.setVisible(true);
			width = (int) Math.max(width, dim.getWidth());
			height += dim.getHeight();
		}
		height += HEADER_HEIGHT;

		BufferedImage out = new BufferedImage(width, height,
				BufferedImage.TYPE_INT_ARGB);

		Graphics2D graphics = out.createGraphics();
		graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
				RenderingHints.VALUE_ANTIALIAS_ON);

		graphics.setColor(new Color(0, 0, 255, 80));
		graphics.setClip(0, 0, width, height);

		paintHeaderShape(graphics, width, HEADER_HEIGHT);

		graphics.fillRect(0, HEADER_HEIGHT, width, height - HEADER_HEIGHT);

		graphics.setColor(Color.white);

		graphics.setFont(getFont());

		graphics.drawString(dragTitle, HEADER_HEIGHT / 2, HEADER_HEIGHT
				- fm.getDescent() - HEADER_MARGIN);

		graphics.drawLine(0, HEADER_HEIGHT, width, HEADER_HEIGHT);

		for (int i = 0; i < row.length; i++) {
			Component rendComponent = rend.getTreeCellRendererComponent(this,
					getPathForRow(row[i]).getLastPathComponent(), false, false,
					false, row[i], false);

			Dimension dim = rendComponent.getPreferredSize();

			rendererPane.paintComponent(graphics, rendComponent, this, 0,
					HEADER_HEIGHT + i * dim.height, width, dim.height, true);
			if (i != row.length - 1) {
				graphics.setColor(new Color(0, 0, 255, 180));
				graphics.drawLine(0, HEADER_HEIGHT
						+ (int) (dim.getHeight() * (i + 1)), width,
						HEADER_HEIGHT + (int) (dim.getHeight() * (i + 1)));
			}
		}
		rendererPane.removeAll();
		System.err.println("   CREATING IMAGE W/ TITLE " + dragTitle);
		return out;
	}

	protected void ensureLockedPathIsShowing() {
		if (lockedPath != null && !isShowing(lockedPath)) {
			FreezableViewport freezableViewport = (FreezableViewport) SwingUtilities
					.getAncestorOfClass(FreezableViewport.class, this);
			if (freezableViewport != null)
				freezableViewport.setFrozen(false);

			Rectangle bounds = getPathBounds(lockedPath);

			if (bounds != null)
				scrollRectToVisible(bounds);

			if (freezableViewport != null)
				freezableViewport.setFrozen(true);
		}
	}

	public boolean isLive() {
		return isLive;
	}

	public void setLive(boolean isLive) {
		this.isLive = isLive;
		TreePath[] paths = SelectionManager.getGlobalSelection().getPaths(
				getRootAlgorithm(), getLinkDatabase());
		setSelectionPaths(paths);
	}

	@Override
	public void collapsePath(TreePath path) {
		if (lockedPath != null && SwingUtil.isChildPath(path, lockedPath))
			return;
		else
			super.collapsePath(path);
	}

	public boolean teardownWhenHidden() {
		return false;
	}

	@Override
	public void expandPath(TreePath path) {
		super.expandPath(path);
		ensureLockedPathIsShowing();
	}

	public boolean isShowing(TreePath path) {
		JViewport viewport = (JViewport) SwingUtilities.getAncestorOfClass(
				JViewport.class, this);
		if (viewport == null)
			return true;
		else {
			Rectangle visibleBounds = viewport.getViewRect();
			Rectangle pathBounds = getPathBounds(path);
			if (pathBounds == null || visibleBounds == null)
				return false;
			return visibleBounds.intersects(pathBounds);
		}
	}

	/*
	 * boolean dragging = false;
	 * 
	 * public void drop(DropTargetDropEvent e) { highlightDropTarget(-1);
	 * indicateClickTarget(-1);
	 * 
	 * Point myPoint = e.getLocation(); TreePath dest = getPath(myPoint);
	 * GestureTarget target = SelectionManager.createGestureTarget(
	 * OBOTermPanel.this, dest, true);
	 * 
	 * Object transferData = DropUtil.getSelection(e);
	 * 
	 * currentDragHandler.drop(OBOTermPanel.this, transferData, target, e
	 * .getLocation(), Controller.getController().getKeyRecorder()
	 * .getKeyChecker());
	 * 
	 * dragging = false; }
	 * 
	 * public void dragOver(DropTargetDragEvent e) { if (!allowDrop(e)) {
	 * e.rejectDrag(); return; } else
	 * e.acceptDrag(DnDConstants.ACTION_COPY_OR_MOVE);
	 * 
	 * int row = getRowForLocation((int) e.getLocation().getX(), (int) e
	 * .getLocation().getY()); highlightDropTarget(row); }
	 * 
	 * public boolean allowDrop(DropTargetDragEvent e) { Object data =
	 * DropUtil.getSelection(e); if (data != null) { TreePath dest =
	 * getPath(e.getLocation()); if (dest == null) return false; GestureTarget
	 * target = SelectionManager.createGestureTarget( OBOTermPanel.this, dest,
	 * true); java.util.List l = Controller.getController() .getInputHandlers();
	 * for (int i = 0; i < l.size(); i++) { InputHandlerI handler =
	 * (InputHandlerI) l.get(i); int allowDropVal =
	 * handler.allowDrop(OBOTermPanel.this, data, target, e.getLocation(),
	 * Controller .getController().getKeyRecorder() .getKeyChecker());
	 * System.err.println(" handler = " + handler.getClass() + ", allowDropVal = " +
	 * allowDropVal); if (allowDropVal == InputHandlerI.ALMOST_ACCEPT_DROP ||
	 * allowDropVal == InputHandlerI.ACCEPT_DROP) {
	 * setDragTitle(handler.getDragDesc()); currentDragHandler = handler; return
	 * allowDropVal == InputHandlerI.ACCEPT_DROP; } } System.err.println();
	 * return false; } return false; }
	 * 
	 * public void dragEnter(DropTargetDragEvent e) { if (!allowDrop(e)) {
	 * e.rejectDrag(); return; } else
	 * e.acceptDrag(DnDConstants.ACTION_COPY_OR_MOVE); }
	 * 
	 * public void dragExit(DropTargetEvent e) { highlightDropTarget(-1); }
	 * 
	 * public void dropActionChanged(DropTargetDragEvent dtde) { }
	 */

	protected class InputListener extends AbstractInputHandlerBridge {
		public InputListener() {
			setComponent(OBOTermPanel.this);
		}

		@Override
		public void mouseReleased(MouseEvent e) {
			super.mouseReleased(e);
		}

		@Override
		public GestureTarget getTarget(double x, double y) {
			Point myPoint = new Point((int) x, (int) y);
			TreePath dest = getPath(myPoint);
			GestureTarget target = SelectionManager.createGestureTarget(
					OBOTermPanel.this, dest, true);
			return target;
		}

		@Override
		public void drop(DropTargetDropEvent e) {
			highlightDropTarget(-1);
			indicateClickTarget(-1);
			super.drop(e);
		}

		@Override
		public void dragOver(DropTargetDragEvent e) {
			super.dragOver(e);
			int row = getRowForLocation((int) e.getLocation().getX(), (int) e
					.getLocation().getY());
			highlightDropTarget(row);
		}

		@Override
		protected void almostDropDelegate(InputHandlerI handler) {
			setDragTitle(handler.getDragDesc());
		}

		public void dragExit(DropTargetEvent e) {
			highlightDropTarget(-1);
		}
	}

	public class OBOTermPanelUI extends DragFriendlyTreeUI {

		protected class MouseUIAdapter extends MouseAdapter {

			@Override
			public void mousePressed(MouseEvent e) {
				indicateClickTarget(getRowForLocation(e.getX(), e.getY()));
			}

			@Override
			public void mouseReleased(MouseEvent e) {
				indicateClickTarget(-1);
				if (OBOTermPanel.this.dragging || e.isConsumed()) {
					return;
				}

				if (tree != null && tree.isEnabled()) {
					tree.requestFocus();
					TreePath path = getClosestPathForLocation(tree, e.getX(), e
							.getY());

					if (path != null) {
						Rectangle bounds = getPathBounds(tree, path);

						if (e.getY() > (bounds.y + bounds.height)) {
							return;
						}

						if (SwingUtilities.isLeftMouseButton(e))
							checkForClickInExpandControl(path, e.getX(), e
									.getY());

						int x = e.getX();

						if (x > bounds.x) {
							if (x <= (bounds.x + bounds.width)
									&& (!isLive() || SelectionManager
											.getManager()
											.doPreSelectValidation(
													OBOTermPanel.this))) {
								selectPathForEvent(path, e);
							}
						}
					}
				}
			}
		}

		public class KeyUIAdapter extends KeyAdapter {
			/** Key code that is being generated for. */
			protected Action repeatKeyAction;

			/** Set to true while keyPressed is active. */
			protected boolean isKeyDown;

			@Override
			public void keyTyped(KeyEvent e) {
			}

			@Override
			public void keyPressed(KeyEvent e) {
				if (tree != null && tree.hasFocus() && tree.isEnabled()) {
					KeyStroke keyStroke = KeyStroke.getKeyStroke(
							e.getKeyCode(), e.getModifiers());

					if (tree.getConditionForKeyStroke(keyStroke) == JComponent.WHEN_FOCUSED) {
						ActionListener listener = tree
								.getActionForKeyStroke(keyStroke);

						if (listener instanceof Action) {
							repeatKeyAction = (Action) listener;
							if (!repeatKeyAction.isEnabled()) {
								repeatKeyAction = null;
							}
						} else
							repeatKeyAction = null;
					} else
						repeatKeyAction = null;
					if (isKeyDown && repeatKeyAction != null) {
						repeatKeyAction.actionPerformed(new ActionEvent(tree,
								ActionEvent.ACTION_PERFORMED,
								"" /* tree.getActionCommand() */, e.getWhen(),
								e.getModifiers()));
						e.consume();
					} else
						isKeyDown = true;
				}
			}

			@Override
			public void keyReleased(KeyEvent e) {
				isKeyDown = false;
			}

		} // End of BasicTreeUI.KeyHandler

		@Override
		protected MouseListener createMouseListener() {
			return new MouseUIAdapter();
		}

		@Override
		protected KeyListener createKeyListener() {
			return new KeyUIAdapter();
		}

		@Override
		protected void selectPathForEvent(TreePath path, MouseEvent event) {
			super.selectPathForEvent(path, event);
			fireSelectionEvent();
			if (isLive())
				SelectionManager.setGlobalSelection(getSelection());
		}

		protected int[] triangleXBuffer = new int[3];

		protected int[] triangleYBuffer = new int[3];

		@Override
		protected boolean isToggleEvent(MouseEvent event) {
			return false;
		}

		@Override
		protected void paintHorizontalLine(Graphics g, JComponent c, int y,
				int left, int right, boolean isLeaf, TreePath path) {
			if (OBOTermPanel.this.getCellRenderer() instanceof LineRenderer) {
				LineRenderer lineRenderer = (LineRenderer) OBOTermPanel.this
						.getCellRenderer();
				// y = getRowHeight() / 2;
				lineRenderer.paintLine(g, c, y, left, right, isLeaf, path);
			} else {
				/*
				 * g.drawLine(left, y, right, y); if (drawArrowhead &&
				 * path.getPathCount() > 3) { triangleYBuffer[0] = y -
				 * triangleYSize; triangleYBuffer[1] = y + triangleYSize;
				 * triangleYBuffer[2] = y;
				 * 
				 * int xoffset = triangleOffset; if (!isLeaf) { xoffset += 2 +
				 * controlWidth / 2; } if (arrowheadLeft) { triangleXBuffer[0] =
				 * left + xoffset + triangleXSize; triangleXBuffer[1] = left +
				 * xoffset + triangleXSize; triangleXBuffer[2] = left + xoffset; }
				 * else { triangleXBuffer[0] = right - xoffset - triangleXSize;
				 * triangleXBuffer[1] = right - xoffset - triangleXSize;
				 * triangleXBuffer[2] = right - xoffset; }
				 * g.fillPolygon(triangleXBuffer, triangleYBuffer, 3); }
				 */
			}
		}

	}

	/*
	 * protected KeyListener draggingKeyListener = new KeyListener() { protected
	 * void processDraggingKey(KeyEvent e) { Component comp =
	 * currentDragEvent.getDragSourceContext() .getComponent(); Point myPoint =
	 * SwingUtilities.convertPoint(comp, currentDragEvent .getX(),
	 * currentDragEvent.getY(), OBOTermPanel.this); TreePath dest =
	 * getPath(myPoint); GestureTarget target =
	 * SelectionManager.createGestureTarget( OBOTermPanel.this, dest, true);
	 * 
	 * OBOTermPanel dragComp = comp instanceof OBOTermPanel ? (OBOTermPanel)
	 * comp : null;
	 * 
	 * Controller controller = Controller.getController();
	 * 
	 * Object transferData = null; try { transferData =
	 * currentDragEvent.getDragSourceContext()
	 * .getTransferable().getTransferData( new DataFlavor(
	 * DataFlavor.javaJVMLocalObjectMimeType)); } catch
	 * (UnsupportedFlavorException e1) { e1.printStackTrace(); } catch
	 * (IOException e1) { e1.printStackTrace(); } catch (ClassNotFoundException
	 * e1) { e1.printStackTrace(); }
	 * 
	 * java.util.List l = controller.getInputHandlers(); for (int i = 0; i <
	 * l.size(); i++) { InputHandlerI handler = (InputHandlerI) l.get(i); int
	 * allowDropVal = handler.allowDrop(dragComp, OBOTermPanel.this, controller,
	 * transferData, target, currentDragEvent, controller.getKeyRecorder()
	 * .getKeyChecker()); if (allowDropVal == InputHandlerI.ALMOST_ACCEPT_DROP ||
	 * allowDropVal == InputHandlerI.ACCEPT_DROP) { currentDragHandler =
	 * handler; break; } } }
	 * 
	 * public void keyTyped(KeyEvent e) { }
	 * 
	 * public void keyReleased(KeyEvent e) { processDraggingKey(e); }
	 * 
	 * public void keyPressed(KeyEvent e) { processDraggingKey(e); } };
	 */
	protected Collection<SelectionListener> selectionListeners = new LinkedList<SelectionListener>();

	protected JScrollPane scrollPane;

	protected TreePath getPath(Point p) {
		TreePath path = OBOTermPanel.this.getPathForLocation((int) p.getX(),
				(int) p.getY());
		return path;
	}

	protected void fireSelectionEvent() {
		for (SelectionListener listener : selectionListeners) {
			listener.selectionChanged(new SelectionEvent(this, getSelection()));
		}
	}

	public void addSelectionListener(SelectionListener listener) {
		selectionListeners.add(listener);
	}

	public void removeSelectionListener(SelectionListener listener) {
		selectionListeners.remove(listener);
	}

	public void setDragTitle(String title) {
		boolean updateImage = !dragTitle.equals(title);
		this.dragTitle = title;

		if (updateImage) {
			GhostImageController.getInstance().forceImageUpdate();
			/*
			 * ((OBODragListener) (dragSource.getListener())).clearImageCache();
			 * controller.getDragController().updateDragImage(dragSource,
			 * false);
			 * 
			 * ((new Exception("Should update drag image"))).printStackTrace();
			 */
		}
	}

	public LinkDatabase getLinkDatabase() {
		return ((TermModel) getModel()).getLinkDatabase();
	}

	public RootAlgorithm getRootAlgorithm() {
		return ((TermModel) getModel()).getRootAlgorithm();
	}

	/*
	 * public TreePath [] getFilteredPathsFromTerms(Collection terms) { return
	 * getFilteredPaths(terms); }
	 * 
	 * public TreePath [] getFilteredPaths(Collection terms) {
	 * 
	 * Set pathVector = new HashSet(); Iterator it = terms.iterator();
	 * while(it.hasNext()) { PathCapable lo = (PathCapable) it.next(); if
	 * (controller.selectShortestPathOnly()) { TreePath path = TermUtil.
	 * getShortestPath(lo, ((TermModel) getModel()). getRootAlgorithm(),
	 * ((TermModel) getModel()). getLinkDatabase()); pathVector.add(path); }
	 * else { pathVector.addAll(TermUtil. getPathsAsVector(lo, ((TermModel)
	 * getModel()). getRootAlgorithm(), ((TermModel) getModel()).
	 * getLinkDatabase())); } }
	 * 
	 * TreePath [] selectPaths = new TreePath[pathVector.size()]; it =
	 * pathVector.iterator(); for(int i=0; it.hasNext(); i++) { selectPaths[i] =
	 * (TreePath) it.next(); }
	 * 
	 * return selectPaths; }
	 * 
	 * public TreePath [] getFilteredPathsFromLinksOrTerms(Collection objects) {
	 * Set terms = new HashSet(); Iterator it = objects.iterator();
	 * while(it.hasNext()) { Object o = it.next(); if (o instanceof Link)
	 * terms.add(((Link) o).getChild()); else if (o instanceof LinkedObject)
	 * terms.add(o); } return getFilteredPathsFromTerms(terms); }
	 * 
	 * public TreePath [] getFilteredPathsFromLinks(Collection links) { Set
	 * terms = new HashSet(); Iterator it = links.iterator();
	 * while(it.hasNext()) { Link link = (Link) it.next();
	 * terms.add(link.getChild()); } return getFilteredPathsFromTerms(terms); }
	 */
	public int getHighlightRow() {
		return highlightedRow;
	}

	public void highlightDropTarget(int row) {
		int oldHighlightedRow = highlightedRow;
		highlightedRow = row;
		repaintRow(highlightedRow);
		repaintRow(oldHighlightedRow);
	}

	public void indicateClickTarget(int row) {
		int oldClickTarget = clickTarget;
		clickTarget = row;
		repaintRow(clickTarget);
		repaintRow(oldClickTarget);
	}

	/*
	 * public void cleanupDragController() { Controller controller =
	 * Controller.getController();
	 * controller.getDragController().unregisterDropTarget(dropTarget);
	 * controller.getDragController().unregisterDragSource(dragSource); }
	 * 
	 * public void initDragController() {
	 * 
	 * Controller controller = Controller.getController(); if (dropTarget ==
	 * null) { inputListener = new InputListener(); dropTarget = new
	 * DropTarget(this, inputListener); } if (dragSource == null) { dragSource =
	 * new DragSource(this, new OBODragListener(this)); }
	 * 
	 * controller.getDragController().registerDropTarget(dropTarget);
	 * controller.getDragController().registerDragSource(dragSource); }
	 */
	public void setScrollOnSelection(boolean scrollOnSelection) {
		this.scrollOnSelection = scrollOnSelection;
	}

	public boolean getScrollOnSelection() {
		return scrollOnSelection;
	}

	public TreePath getLockedPath() {
		return lockedPath;
	}

	public void setLockedPath(TreePath lockedPath) {
		this.lockedPath = lockedPath;
		// buttonPanel.setLockedPath(lockedPath);
		FreezableScrollPane scrollPane = (FreezableScrollPane) SwingUtilities
				.getAncestorOfClass(FreezableScrollPane.class, this);
		if (scrollPane != null) {
			scrollPane.setFrozen(lockedPath != null);
			if (lockedPath != null) {
				Rectangle r = getPathBounds(lockedPath);
				if (!scrollPane.getViewport().getViewRect().contains(r)) {
					JOptionPane
							.showMessageDialog(
									this,
									"Warning: The view has been locked around a path that "
											+ "isn't currently visible. The view will be adjusted to "
											+ "make the path visible.");
					ensureLockedPathIsShowing();
				}
			}
		}
		repaint();
	}

	@Override
	public Color getBackground() {
		if (lockedPath != null)
			return lockGray;
		else
			return Color.white;
	}

	@Override
	public void paint(Graphics g) {
		TreeModel model = getModel();
		if (model instanceof DefaultTermModel) {
			((DefaultTermModel) model).setPainting(true);
		}
		super.paint(g);
		if (model instanceof DefaultTermModel) {
			((DefaultTermModel) model).setPainting(false);
		}
	}

	public void repaintRow(int row) {
		if (row != -1) {
			Rectangle bounds = getRowBounds(row);
			if (bounds != null) {
				repaint(bounds.x, bounds.y, bounds.width, bounds.height);
			}
		}
	}

	public void setFilter(FilterPair filter) {
		if (getModel() instanceof TermModel) {
			this.filter = filter;
			if (filter != null) {
				((TermModel) getModel())
						.setTermFilter(filter.getObjectFilter());
				((TermModel) getModel()).setLinkFilter(filter.getLinkFilter());
			} else {
				((TermModel) getModel()).setTermFilter(null);
				((TermModel) getModel()).setLinkFilter(null);
			}
			reload();
		}
	}

	public FilterPair getFilter() {
		if (getModel() instanceof TermModel) {
			return filter;
		} else
			return null;
	}

	/*
	 * public void setTermFilter(VectorFilter filter) { if (getModel()
	 * instanceof TermModel) { ((TermModel) getModel()).setTermFilter(filter);
	 * formatCornerComponent(); reload(); } }
	 */

	public void setPropertyFilter(OBOProperty type) {
		if (getModel() instanceof DefaultTermModel) {
			((DefaultTermModel) getModel()).setPropertyFilter(type);
		}
	}

	public OBOProperty getPropertyFilter() {
		return ((DefaultTermModel) getModel()).getPropertyFilter();
	}

	public void addObjectRenderer(RenderedFilter pair) {
		objectRenderers.add(pair);
		repaint();
	}

	@Override
	public synchronized void addMouseListener(MouseListener l) {
		super.addMouseListener(l);
	}

	@Override
	public synchronized void removeMouseListener(MouseListener l) {
		// TODO Auto-generated method stub
		super.removeMouseListener(l);
	}

	public void removeObjectRenderer(RenderedFilter pair) {
		objectRenderers.remove(pair);
		repaint();
	}

	public java.util.List<RenderedFilter> getObjectRenderers() {
		return objectRenderers;
	}

	/*
	 * public void addFilteredRenderer(FilteredRenderer renderer) {
	 * filteredRenderers.add(renderer); formatCornerComponent(); reload(); }
	 * 
	 * public void removeFilteredRenderer(FilteredRenderer renderer) {
	 * filteredRenderers.remove(renderer); formatCornerComponent(); reload(); }
	 * 
	 * public Vector getFilteredRenderers() { return filteredRenderers; }
	 */
	public void setIgnoreSelection(boolean ignoreSelection) {
		this.ignoreSelection = ignoreSelection;
	}

	public boolean ignoreSelection() {
		return ignoreSelection;
	}

	public int getClickTarget() {
		return clickTarget;
	}

	protected void scrollToTabLoc(int oldRow) {
		if (tabRow == -1 || lockedPath != null)
			return;
		if (tabRow >= getSelectionCount())
			tabRow = getSelectionCount() - 1;
		scrollRowToVisible(getTabRow());
		if (oldRow != -1)
			repaint(getRowBounds(oldRow));
		if (tabRow != -1)
			repaint(getRowBounds(tabRow));
		repaint();
	}

	protected void tabForward() {
		if (sortedSelectionRows.length == 0) {
			tabRow = -1;
		} else {
			if (tabRow < sortedSelectionRows.length - 1)
				tabRow++;
			else
				tabRow = 0;
		}
		if (tabRow >= 0) {
			int row = sortedSelectionRows[tabRow];
			if (isLive()) {
				LinkedObject term = null;
				TreePath path = getPathForRow(row);
				Object o = path.getLastPathComponent();
				if (o instanceof Link) {
					term = ((Link) o).getChild();
				}
				if (term != null) {
					Selection selection = SelectionManager.changeSubSelection(
							SelectionManager.getGlobalSelection(), term);
					SelectionManager.setGlobalSelection(selection);
				}
			}
			scrollToTabLoc(tabRow);
		}
	}

	protected void tabBackward() {
		if (sortedSelectionRows.length == 0) {
			tabRow = -1;
		} else {
			if (tabRow > 0)
				tabRow--;
			else
				tabRow = sortedSelectionRows.length - 1;
		}
		if (tabRow >= 0) {
			int row = sortedSelectionRows[tabRow];
			if (isLive()) {
				LinkedObject term = null;
				TreePath path = getPathForRow(row);
				Object o = path.getLastPathComponent();
				if (o instanceof Link) {
					term = ((Link) o).getChild();
				}
				if (term != null) {
					Selection selection = SelectionManager.changeSubSelection(
							SelectionManager.getGlobalSelection(), term);
					SelectionManager.setGlobalSelection(selection);
				}
			}
			scrollToTabLoc(tabRow);
		}
	}

	public int getTabRow() {
		if (tabRow < 0 || tabRow >= sortedSelectionRows.length
				|| sortedSelectionRows.length == 0)
			return -1;
		if (tabRow >= sortedSelectionRows.length)
			tabRow = sortedSelectionRows.length - 1;

		return sortedSelectionRows[tabRow];
	}

	@Override
	public String getToolTipText(MouseEvent e) {
		TreePath path = getPathForLocation(e.getX(), e.getY());
		if (path == null || !(path.getLastPathComponent() instanceof Link))
			return null;
		Link link = (Link) path.getLastPathComponent();
		String s = link.getChild().getID();
		StringBuffer buf = new StringBuffer("<html>");
		int linelength = 0;
		int MAXLENGTH = 40;
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			linelength++;
			if (c == '<')
				buf.append("&lt;");
			else if (c == '>')
				buf.append("&gt;");
			else if (c == '\n'
					|| (linelength > MAXLENGTH && Character.isWhitespace(c))) {
				buf.append("<br>\n");
				linelength = 0;
			} else
				buf.append(c);
		}

		buf.append("</html>");
		return buf.toString();
	}

	public void attachListeners() {

		addComponentListener(new ComponentAdapter() {
			@Override
			public void componentResized(ComponentEvent e) {
				ensureLockedPathIsShowing();
			}
		});
		addTreeSelectionListener(treeSelectionListener);
		addMouseListener(new MouseAdapter() {
			@Override
			public void mouseEntered(MouseEvent e) {
				EditActionManager.getManager().getKeyRecorder().addKeyListener(
						focusKeyListener);
			}

			@Override
			public void mouseExited(MouseEvent e) {
				EditActionManager.getManager().getKeyRecorder().removeKeyListener(
						focusKeyListener);
			}
		});
		addMouseListener(inputListener);
		addKeyListener(inputListener);
		addFocusListener(focusListener);
		Preferences.getPreferences().addReconfigListener(reconfigListener);
		GUIUtil.addReloadListener(reloadListener);
		// controller.addListener(rendererListener);
		SelectionManager.getManager().addSelectionListener(selectionListener);

	}

	public void cleanup() {

		ToolTipManager.sharedInstance().unregisterComponent(this);
		// controller.removeListener(termSelectListener);
		EditActionManager.getManager().getKeyRecorder().removeKeyListener(
				focusKeyListener);
		Preferences.getPreferences().removeReconfigListener(reconfigListener);
		GUIUtil.removeReloadListener(reloadListener);
		SelectionManager.getManager()
				.removeSelectionListener(selectionListener);

		SelectionTransferHandler.removeHandler(selectionTransferHandler);
		setDropTarget(null);
	}

	public OBOTermPanel(String id) {
		this.id = id;
		setDrawArrowhead(true);
		setShowsRootHandles(true);
		setRootVisible(false);
		/*
		 * addFocusListener(new FocusAdapter() { @Override public void
		 * focusGained(FocusEvent e) {
		 * Controller.getController().setPrimarySelector(OBOTermPanel.this); }
		 * });
		 */
		scrollPane = new JScrollPane(this);

		DragFriendlyTreeUI ui = getDefaultUI();
		setUI(ui);
		ui.setRightChildIndent(20);

		DefaultTermModel model = new DefaultTermModel();
		setModel(model);
		model.init();
	}

	@Override
	public void setUI(TreeUI ui) {
		if (!(ui instanceof OBOTermPanelUI))
			super.setUI(getDefaultUI());
		else
			super.setUI(ui);
	}

	public Selection getSelection(MouseEvent e) {
		return getSelection();
	}

	public Selection getSelection() {
		TreePath[] paths = getSelectionPaths();
		if (paths == null || paths.length == 0)
			return SelectionManager.createEmptySelection(this);
		Link leadLink = null;
		TreePath lead = getLeadSelectionPath();
		if (lead != null) {
			leadLink = (Link) lead.getLastPathComponent();
			int leadIndex = -1;
			for (int i = 0; i < paths.length; i++) {
				if (paths[i] == lead) {
					leadIndex = i;
					break;
				}
			}
			// if the lead index isn't the first item in the list of paths
			// move it up
			if (leadIndex > 0) {
				TreePath temp = paths[0];
				paths[0] = lead;
				paths[leadIndex] = temp;
			}
		}
		return SelectionManager.createSelectionFromPaths(this,
				getSelectionPaths(), leadLink, SessionManager.getManager()
						.getCurrentLinkDatabase(), RootAlgorithm.GREEDY, true);
	}

	public void select(Selection selection) {

		TreePath[] paths = selection.getPaths(getRootAlgorithm(),
				getLinkDatabase());
		setSelectionPaths(paths);
		System.err.println("set selection paths to:");
		for (int i = 0; i < paths.length; i++)
			System.err.println("  path[" + i + "] = " + paths[i]);
		setSelectionPaths(paths);
		if (paths.length > 0) {
			makeVisible(paths[0]);
			scrollPathToVisible(paths[0]);
		}
	}

	public void fillInMenu(MouseEvent e, JPopupMenu menu) {
		final TreePath[] selectedPaths = getSelection().getPaths(
				getRootAlgorithm(), getLinkDatabase());

		JMenuItem collapseItem = new JMenuItem(
				"Collapse all children of selection");

		final Vector<TreePath> collapsePaths = new Vector<TreePath>();
		for (int i = 0; i < selectedPaths.length; i++) {
			if (isExpanded(selectedPaths[i]))
				collapsePaths.add(selectedPaths[i]);
		}
		collapseItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {

				for (int i = 0; i < collapsePaths.size(); i++)
					SwingUtil.collapseTree(OBOTermPanel.this, collapsePaths
							.get(i));

				TreePath rootPath = new TreePath(getModel().getRoot());
				expandPath(rootPath);
			}
		});
		collapseItem.setEnabled(collapsePaths.size() > 0);
		menu.add(collapseItem);

		JMenuItem lockItem = new JMenuItem("Lock view");
		lockItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setLockedPath(selectedPaths[0]);
			}
		});
		lockItem.setEnabled(selectedPaths.length == 1
				&& !selectedPaths[0].equals(lockedPath));
		menu.add(lockItem);

		JMenuItem unlockItem = new JMenuItem("Unlock view");
		unlockItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setLockedPath(null);
			}
		});
		unlockItem.setEnabled(lockedPath != null);
		menu.add(unlockItem);

		menu.addSeparator();

		Vector v = getFilterMenuItems();
		for (int i = 0; i < v.size(); i++) {
			JMenuItem item = (JMenuItem) v.get(i);
			menu.add(item);
		}

	}

	public void setDrawArrowhead(boolean drawArrowhead) {
		if (getUI() instanceof DragFriendlyTreeUI) {
			DragFriendlyTreeUI ui = (DragFriendlyTreeUI) getUI();
			ui.setDrawArrowhead(drawArrowhead);
		}
	}

	protected DragFriendlyTreeUI getDefaultUI() {
		DragFriendlyTreeUI ui = new OBOTermPanelUI();
		return ui;
	}

	public void setToolTips() {

		if (Preferences.getPreferences().getShowToolTips())
			ToolTipManager.sharedInstance().registerComponent(this);
		else
			ToolTipManager.sharedInstance().unregisterComponent(this);
	}

	public TreePath[] getExpandedPaths() {
		TreePath[] out = new TreePath[getRowCount()];
		for (int i = 0; i < out.length; i++)
			out[i] = getPathForRow(i);
		return out;
	}

	public boolean pathIsValid(TreePath current) {
		return PathUtil.pathIsValid(current, getModel());
		/*
		 * Object [] objects = current.getPath(); // only expand paths that are
		 * actually in the model // otherwise some JTree implementations will
		 * become corrupted boolean allowExpand = true; for(int j=0; j <
		 * objects.length; j++) { if (!(objects[j] instanceof Link)) continue;
		 * Link old = (Link) objects[j];
		 * 
		 * if (TermUtil.findChildRel(old, old.getParent()) == null) {
		 * allowExpand = false; break; } } return allowExpand;
		 */
	}

	public void synchronize(OBOTermPanel panel) {
		TreePath rootPath = new TreePath(TermModel.ROOT);
		Enumeration e = panel.getExpandedDescendants(rootPath);
		while (e.hasMoreElements()) {
			expandPath((TreePath) e.nextElement());
		}
		setSelectionPaths(panel.getSelectionPaths());
	}

	protected static class SingletonEnumeration implements Enumeration {
		protected Object object;

		public SingletonEnumeration() {
		}

		public void setObject(Object object) {
			this.object = object;
		}

		public boolean hasMoreElements() {
			return object != null;
		}

		public Object nextElement() {
			Object out = object;
			object = null;
			return out;
		}
	}

	protected SingletonEnumeration singletonEnumeration = new SingletonEnumeration();

	public TreePath[] getSelectionPaths() {
		TreePath[] out = super.getSelectionPaths();
		if (out == null) {
			out = new TreePath[0];
		}
		return out;
	}

	public void reload() {
		System.err.println("reloading OBO Term Panel...");
		long time = System.currentTimeMillis();
		TreeSelectionListener[] selectionListeners = getTreeSelectionListeners();
		for (int i = 0; i < selectionListeners.length; i++) {
			System.err.println("removing selection listener "
					+ selectionListeners[i]);
			removeTreeSelectionListener(selectionListeners[i]);
		}

		TreePath[] expanded = null;
		TreePath[] selected = getSelectionPaths();

		expanded = getExpandedPaths();
		if (lockedPath != null && !pathIsValid(lockedPath))
			setLockedPath(null);

		TreeModel model = getModel();

		if (model instanceof TermModel) {
			((TermModel) model).reload();
		}

		long time2 = System.currentTimeMillis();
		restorePaths(expanded);
		if (selected != null)
			restoreSelectionPaths(selected);
		time2 = System.currentTimeMillis() - time2;
		for (int i = 0; i < selectionListeners.length; i++) {
			System.err.println("adding selection listener "
					+ selectionListeners[i]);
			addTreeSelectionListener(selectionListeners[i]);
		}
		System.err.println("reloaded in " + (System.currentTimeMillis() - time)
				+ " (expanding took " + time2 + " ms)");
	}

	public void restorePaths(TreePath[] expanded) {
		for (int i = 0; i < expanded.length; i++) {
			TreePath current = expanded[i];
			if (PathUtil.pathIsValid(current, getModel())) {
				makeVisible(current);
			}
		}
	}

	public void restoreSelectionPaths(TreePath[] expanded) {
		for (int i = 0; i < expanded.length; i++) {
			TreePath current = expanded[i];
			if (PathUtil.pathIsValid(current, getModel())) {
				addSelectionPath(current);
			}
		}
	}

	protected int[] getSortedSelectionRows() {
		int[] arr = getSelectionRows();
		if (arr == null)
			return new int[0];
		if (arr.length < 2)
			return arr;
		sort(arr);
		return arr;
	}

	@Override
	public void setModel(TreeModel model) {
		super.setModel(model);
		reload();
	}

	protected Vector getFilterMenuItems() {
		Vector<JMenuItem> v = new Vector<JMenuItem>();

		JMenuItem removeAllDecorationAndFilters = new JMenuItem(
				"Remove all renderers and filters");
		removeAllDecorationAndFilters.setEnabled(objectRenderers.size() > 0
				|| getFilter() != null);
		removeAllDecorationAndFilters.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				objectRenderers.clear();
				if (getFilter() != null) {
					setFilter(null);
					reload();
				}
				repaint();
			}
		});
		v.add(removeAllDecorationAndFilters);

		JMenuItem removeAllDecoration = new JMenuItem("Remove all renderers");
		removeAllDecoration.setEnabled(objectRenderers.size() > 0);
		removeAllDecoration.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				objectRenderers.clear();
				// reload();
				repaint();
			}
		});
		v.add(removeAllDecoration);

		JMenu renderMenu = new JMenu("Remove specific renderer");
		renderMenu.setEnabled(objectRenderers.size() > 0);
		Iterator it = objectRenderers.iterator();
		while (it.hasNext()) {
			final FilterPair fr = (FilterPair) it.next();

			JMenuItem ritem = new JMenuItem("Remove " + fr.toString());
			ritem.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					RenderedFilter f = fr.getLinkRenderer();
					if (f != null)
						removeLinkRenderer(f);
					f = fr.getObjectRenderer();
					if (f != null)
						removeObjectRenderer(f);
				}
			});
			renderMenu.add(ritem);
		}
		v.add(renderMenu);

		JMenuItem filterMenuItem = new JMenuItem("Remove filter");
		filterMenuItem.setEnabled(getFilter() != null);
		filterMenuItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setFilter(null);
			}
		});
		v.add(filterMenuItem);

		return v;
	}

	protected static void sort(int[] arr) {
		quicksort(arr, 0, arr.length - 1);
	}

	private static void quicksort(int[] a, int low, int high) {
		if (low < high) {
			int pivot = a[high];
			int smallSide = low;
			int largeSide = high - 1;

			while (smallSide < largeSide) {
				while (a[smallSide] < pivot)
					smallSide++;
				while (largeSide >= 0 && pivot <= a[largeSide])
					largeSide--;
				if (smallSide < largeSide) {
					int temp = a[smallSide];
					a[smallSide] = a[largeSide];
					a[largeSide] = temp;
				}
			}

			int middle;
			if (pivot < a[smallSide]) {
				int temp = a[smallSide];
				a[smallSide] = a[high];
				a[high] = temp;
				middle = smallSide;
			} else {
				middle = high;
			}

			quicksort(a, low, middle - 1);
			quicksort(a, middle + 1, high);
		}
	}

	public JComponent getComponent() {
		return scrollPane;
	}

	public ComponentConfiguration getConfiguration() {
		OBOPanelConfiguration config = new OBOPanelConfiguration();

		config.setLive(isLive());

		config.setFilter(getFilter());

		if (getModel() instanceof DefaultTermModel) {
			OBOProperty filterProperty = ((DefaultTermModel) getModel())
					.getPropertyFilter();
			if (filterProperty == null)
				config.setFilterPropID(null);
			else
				config.setFilterPropID(filterProperty.getID());
		}

		if (getRootAlgorithm() == RootAlgorithm.STRICT)
			config.setRootAlgorithm("STRICT");
		else if (getRootAlgorithm() == RootAlgorithm.GREEDY)
			config.setRootAlgorithm("GREEDY");
		return config;
	}

	public void setConfiguration(ComponentConfiguration c) {
		if (c instanceof OBOPanelConfiguration) {
			OBOPanelConfiguration config = (OBOPanelConfiguration) c;
			setLive(config.isLive());
			if (config.getFilter() != null)
				setFilter(config.getFilter());
			if (config.getRootAlgorithm() != null) {
				if (config.getRootAlgorithm().equals("STRICT")) {
					setRootAlgorithm(RootAlgorithm.STRICT);
				} else if (config.getRootAlgorithm().equals("GREEDY")) {
					setRootAlgorithm(RootAlgorithm.GREEDY);
				}
			}
			if (config.getFilterPropID() == null)
				setPropertyFilter(null);
			else {
				IdentifiedObject obj = sessionManager.getSession().getObject(
						config.getFilterPropID());
				OBOProperty type = (OBOProperty) obj;
				setPropertyFilter(type);
			}
		}
	}

	public void setRootAlgorithm(RootAlgorithm algorithm) {
		if (getModel() instanceof DefaultTermModel) {
			((DefaultTermModel) getModel()).setRootAlgorithm(algorithm);
			reload();
		}
	}

	public boolean isSingleton() {
		return false;
	}

	protected DropTarget dropTarget = new DropTarget(this, inputListener);

	protected SelectionTransferHandler selectionTransferHandler;

	public void init() {
		selectionTransferHandler = SelectionTransferHandler
				.installHandler(this);
		setDropTarget(dropTarget);
		attachListeners();
		setToolTips();
		setCellRenderer(new OBOCellRenderer());
	}

	public String getID() {
		return id;
	}

	public void setXML(String xml) {
	}

	public boolean hasCombinedTermsAndLinks() {
		return true;
	}

	public boolean isXMLSettable() {
		return false;
	}

	public String getTitle() {
		if (title == null)
			return getID();
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public void addLinkRenderer(RenderedFilter renderer) {
		linkRenderers.add(renderer);
	}

	public List<RenderedFilter> getLinkRenderers() {
		return linkRenderers;
	}

	public void removeLinkRenderer(RenderedFilter renderer) {
		linkRenderers.remove(renderer);
	}
}