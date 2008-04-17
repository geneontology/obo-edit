package org.oboedit.gui.components;

import java.awt.*;
import java.awt.dnd.Autoscroll;
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
import javax.swing.plaf.basic.BasicTreeUI;
import javax.swing.tree.*;

import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.ConfigurationPanel;
import org.bbop.framework.GUIManager;
import org.bbop.framework.GUIComponent;
import org.bbop.swing.*;
import org.bbop.swing.plaf.DragFriendlyTreeUI;
import org.bbop.util.CollectionUtil;
import org.obo.datamodel.*;
import org.obo.datamodel.impl.*;
import org.obo.filters.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.EditActionManager;
import org.oboedit.controller.SelectionManager;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.AbstractInputHandlerBridge;
import org.oboedit.gui.AbstractSelectableHandlerBridge;
import org.oboedit.gui.DefaultTermModel;
import org.oboedit.gui.EditActionToolbar;
import org.oboedit.gui.Filterable;
import org.oboedit.gui.FilteredRenderable;
import org.oboedit.gui.GestureTarget;
import org.oboedit.gui.HTMLNodeLabelProvider;
import org.oboedit.gui.InputHandlerI;
import org.oboedit.gui.NodeLabelProvider;
import org.oboedit.gui.OBOCellRenderer;
import org.oboedit.gui.ObjectSelector;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.RightClickMenuProvider;
import org.oboedit.gui.Selection;
import org.oboedit.gui.SelectionTransferHandler;
import org.oboedit.gui.TermModel;
import org.oboedit.gui.event.*;
import org.oboedit.gui.filter.BackgroundColorSpecField;
import org.oboedit.gui.filter.RenderedFilter;
import org.oboedit.util.GUIUtil;
import org.oboedit.util.PathUtil;

public class OBOTermPanel extends JTree implements ObjectSelector,
		FilteredRenderable, Filterable, GUIComponent, DragImageGenerator,
		RightClickMenuProvider, Autoscroll {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected String title;

	protected static final int AUTOSCROLL_MARGIN = 12;
	public static Border EMPTY_BORDER = new EmptyBorder(1, 1, 1, 1);

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

	protected DragSourceDragEvent currentDragEvent;

	protected String dragTitle = "";

	protected java.util.List<RenderedFilter> objectRenderers = new ArrayList<RenderedFilter>();

	protected java.util.List<RenderedFilter> automaticObjectRenderers = new ArrayList<RenderedFilter>();

	protected java.util.List<RenderedFilter> linkRenderers = new ArrayList<RenderedFilter>();

	protected SessionManager sessionManager = SessionManager.getManager();

	protected static final Color lockGray = new Color(200, 200, 200);

	protected static final Color secondaryGray = new Color(240, 240, 240);

	final static int HEADER_HEIGHT = 20;

	final static int HEADER_MARGIN = 2;

	protected String id;

	protected boolean revertToDefaultAction = true;

	protected EditActionToolbar toolbar;

	protected NodeLabelProvider nodeLabelProvider;

	protected List<SelectionListener> selectionListeners = new ArrayList<SelectionListener>();
	protected List<ExpandCollapseListener> expansionListeners = new ArrayList<ExpandCollapseListener>();

	protected JScrollPane scrollPane;

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
				TreePath rootPath = new TreePath(PathUtil.ROOT);
				Iterator it = roots.iterator();
				while (it.hasNext()) {
					OBORestriction tr = new OBORestrictionImpl(
							(LinkedObject) it.next());

					TreePath path = rootPath.pathByAddingChild(tr);
					if (expansionBridge != null) {
						removeTreeExpansionListener(expansionBridge);
						removeTreeWillExpandListener(expansionBridge);
					}
					expandPath(path);
					if (expansionBridge != null) {
						addTreeExpansionListener(expansionBridge);
						addTreeWillExpandListener(expansionBridge);
					}
				}
			}
			// TODO Auto-generated method stub

		}
	};

	public ConfigurationPanel getConfigurationPanel() {
		return new OntologyEditorConfigEditor();
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

	protected class ExpansionBridge implements TreeExpansionListener,
			TreeWillExpandListener {

		protected Collection<PathCapable> oldVisibles;

		public void treeCollapsed(TreeExpansionEvent event) {
			fireEvent();
			oldVisibles = null;
		}

		public void treeExpanded(TreeExpansionEvent event) {
			fireEvent();
			oldVisibles = null;
		}

		protected void fireEvent() {
			if (expansionListeners.size() < 1)
				return;
			Collection<PathCapable> visible = getVisibleObjects();
			Iterator<PathCapable> it = oldVisibles.iterator();
			while (it.hasNext()) {
				PathCapable old = it.next();
				if (visible.remove(old))
					it.remove();
			}
			fireExpansionStateChanged(CollectionUtil.getObjectsOfType(visible,
					IdentifiedObject.class), CollectionUtil.getObjectsOfType(
					oldVisibles, IdentifiedObject.class));
		}

		public void treeWillCollapse(TreeExpansionEvent event)
				throws ExpandVetoException {
			if (expansionListeners.size() < 1)
				return;
			oldVisibles = getVisibleObjects();
		}

		public void treeWillExpand(TreeExpansionEvent event)
				throws ExpandVetoException {
			if (expansionListeners.size() < 1)
				return;
			oldVisibles = getVisibleObjects();
		}

	}

	ExpansionBridge expansionBridge = new ExpansionBridge();

	protected boolean isLive = true;

	public Icon getImage(DragSourceDragEvent e) {
		return new ImageIcon(getDragImage(getSelectionRows()));
	}

	protected void fireExpansionStateChanged(
			Collection<IdentifiedObject> shown,
			Collection<IdentifiedObject> hidden) {
		ExpansionEvent e = null;
		int size = expansionListeners.size();
		for (int i = 0; i < size && i < expansionListeners.size(); i++) {
			ExpandCollapseListener listener = (ExpandCollapseListener) expansionListeners
					.get(i);
			if (e == null) {
				e = new ExpansionEvent(this, shown, hidden);
			}
			listener.expandStateChanged(e);
		}
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

	protected class InputListener extends AbstractSelectableHandlerBridge {

		protected HistoryListener historyListener = new HistoryListener() {

			public void applied(HistoryAppliedEvent event) {
				updateFromHistory();
			}

			public void reversed(HistoryAppliedEvent event) {
				updateFromHistory();
			}

		};

		public InputListener() {
			setComponent(OBOTermPanel.this);
		}

		public void install() {
			SessionManager.getManager().addHistoryListener(historyListener);
		}

		public void uninstall() {
			SessionManager.getManager().removeHistoryListener(historyListener);
		}

		protected void updateFromHistory() {
			if (revertToDefaultAction) {
				toolbar.setCurrentHandler(EditActionManager.getManager()
						.getDefaultInputHandler());
			}
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
		public void setCurrentHandler(InputHandlerI handler) {
			super.setCurrentHandler(handler);
			if (handler != null)
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
				if (e.isConsumed() || !SwingUtilities.isLeftMouseButton(e)) {
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
									&& (path.getLastPathComponent() instanceof Link)) {
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
			TreePath[] selected = getSelectionPaths();
			super.selectPathForEvent(path, event);
			if (!SelectionManager.getManager().doPreSelectValidation(
					getSelection())) {
				setSelectionPaths(selected);
			} else {
				fireSelectionEvent();
				if (isLive())
					SelectionManager.setGlobalSelection(getSelection());
			}
		}

		@Override
		protected boolean isToggleEvent(MouseEvent event) {
			return false;
		}

		@Override
		public void installUI(JComponent c) {
			// TODO Auto-generated method stub
			super.installUI(c);
		}
	}

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
		}
	}

	public LinkDatabase getLinkDatabase() {
		return ((TermModel) getModel()).getLinkDatabase();
	}

	public RootAlgorithm getRootAlgorithm() {
		return ((TermModel) getModel()).getRootAlgorithm();
	}

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

	public void setLinkFilter(Filter<?> linkFilter) {
		if (getModel() instanceof TermModel) {
			((TermModel) getModel()).setLinkFilter(linkFilter);
			reload();
		}
	}

	public void setTermFilter(Filter<?> termFilter) {
		if (getModel() instanceof TermModel) {
			((TermModel) getModel()).setTermFilter(termFilter);
			reload();
		}
	}

	public void setFilters(Filter<?> termFilter, Filter<?> linkFilter) {
		if (getModel() instanceof TermModel) {
			((TermModel) getModel()).setLinkFilter(linkFilter);
			((TermModel) getModel()).setTermFilter(termFilter);
			reload();
		}
	}

	public Filter<?> getLinkFilter() {
		if (getModel() instanceof TermModel)
			return ((TermModel) getModel()).getLinkFilter();
		else
			return null;
	}

	public Filter<?> getTermFilter() {
		if (getModel() instanceof TermModel)
			return ((TermModel) getModel()).getTermFilter();
		else
			return null;
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
				EditActionManager.getManager().getKeyRecorder()
						.removeKeyListener(focusKeyListener);
			}
		});
		addMouseListener(inputListener);
		addKeyListener(inputListener);
		inputListener.install();
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
		inputListener.uninstall();
		Preferences.getPreferences().removeReconfigListener(reconfigListener);
		GUIUtil.removeReloadListener(reloadListener);
		SelectionManager.getManager()
				.removeSelectionListener(selectionListener);

		SelectionTransferHandler.removeHandler(selectionTransferHandler);
		setDropTarget(null);
	}

	protected JPanel panel;

	// protected JToolBar toolbar;
	// protected JComboBox gestureBox = new JComboBox();

	protected boolean bridgeEnabled = false;

	public OBOTermPanel(String id) {
		this.id = id;
		setShowsRootHandles(true);
		setRootVisible(false);
		setNodeLabelProvider(new HTMLNodeLabelProvider(this, CollectionUtil
				.list(BackgroundColorSpecField.FIELD), null, null));
		/*
		 * addFocusListener(new FocusAdapter() { @Override public void
		 * focusGained(FocusEvent e) {
		 * Controller.getController().setPrimarySelector(OBOTermPanel.this); }
		 * });
		 */
//		setFont(Preferences.getPreferences().getFont()); // doesn't do anything--need to set font in OBOCellRenderer
		scrollPane = new JScrollPane(this);
		panel = new JPanel();
		panel.setLayout(new BorderLayout());
		panel.add(scrollPane, "Center");
		toolbar = new EditActionToolbar(panel, inputListener, true);
		DragFriendlyTreeUI ui = getDefaultUI();
		setUI(ui);
		ui.setRightChildIndent(8);

		DefaultTermModel model = new DefaultTermModel();
		setModel(model);
		model.init();
		setActionMap(new ActionMap());
	}

	// protected boolean showToolbar = false;

	@Override
	public void setUI(TreeUI ui) {
		if (!(ui instanceof OBOTermPanelUI))
			super.setUI(getDefaultUI());
		else
			super.setUI(ui);
		setRowHeight(0);
	}

	public Selection getSelection(MouseEvent e) {
		return getSelection();
	}

	public Selection getSelection() {
		TreePath[] paths = getSelectionPaths();
		TreePath lead = getLeadSelectionPath();
		return createSelectionFromPaths(lead, paths);
	}

    // NOT static.
	protected Selection createSelectionFromPaths(TreePath lead,
			TreePath[] paths) {
		if (paths == null || paths.length == 0)
			return SelectionManager.createEmptySelection();
		Link leadLink = null;
		if (lead != null) {
			leadLink = (Link) lead.getLastPathComponent();
			int leadIndex = -1;
			for (int i = 0; i < paths.length; i++) {
				if (paths[i] == lead) {
					leadIndex = i;
					break;
				}
			}
			if (leadIndex > 0) {
				TreePath temp = paths[0];
				paths[0] = lead;
				paths[leadIndex] = temp;
			}
		}
		// createSelectionFromPaths was being called with null as the
		// first arg because when this method was (incorrectly) made a
		// static method it didn't have access to "this".  Not sure
		// why it was done that way.
//		return SelectionManager.createSelectionFromPaths(null, paths, leadLink,
		return SelectionManager.createSelectionFromPaths(this, paths, leadLink,
				SessionManager.getManager().getCurrentLinkDatabase(),
				RootAlgorithm.GREEDY, true);
	}

	public void select(Selection selection) {

		TreePath[] paths = selection.getPaths(getRootAlgorithm(),
				getLinkDatabase());
		setSelectionPaths(paths);
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
		TreePath rootPath = new TreePath(PathUtil.ROOT);
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
//		System.err.println("Reloading OBO Term Panel...");
		long time = System.currentTimeMillis();
		TreeSelectionListener[] selectionListeners = getTreeSelectionListeners();
		for (int i = 0; i < selectionListeners.length; i++) {
//			System.err.println("removing selection listener " + selectionListeners[i]);
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
		clearToggledPaths();

//		long time2 = System.currentTimeMillis();
		restorePaths(expanded);
		if (selected != null)
			restoreSelectionPaths(selected);
//		time2 = System.currentTimeMillis() - time2;
		for (int i = 0; i < selectionListeners.length; i++) {
//			System.err.println("adding selection listener " + selectionListeners[i]);
			addTreeSelectionListener(selectionListeners[i]);
		}
//		System.err.println("reloaded in " + (System.currentTimeMillis() - time)
//				+ " (expanding took " + time2 + " ms)");
	}

	public void restorePaths(TreePath[] expanded) {
		if (expansionBridge != null) {
			removeTreeExpansionListener(expansionBridge);
			removeTreeWillExpandListener(expansionBridge);
		}
		for (int i = 0; i < expanded.length; i++) {
			TreePath current = expanded[i];
			TreePath newPath = PathUtil.reconstruct(current, getModel());
			if (newPath != null)
				makeVisible(newPath);
		}
		if (expansionBridge != null) {
			addTreeExpansionListener(expansionBridge);
			addTreeWillExpandListener(expansionBridge);
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
				|| getLinkFilter() != null || getTermFilter() != null);
		removeAllDecorationAndFilters.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				objectRenderers.clear();
				if (getLinkFilter() != null || getTermFilter() != null) {
					setLinkFilter(null);
					setTermFilter(null);
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
		Iterator<RenderedFilter> it = objectRenderers.iterator();
		while (it.hasNext()) {
			final RenderedFilter fr = it.next();

			JMenuItem ritem = new JMenuItem("Remove " + fr.toString());
			ritem.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					removeObjectRenderer(fr);
				}
			});
			renderMenu.add(ritem);
		}
		it = linkRenderers.iterator();
		while (it.hasNext()) {
			final RenderedFilter fr = it.next();

			JMenuItem ritem = new JMenuItem("Remove " + fr.toString());
			ritem.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					removeLinkRenderer(fr);
				}
			});
			renderMenu.add(ritem);
		}
		v.add(renderMenu);

		JMenuItem linkFilterMenuItem = new JMenuItem("Remove link filter");
		linkFilterMenuItem.setEnabled(getLinkFilter() != null);
		linkFilterMenuItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setLinkFilter(null);
			}
		});
		v.add(linkFilterMenuItem);

		JMenuItem termFilterMenuItem = new JMenuItem("Remove term filter");
		termFilterMenuItem.setEnabled(getTermFilter() != null);
		termFilterMenuItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setTermFilter(null);
			}
		});
		v.add(termFilterMenuItem);

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
		// return scrollPane;
		return panel;
	}

	public ComponentConfiguration getConfiguration() {
		String rootStr = null;
		if (getRootAlgorithm() == RootAlgorithm.STRICT)
			rootStr = "STRICT";
		else if (getRootAlgorithm() == RootAlgorithm.GREEDY)
			rootStr = "GREEDY";
		String basicHTML = null;
		if (getNodeLabelProvider() instanceof HTMLNodeLabelProvider) {
			basicHTML = ((HTMLNodeLabelProvider) getNodeLabelProvider())
					.getHtmlExpression();
		}
		InputHandlerI currentHandler = toolbar.getCurrentHandler();
		return new OntologyEditorConfiguration(getTermFilter(),
				getLinkFilter(), getObjectRenderers(), getLinkRenderers(),
				basicHTML, toolbar.getShowToolbar(), toolbar
						.getToolbarPosition(),
				(currentHandler != null ? currentHandler.getID() : null),
				isRevertToDefaultAction(), isLive(), rootStr);
	}

	public void setConfiguration(ComponentConfiguration c) {
		if (c instanceof OntologyEditorConfiguration) {
			OntologyEditorConfiguration config = (OntologyEditorConfiguration) c;
			setLive(config.isLive());
			setLinkFilter(config.getLinkFilter());
			setTermFilter(config.getTermFilter());
			setObjectRenderers(config.getObjectRenderers());
			setLinkRenderers(config.getLinkRenderers());
			if (getNodeLabelProvider() instanceof HTMLNodeLabelProvider) {
				((HTMLNodeLabelProvider) getNodeLabelProvider())
						.setHtmlExpression(config.getBasicHTML());
			}
			if (config.getRootAlgorithm() != null) {
				if (config.getRootAlgorithm().equals("STRICT")) {
					setRootAlgorithm(RootAlgorithm.STRICT);
				} else if (config.getRootAlgorithm().equals("GREEDY")) {
					setRootAlgorithm(RootAlgorithm.GREEDY);
				}
			}
			toolbar.setShowToolbar(config.getShowToolbar());
			setRevertToDefaultAction(config.isRevertToDefaultAction());
			toolbar.setCurrentHandler(EditActionManager.getManager()
					.getInputHandler(config.getDragActionID()));
			toolbar.setToolbarPosition(config.getToolbarPosition());
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
	// protected String toolbarPosition = BorderLayout.NORTH;

	protected SelectionTransferHandler selectionTransferHandler;

	public void init() {
		selectionTransferHandler = SelectionTransferHandler
				.installHandler(this);
		setDropTarget(dropTarget);
		attachListeners();
		setToolTips();
		setCellRenderer(new OBOCellRenderer());

		toolbar.updateGestureList();
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

	public boolean isRevertToDefaultAction() {
		return revertToDefaultAction;
	}

	public void setRevertToDefaultAction(boolean revertToDefaultAction) {
		this.revertToDefaultAction = revertToDefaultAction;
		inputListener.setRevertToDefaultAction(revertToDefaultAction);
	}

	public void setLinkRenderers(List<RenderedFilter> renderers) {
		this.linkRenderers = renderers;
	}

	public void setObjectRenderers(List<RenderedFilter> renderers) {
		this.objectRenderers = renderers;
	}

	public NodeLabelProvider getNodeLabelProvider() {
		return nodeLabelProvider;
	}

	public void setNodeLabelProvider(NodeLabelProvider nodeLabelProvider) {
		this.nodeLabelProvider = nodeLabelProvider;
	}

	// Ok, we’ve been told to scroll because the mouse cursor is in our
	// scroll zone.
	public void autoscroll(Point p) {
		JViewport viewport = SwingUtil
				.getAncestorOfClass(JViewport.class, this);
		if (viewport != null) {
			Rectangle outer = getBounds();
			boolean top = p.y + outer.y <= AUTOSCROLL_MARGIN;
			Rectangle viewRect = viewport.getViewRect();
			if (top)
				viewRect.y -= AUTOSCROLL_MARGIN;
			else
				viewRect.y += AUTOSCROLL_MARGIN;
			if (viewRect.y < 0)
				viewRect.y = 0;
			if (viewRect.y + viewRect.height > getHeight())
				viewRect.y = getHeight() - viewRect.height;
			viewport.setViewPosition(viewRect.getLocation());
		}
	}

	// Calculate the insets for the *JTREE*, not the viewport
	// the tree is in. This makes it a bit messy.
	public Insets getAutoscrollInsets() {
		Rectangle outer = getBounds();
		Rectangle inner = getParent().getBounds();
		return new Insets(inner.y - outer.y + AUTOSCROLL_MARGIN, inner.x
				- outer.x + AUTOSCROLL_MARGIN, outer.height - inner.height
				- inner.y + outer.y + AUTOSCROLL_MARGIN, outer.width
				- inner.width - inner.x + outer.x + AUTOSCROLL_MARGIN);
	}

	public void addAutomaticObjectRenderer(RenderedFilter renderer) {
		automaticObjectRenderers.add(renderer);
	}

	public void removeAutomaticObjectRenderer(RenderedFilter renderer) {
		automaticObjectRenderers.remove(renderer);
	}

	public List<RenderedFilter> getAutomaticObjectRenderers() {
		return automaticObjectRenderers;
	}

	public Collection<PathCapable> getVisibleObjects() {
		Collection<PathCapable> out = new HashSet<PathCapable>();
		for (int i = 0; i < getRowCount(); i++) {
			TreePath path = getPathForRow(i);
			Object o = path.getLastPathComponent();
			if (o instanceof Link) {
				Link link = (Link) o;
				out.add(link);
				if (link.getParent() != null)
					out.add(link.getParent());
				if (link.getChild() != null)
					out.add(link.getChild());
			}
		}
		return out;
	}

	public void addExpansionListener(ExpandCollapseListener listener) {
		expansionListeners.add(listener);
	}

	public void removeExpansionListener(ExpandCollapseListener listener) {
		expansionListeners.remove(listener);
	}

	public void redraw() {
		reload();
	}
}
