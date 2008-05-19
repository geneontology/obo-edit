package org.bbop.swing;

import java.awt.*;
import java.awt.geom.*;
import java.awt.font.*;
import java.awt.image.*;
import javax.swing.*;
import javax.swing.plaf.*;
import javax.swing.tree.*;
import java.lang.reflect.*;

import org.bbop.util.*;

import java.util.*;
import java.io.*;

/**
 * A pile of useful methods for Swing applications.
 */
import org.apache.log4j.*;

public class SwingUtil {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SwingUtil.class);

	protected SwingUtil() {
		super();
	}

	public static int NONE = 0;

	public static int TOP = 1;

	public static int BOTTOM = 2;

	public static int LEFT = 4;

	public static int RIGHT = 8;

	public static String getComboBoxValue(JComboBox comp) {
		String selectedPath = comp.getSelectedItem().toString();
		if (selectedPath == null
				|| !selectedPath.equals(comp.getEditor().getItem())) {
			selectedPath = comp.getEditor().getItem().toString();
		}
		return selectedPath;
	}

	/**
	 * Convenience method for searching above <code>comp</code> in the
	 * component hierarchy and returns the first object of class <code>c</code>
	 * it finds. Can return null, if a class <code>c</code> cannot be found.
	 */
	public static Component getShallowestAncestorOfClass(Class<?> c,
			Component comp) {
		if (comp == null || c == null)
			return null;

		Component parent = comp;
		Component match = null;
		while (parent != null) {
			if (c.isInstance(parent))
				match = parent;
			parent = parent.getParent();
		}
		return match;
	}

	public static void setPreferredWidth(JComponent component, int width) {
		component.setPreferredSize(new Dimension(width, component
				.getPreferredSize().height));
	}

	public static void setPreferredHeight(JComponent component, int height) {
		component.setPreferredSize(new Dimension(
				component.getPreferredSize().width, height));
	}

	public static String getHTMLFontStyle(Font font) {
		StringBuffer out = new StringBuffer();
		out.append("font-size: " + font.getSize() + "px; ");
		out.append("font-family: " + font.getSize() + "; ");
		if (font.isItalic())
			out.append("font-style: italic; ");
		if (font.isBold())
			out.append("font-weight: bold; ");
		return out.toString();
	}

	public static int withinInsets(Point p, Insets insets, Component c) {
		if (insets == null || p == null || c == null || !c.contains(p))
			return NONE;
		int out = 0;
		if (p.x < insets.left)
			out = out | LEFT;
		else if (p.x > c.getSize().width - insets.right)
			out = out | RIGHT;
		if (p.y < insets.bottom)
			out = out | TOP;
		else if (p.y > c.getSize().height - insets.top)
			out = out | BOTTOM;

		return out;
	}

	public static void mapAction(JComponent c, int condition,
			KeyStroke keyStroke, Action action) {
		Object key = action.getValue(Action.NAME);
		if (key == null)
			throw new IllegalArgumentException("The provided action must "
					+ "have a name defined");
		mapAction(c, condition, key, keyStroke, action);
	}

	public static void mapAction(JComponent c, int condition, Object key,
			KeyStroke keyStroke, Action action) {
		c.getActionMap().put(key, action);
		c.getInputMap(condition).put(keyStroke, key);
	}

	/**
	 * Generic-ified version of SwingUtilities.getAncestorOfClass()
	 */
	public static <T> T getAncestorOfClass(Class<T> c, Component comp) {
		if (comp == null || c == null)
			return null;

		Container parent = comp.getParent();
		while (parent != null && !(c.isInstance(parent)))
			parent = parent.getParent();
		return (T) parent;
	}

	public static Image getImage(Component c) {
		GraphicsEnvironment ge = GraphicsEnvironment
				.getLocalGraphicsEnvironment();
		GraphicsDevice gd = ge.getDefaultScreenDevice();
		GraphicsConfiguration gc = gd.getDefaultConfiguration();

		// Create an image that supports transparent pixels
		BufferedImage bImage = gc.createCompatibleImage(c.getWidth(), c
				.getHeight(), Transparency.BITMASK);

		/*
		 * And now this is how we get an image of the component
		 */
		Graphics2D g = bImage.createGraphics();

		// Then use the current component we're in and call paint on this
		// graphics object
		c.paint(g);
		return bImage;
	}

	public static void masterValidate(Component c) {
		JRootPane pane = SwingUtilities.getRootPane(c);
		if (pane != null)
			pane.getContentPane().validate();
	}

	public static boolean isVectorFont(Font font) {
		GlyphVector gv = font.createGlyphVector(new FontRenderContext(null,
				true, false), "Test");
		Shape fontShape = gv.getOutline();
		Rectangle2D bounds = fontShape.getBounds2D();
		return !(bounds.getWidth() == 0 && bounds.getHeight() == 0);
	}

	public static boolean isChildPath(TreePath child, TreePath parent) {
		Object[] childArr = child.getPath();
		Object[] parentArr = parent.getPath();
		if (childArr.length == 0 || childArr.length > parentArr.length)
			return false;
		for (int i = 0; i < childArr.length; i++)
			if (!childArr[i].equals(parentArr[i]))
				return false;
		return true;
	}

	public static int getIndex(Container parent, Component child) {
		for (int i = 0; i < parent.getComponentCount(); i++) {
			if (child.equals(parent.getComponent(i)))
				return i;
		}
		return -1;
	}

	public static void printTreeModel(TreeModel model) {
		printTreeModel(System.err, model);
	}

	public static void printTreeModel(PrintStream out, TreeModel model) {
		printTreeModel(out, model.getRoot(), 0, model, new Hashtable());
	}

	private static void printTreeModel(PrintStream out, Object current,
			int depth, TreeModel model, Hashtable seen) {
		out.print(StringUtil.repeat(" ", depth));
		out.println(current);
		if (seen.containsKey(current))
			return;
		else
			seen.put(current, current);
		int count = model.getChildCount(current);
		for (int i = 0; i < count; i++) {
			printTreeModel(out, model.getChild(current, i), depth + 2, model,
					seen);
		}
	}
	
	
	/*
	 * Returns the bounding rectangle for the component text.
	 */
	public static Rectangle getTextRectangle(JLabel label) {

		String text = label.getText();
		Icon icon = (label.isEnabled()) ? label.getIcon() : label
				.getDisabledIcon();

		if ((icon == null) && (text == null)) {
			return null;
		}

		Rectangle paintIconR = new Rectangle();
		Rectangle paintTextR = new Rectangle();
		Rectangle paintViewR = new Rectangle();
		Insets paintViewInsets = new Insets(0, 0, 0, 0);

		paintViewInsets = label.getInsets(paintViewInsets);
		paintViewR.x = paintViewInsets.left;
		paintViewR.y = paintViewInsets.top;
		paintViewR.width = label.getWidth()
				- (paintViewInsets.left + paintViewInsets.right);
		paintViewR.height = label.getHeight()
				- (paintViewInsets.top + paintViewInsets.bottom);

		Graphics g = label.getGraphics();
		if (g == null) {
			return null;
		}
		String clippedText = SwingUtilities.layoutCompoundLabel(
				(JComponent) label, g.getFontMetrics(), text, icon, label
						.getVerticalAlignment(),
				label.getHorizontalAlignment(),
				label.getVerticalTextPosition(), label
						.getHorizontalTextPosition(), paintViewR, paintIconR,
				paintTextR, label.getIconTextGap());

		return paintTextR;
	}

	/**
	 * Places a Window in the center of the screen.
	 * 
	 * @param win
	 *            the window to center
	 */
	public static void center(Window win) {
		Dimension screenSize = win.getToolkit().getScreenSize();
		Dimension windowSize = win.getSize();
		int x = (screenSize.width - windowSize.width) / 2;
		int y = (screenSize.height - windowSize.height) / 2;
		win.setLocation(x, y);
	}

	public static void center(Window relativeToMe, Window centerMe) {
		int x = relativeToMe.getX()
				+ (relativeToMe.getWidth() - centerMe.getWidth()) / 2;
		int y = relativeToMe.getY()
				+ (relativeToMe.getHeight() - centerMe.getHeight()) / 2;
		centerMe.setLocation(x, y);
	}

	public static ComponentUI getUI(JComponent component) {
		try {
			Method method = component.getClass().getMethod("getUI",
					new Class[0]);
			Object o = method.invoke(component, new Object[0]);
			if (o instanceof ComponentUI)
				return (ComponentUI) o;
			else
				return null;
		} catch (Exception e) {
			return null;
		}
	}

	/**
	 * Resizes a JDialog to its preferred size.
	 * 
	 * @param dialog
	 *            the dialog box to resize
	 */
	public static void sizeToPreferred(JDialog dialog) {
		Dimension preferredSize = dialog.getContentPane().getPreferredSize();
		dialog.setSize(preferredSize);
	}

	/**
	 * Resizes a Component to its preferred size
	 * 
	 * @param comp
	 *            the component to resize
	 */
	public static void sizeToPreferred(Component comp) {
		Dimension preferredSize = comp.getPreferredSize();
		comp.setSize(preferredSize);
	}

	/**
	 * Forces the current thread to yield until the given image has completely
	 * loaded. This is useful if you need to guarantee that an image has fully
	 * loaded.
	 * 
	 * @param in
	 *            the image being loaded
	 * @deprecated Use a java.awt.MediaTracker to watch your image loading
	 */
	public static void blockUntilImagePrepared(Image in) {
		while (!Toolkit.getDefaultToolkit().prepareImage(in, -1, -1, null)) {
			Thread.currentThread().yield();
		}
	}

	/**
	 * Collapses all the child nodes of a path
	 * 
	 * @param tree
	 *            the tree that contains the paths to collapse
	 * @param path
	 *            the path to collapse
	 */
	public static void collapseTree(JTree tree, TreePath path) {
		HashSet pathSet = new HashSet();
		pathSet.add(path);
		Enumeration e = tree.getExpandedDescendants(path);
		while (e != null && e.hasMoreElements()) {
			TreePath currentPath = (TreePath) e.nextElement();
			while (currentPath != null && path.isDescendant(currentPath)
					&& currentPath.getPathCount() > 0) {
				pathSet.add(currentPath);
				currentPath = currentPath.getParentPath();
			}
		}

		Vector paths = new Vector(pathSet);
		Comparator pathSorter = new Comparator() {
			public int compare(Object a, Object b) {
				TreePath ta = (TreePath) a;
				TreePath tb = (TreePath) b;
				if (ta.getPathCount() > tb.getPathCount())
					return -1;
				else if (ta.getPathCount() < tb.getPathCount())
					return 1;
				else
					return 0;
			}
		};
		Collections.sort(paths, pathSorter);
		for (int i = 0; i < paths.size(); i++) {
			TreePath p = (TreePath) paths.elementAt(i);
			tree.collapsePath(p);
		}
	}

	/**
	 * Calls c.requestFocus(), but first walks up the component heirarchy to see
	 * if there are any JTabbedPanes between this component and the root. If
	 * there are, the tabs that contain this component are selected so that the
	 * requestFocus() call has a chance of working.
	 * 
	 * @param c
	 */
	public static void requestFocus(Component c) {
		Component parent = c;
		Component lastParent = null;
		do {
			lastParent = parent;
			parent = parent.getParent();
			if (c instanceof JTabbedPane) {
				JTabbedPane pane = (JTabbedPane) parent;
				pane.setSelectedComponent(lastParent);
			}
		} while (parent != null);
		c.requestFocus();
	}

	public static void collapseTree(JTree tree) {
		collapseTree(tree, new TreePath(tree.getModel().getRoot()));
	}

	public static void expandTree(JTree tree) {
		expandTree(tree, new TreePath(tree.getModel().getRoot()));
	}

	/**
	 * Collapses all the child nodes of a path
	 * 
	 * @param tree
	 *            the tree that contains the paths to collapse
	 * @param path
	 *            the path to collapse
	 */
	public static void expandTree(JTree tree, TreePath path) {
		expandTree(tree, path, new Hashtable());
	}

	protected static void expandTree(JTree tree, TreePath path,
			Hashtable lookedAt) {
		Object node = path.getLastPathComponent();
		if (lookedAt.containsKey(node))
			return;
		lookedAt.put(node, node);

		Vector paths = new Vector();
		tree.makeVisible(path);
		int childCount = tree.getModel().getChildCount(node);
		for (int i = 0; i < childCount; i++) {
			Object child = tree.getModel().getChild(node, i);
			TreePath p = path.pathByAddingChild(child);
			expandTree(tree, p, lookedAt);
		}
	}

	/**
	 * Determines whether a path exists in a given tree model. For this to work,
	 * the TreeModel.getIndexOfChild() method has to be correctly implemented
	 */
	public static boolean existsInModel(TreeModel model, TreePath path) {
		Object[] objects = path.getPath();
		if (!objects[0].equals(model.getRoot()))
			return false;
		Object prev = objects[0];
		for (int i = 1; i < objects.length; i++) {
			if (model.getIndexOfChild(prev, objects[i]) < 0)
				return false;
			prev = objects[i];
		}
		return true;
	}

	/**
	 * Decodes the bits of a java.awt.image.ImageObserver infoflag into a human
	 * readable string.
	 * 
	 * @param infoflag
	 *            the flag to decode
	 * @return a string describing the flag
	 */
	public static String imageObserverInfoflagToString(int infoflag) {
		String out = "";
		if ((infoflag & ImageObserver.ABORT) == ImageObserver.ABORT)
			out += "ABORT ";
		if ((infoflag & ImageObserver.ALLBITS) == ImageObserver.ALLBITS)
			out += "ALLBITS ";
		if ((infoflag & ImageObserver.ERROR) == ImageObserver.ERROR)
			out += "ERROR ";
		if ((infoflag & ImageObserver.FRAMEBITS) == ImageObserver.FRAMEBITS)
			out += "FRAMEBITS ";
		if ((infoflag & ImageObserver.HEIGHT) == ImageObserver.HEIGHT)
			out += "HEIGHT ";
		if ((infoflag & ImageObserver.PROPERTIES) == ImageObserver.PROPERTIES)
			out += "PROPERTIES ";
		if ((infoflag & ImageObserver.SOMEBITS) == ImageObserver.SOMEBITS)
			out += "SOMEBITS ";
		if ((infoflag & ImageObserver.WIDTH) == ImageObserver.WIDTH)
			out += "WIDTH ";
		return out;
	}

	/* Used by makeCompactGrid. */
	private static SpringLayout.Constraints getConstraintsForCell(int row,
			int col, Container parent, int cols) {
		SpringLayout layout = (SpringLayout) parent.getLayout();
		Component c = parent.getComponent(row * cols + col);
		return layout.getConstraints(c);
	}

	public static <T> T getDescendantOfType(Component c, Class<T> type) {
		if (type.isAssignableFrom(c.getClass()))
			return (T) c;
		if (c instanceof Container) {
			for (Component child : ((Container) c).getComponents()) {
				T out = getDescendantOfType(child, type);
				if (out != null)
					return out;
			}
		}
		return null;
	}

	public static Component getDescendantWithName(Component c, String name) {
		if (c.getName() != null && c.getName().equals(name))
			return c;
		if (c instanceof Container) {
			for (int i = 0; i < ((Container) c).getComponentCount(); i++) {
				Component out = getDescendantWithName(((Container) c)
						.getComponent(i), name);
				if (out != null)
					return out;
			}
			return null;
		} else
			return null;
	}

	/*
	 * Returns all of a container's descendants of a given class.
	 * 
	 * @param c the container to search @param findClass the class to match
	 * @return a list of components that match the class
	 */
	public static Collection getAllDescendants(Container c, Class findClass) {
		return getAllDescendants(c, findClass, false);
	}

	/*
	 * Returns all of a container's descendants of a given class.
	 * 
	 * @param c the container to search @param findClass the class to match
	 * @param deep whether to search the children of matching classes @return a
	 * list of components that match the class
	 */
	public static Collection getAllDescendants(Container c, Class findClass,
			boolean deep) {
		Collection results = new LinkedList();
		fillAllDescendants(c, findClass, deep, results);
		return results;
	}

	protected static void fillAllDescendants(Container c, Class findClass,
			boolean deep, Collection results) {
		if (findClass.isInstance(c)) {
			results.add(c);
			if (!deep)
				return;
		}
		for (int i = 0; i < c.getComponentCount(); i++) {
			Component comp = c.getComponent(i);
			if (comp instanceof Container)
				fillAllDescendants((Container) comp, findClass, deep, results);
		}
	}

	/**
	 * Aligns the first <code>rows</code> * <code>cols</code> components of
	 * <code>parent</code> in a grid. Each component in a column is as wide as
	 * the maximum preferred width of the components in that column; height is
	 * similarly determined for each row. The parent is made just big enough to
	 * fit them all. This code was unabashedly stolen from the java swing
	 * tutorial at
	 * http://java.sun.com/docs/books/tutorial/uiswing/layout/example-1dot4/SpringUtilities.java
	 * 
	 * @param rows
	 *            number of rows
	 * @param cols
	 *            number of columns
	 * @param initialX
	 *            x location to start the grid at
	 * @param initialY
	 *            y location to start the grid at
	 * @param xPad
	 *            x padding between cells
	 * @param yPad
	 *            y padding between cells
	 */
	public static void makeCompactGrid(Container parent, int rows, int cols,
			int initialX, int initialY, int xPad, int yPad) {
		SpringLayout layout;
		try {
			layout = (SpringLayout) parent.getLayout();
		} catch (ClassCastException exc) {
			System.err
					.println("The first argument to makeCompactGrid must use SpringLayout.");
			return;
		}

		// Align all cells in each column and make them the same width.
		Spring x = Spring.constant(initialX);
		for (int c = 0; c < cols; c++) {
			Spring width = Spring.constant(0);
			for (int r = 0; r < rows; r++) {
				width = Spring.max(width, getConstraintsForCell(r, c, parent,
						cols).getWidth());
			}
			for (int r = 0; r < rows; r++) {
				SpringLayout.Constraints constraints = getConstraintsForCell(r,
						c, parent, cols);
				constraints.setX(x);
				constraints.setWidth(width);
			}
			x = Spring.sum(x, Spring.sum(width, Spring.constant(xPad)));
		}

		// Align all cells in each row and make them the same height.
		Spring y = Spring.constant(initialY);
		for (int r = 0; r < rows; r++) {
			Spring height = Spring.constant(0);
			for (int c = 0; c < cols; c++) {
				height = Spring.max(height, getConstraintsForCell(r, c, parent,
						cols).getHeight());
			}
			for (int c = 0; c < cols; c++) {
				SpringLayout.Constraints constraints = getConstraintsForCell(r,
						c, parent, cols);
				constraints.setY(y);
				constraints.setHeight(height);
			}
			y = Spring.sum(y, Spring.sum(height, Spring.constant(yPad)));
		}

		// Set the parent's size.
		SpringLayout.Constraints pCons = layout.getConstraints(parent);
		pCons.setConstraint(SpringLayout.SOUTH, y);
		pCons.setConstraint(SpringLayout.EAST, x);
	}

	public static java.util.List getListData(JList jlist) {
		java.util.List out = new ArrayList();
		ListModel model = jlist.getModel();
		for (int i = 0; i < model.getSize(); i++)
			out.add(model.getElementAt(i));
		return out;
	}

	public static Cursor buildCursorByTrimming(Image image, String name,
			Cursor defaultCursor) {
		return buildCursorByTrimming(image, 0, 0, name, defaultCursor);
	}

	public static Cursor buildCursorByTrimming(Image image, int x, int y,
			String name, Cursor defaultCursor) {
		return buildCursorByTrimming(Toolkit.getDefaultToolkit(), image, x, y,
				name, defaultCursor);
	}

	public static Cursor buildCursorByTrimming(Toolkit toolkit, Image image,
			int x, int y, String name, Cursor defaultCursor) {
		Dimension d = toolkit.getBestCursorSize(image.getWidth(null), image
				.getHeight(null));
		if (d == null || d.getWidth() <= 0 || d.getHeight() <= 0)
			return defaultCursor;
		BufferedImage out = new BufferedImage((int) d.getWidth(), (int) d
				.getHeight(), BufferedImage.TYPE_INT_ARGB);
		out.getGraphics().drawImage(image, 0, 0, null);
		return toolkit.createCustomCursor(out, new Point(x, y), name);
	}
}
