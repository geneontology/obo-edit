package org.bbop.swing;

import org.bbop.expression.ExpressionException;
import org.bbop.expression.ExpressionUtil;
import org.bbop.expression.JexlContext;
import org.bbop.expression.context.HashMapContext;
import org.xml.sax.*;
import org.xml.sax.helpers.*;

import java.util.*;
import java.util.List;
import java.io.*;
import java.net.*;
import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;

public class XMLLayout {

	protected static final Map<String, Color> namedColors = new HashMap<String, Color>();
	static {
		namedColors.put("black", Color.decode("#000000"));
		namedColors.put("silver", Color.decode("#C0C0C0"));
		namedColors.put("gray", Color.decode("#808080"));
		namedColors.put("white", Color.decode("#FFFFFF"));
		namedColors.put("maroon", Color.decode("#800000"));
		namedColors.put("red", Color.decode("#FF0000"));
		namedColors.put("purple", Color.decode("#800080"));
		namedColors.put("fuchsia", Color.decode("#FF00FF"));
		namedColors.put("green", Color.decode("#008000"));
		namedColors.put("lime", Color.decode("#00FF00"));
		namedColors.put("olive", Color.decode("#808000"));
		namedColors.put("yellow", Color.decode("#FFFF00"));
		namedColors.put("navy", Color.decode("#000080"));
		namedColors.put("blue", Color.decode("#0000FF"));
		namedColors.put("teal", Color.decode("#008080"));
		namedColors.put("aqua", Color.decode("#00FFFF"));
	}

	public static abstract class LayoutItem {
		Color background = null;

		Color foreground = null;

		int width = -1;

		int height = -1;

		int maxWidth = -1;

		int maxHeight = -1;

		Font font = null;

		String name;

		int leftMargin = 0;

		int rightMargin = 0;

		int bottomMargin = 0;

		int topMargin = 0;

		String borderTitle;

		List components;

		JexlContext context;

		XMLLayoutRoot layoutRoot;

		protected int lineNumber;

		protected boolean suppressExpressionExceptions;

		public void setLineNumber(int lineNumber) {
			this.lineNumber = lineNumber;
		}

		public void setSuppressExpressionExceptions(
				boolean suppressExpressionExceptions) {
			this.suppressExpressionExceptions = suppressExpressionExceptions;
		}

		public int getLineNumber() {
			return lineNumber;
		}

		public abstract String getTagName();

		public void setBackground(Color background, Color defaultBGColor) {
			if (background == null)
				background = defaultBGColor;
			this.background = background;
		}

		public void setLayoutRoot(XMLLayoutRoot layoutRoot) {
			this.layoutRoot = layoutRoot;
		}

		public void setContext(JexlContext context) {
			this.context = context;
		}

		public JexlContext getContext() {
			return context;
		}

		public String parse(String s) throws ExpressionException {
			if (s == null)
				return null;
			if (getContext() != null) {
				try {
					return ExpressionUtil.resolveBacktickExpression(s,
							getContext());
				} catch (ExpressionException ex) {
					if (!suppressExpressionExceptions) {
						// rethrow the exception with the line number set
						ex.setLineNumber(lineNumber);
						throw ex;
					} else
						return s;
				}
			} else
				return s;
		}

		public List getComponents() throws ExpressionException {
			if (components == null) {
				components = createComponents();
				Iterator it = components.iterator();
				while (it.hasNext()) {
					Component c = (Component) it.next();
					configure(c);
					if (c instanceof XMLLayoutComponent) {
						((XMLLayoutComponent) c).guiupdate();
					}

				}
			}
			return components;
		}

		protected void configureBorders(Component c) throws ExpressionException {
			if (c instanceof JComponent) {
				JComponent jc = (JComponent) c;
				Border border = jc.getBorder();
				if (borderTitle != null) {
					TitledBorder titledBorder = new TitledBorder(
							parse(borderTitle));
					if (font != null)
						titledBorder.setTitleFont(font);
					if (border == null)
						border = titledBorder;
					else
						border = new CompoundBorder(titledBorder, border);
				}
				if (leftMargin + rightMargin + topMargin + bottomMargin > 0) {
					EmptyBorder emptyBorder = new EmptyBorder(topMargin,
							leftMargin, bottomMargin, rightMargin);
					if (border == null) {
						border = emptyBorder;
					} else {
						border = new CompoundBorder(emptyBorder, border);
					}
				}
				if (border != null)
					jc.setBorder(border);
			}
		}

		protected void configure(Component c) throws ExpressionException {
			configureBorders(c);

			if (foreground != null)
				c.setForeground(foreground);
			// don't set the background on non-containers
			if (background != null)
				c.setBackground(background);

			if (font != null)
				c.setFont(font);

			if (name != null)
				c.setName(name);

			Dimension dim = null;
			if (width != -1) {
				if (dim == null)
					dim = c.getPreferredSize();
				dim.width = width;
			}
			if (height != -1) {
				if (dim == null)
					dim = c.getPreferredSize();
				dim.height = height;
			}
			if (c instanceof JComponent) {
				if (dim != null) {
					((JComponent) c).setPreferredSize(dim);
				}
				// ((JComponent) c).setAlignmentX(JComponent.LEFT_ALIGNMENT);
				// ((JComponent) c).setAlignmentY(JComponent.TOP_ALIGNMENT);
			} else if (dim != null)
				c.setSize(dim);

			if (c instanceof JComponent) {
				dim = null;
				if (maxWidth != -1) {
					if (dim == null)
						dim = c.getMaximumSize();
					dim.width = maxWidth;
				}
				if (maxHeight != -1) {
					if (dim == null)
						dim = c.getMaximumSize();
					dim.height = maxHeight;
				}
				if (dim != null)
					((JComponent) c).setMaximumSize(dim);
			}
		}

		protected Component createComponent() throws ExpressionException {
			return null;
		}

		protected List createComponents() throws ExpressionException {
			Component c = createComponent();
			if (c == null)
				return Collections.EMPTY_LIST;
			else {
				List out = new LinkedList();
				out.add(c);
				return out;
			}
		}

		public int getMaxHeight() {
			return maxHeight;
		}

		public void setMaxHeight(int maxHeight) {
			this.maxHeight = maxHeight;
		}

		public int getMaxWidth() {
			return maxWidth;
		}

		public void setMaxWidth(int maxWidth) {
			this.maxWidth = maxWidth;
		}
	}

	public static abstract class LayoutContainerItem extends LayoutItem {
		protected java.util.List items = new LinkedList();

		public void addItem(LayoutItem item) {
			if (item != null
					&& !getSubItemType().isAssignableFrom(item.getClass())) {
				throw new RuntimeException("Cannot add a child of type "
						+ item.getClass() + " to tag <" + getTagName()
						+ ">; expected type " + getSubItemType());
			}
			items.add(item);
			if (getMaxItems() != -1 && items.size() > getMaxItems())
				throw new RuntimeException("Cannot add more than "
						+ getMaxItems() + " child items to a <" + getTagName()
						+ "> tag.");
		}

		public int getMaxItems() {
			return -1;
		}

		public Class getSubItemType() {
			return LayoutItem.class;
		}

		public void setLayoutRoot(XMLLayoutRoot layoutRoot) {
			super.setLayoutRoot(layoutRoot);
			Iterator it = items.iterator();
			while (it.hasNext()) {
				LayoutItem item = (LayoutItem) it.next();
				if (item != null)
					item.setLayoutRoot(layoutRoot);
			}
			this.layoutRoot = layoutRoot;
		}

		public LayoutItem getFirstItem() {
			if (items.size() > 0)
				return (LayoutItem) items.get(0);
			else
				return null;
		}

		public java.util.List getItems() {
			return items;
		}

		public boolean canAddDirectly() {
			return true;
		}
	}

	public static abstract class XMLSubPanel extends JPanel implements
			XMLLayoutComponent {

		protected LayoutContainerItem lcitem;

		public void setLayoutItem(LayoutContainerItem lcitem) {
			this.lcitem = lcitem;
		}

		public void guiupdate() throws ExpressionException {
			removeAll();
			Collection items = lcitem.getItems();
			Iterator it = items.iterator();
			while (it.hasNext()) {
				LayoutItem item = (LayoutItem) it.next();
				Collection comps = item.getComponents();
				Iterator it2 = comps.iterator();
				while (it2.hasNext()) {
					Component c = (Component) it2.next();
					add(c);
				}
			}
			validate();
		}
	}

	public static class WindowElement extends LayoutContainerItem {
		protected int x = -1;

		protected int y = -1;

		protected int width = 300;

		protected int height = 300;

		protected boolean startMinimized = false;

		protected boolean doPack = false;

		protected String title;

		protected XMLDialog window;

		public WindowElement(int width, int height, String title,
				boolean startMinimized) {
			this.width = width;
			this.height = height;
			this.title = title;
			this.startMinimized = startMinimized;
		}

		public String getTagName() {
			return "window";
		}

		public int getMaxItems() {
			return 1;
		}

		public void clear() {
			if (window != null)
				window.dispose();
			window = null;
		}

		public List getComponents() {
			if (window == null) {
				createWindow();
			}
			return Collections.singletonList(window);
		}

		public void createWindow() {
			window = new XMLDialog();
			if (x > -1 && y > -1)
				window.setLocation(x, y);
			window.setSize(width, height);
			if (startMinimized)
				window.doButtonMinimize();
			else {
				window.setVisible(true);
			}
		}

		protected Frame getDialogOwner() {
			if (layoutRoot == null || !(layoutRoot instanceof Container))
				return null;
			return (Frame) SwingUtilities.getAncestorOfClass(Frame.class,
					(Container) layoutRoot);
		}

		public class XMLDialog extends JDialog implements XMLLayoutComponent {

			/**
			 * 
			 */
			private static final long serialVersionUID = 2749969648347275645L;

			public XMLDialog() {
				super(getDialogOwner());
				setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
				getContentPane().setLayout(new GridLayout(1, 1));
				addWindowListener(new WindowAdapter() {
					public void windowClosing(WindowEvent e) {
						doButtonMinimize();
					}

					public void windowIconified(WindowEvent e) {
						doButtonMinimize();
					}
				});
			}

			public void doButtonMinimize() {
				if (layoutRoot != null && layoutRoot instanceof XMLLayoutPanel) {
					((XMLLayoutPanel) layoutRoot).minimizeWindow(this);
				} else
					System.err
							.println("didn't do button minimize, layoutRoot = "
									+ layoutRoot);
			}

			public void guiupdate() throws ExpressionException {
				if (title != null)
					setTitle(parse(title));
				getContentPane().removeAll();
				Iterator it = getFirstItem().getComponents().iterator();
				if (it.hasNext())
					getContentPane().add((Component) it.next());
				getContentPane().validate();
			}

			public void setVisible(boolean visible) {
				if (!isDisplayable() && doPack) {
					pack();
				}
				super.setVisible(visible);
			}
		}

		public Component createComponent() {
			return null;
		}
	}

	public static class GlueElement extends LayoutItem {
		public static final int VERTICAL = 0;

		public static final int HORIZONTAL = 1;

		public static final int BOTH = 2;

		protected int orientation;

		protected class Glue extends Box.Filler implements XMLLayoutComponent {

			/**
			 * 
			 */
			private static final long serialVersionUID = -2857876679688442918L;

			public Glue() {
				super(new Dimension(0, 0), new Dimension(0, 0), new Dimension(
						0, 0));
			}

			public void guiupdate() {
				if (orientation == VERTICAL) {
					changeShape(new Dimension(0, 0), new Dimension(0, 0),
							new Dimension(0, Short.MAX_VALUE));
				} else if (orientation == HORIZONTAL) {
					changeShape(new Dimension(0, 0), new Dimension(0, 0),
							new Dimension(Short.MAX_VALUE, 0));
				} else {
					changeShape(new Dimension(0, 0), new Dimension(0, 0),
							new Dimension(Short.MAX_VALUE, Short.MAX_VALUE));
				}
			}
		}

		public String getTagName() {
			return "glue";
		}

		public GlueElement(int orientation) {
			this.orientation = orientation;
		}

		public int getOrientation() {
			return orientation;
		}

		public Component createComponent() {
			return new Glue();
		}
	}

	public static class SpacerElement extends LayoutItem {
		public static final int VERTICAL = 0;

		public static final int HORIZONTAL = 1;

		public static final int BOTH = 2;

		public static final int GLUE = 3;

		protected int orientation;

		protected int size;

		protected class Spacer extends Box.Filler implements XMLLayoutComponent {

			/**
			 * 
			 */
			private static final long serialVersionUID = 765797498221981065L;

			public Spacer() {
				super(new Dimension(0, 0), new Dimension(0, 0), new Dimension(
						0, 0));
			}

			public void guiupdate() {
				if (orientation == VERTICAL) {
					changeShape(new Dimension(0, size), new Dimension(0, size),
							new Dimension(0, size));
				} else if (orientation == HORIZONTAL) {
					changeShape(new Dimension(size, 0), new Dimension(size, 0),
							new Dimension(size, 0));
				} else if (orientation == BOTH) {
					changeShape(new Dimension(size, size), new Dimension(size,
							size), new Dimension(size, size));
				} else if (orientation == GLUE) {
					changeShape(new Dimension(0, 0), new Dimension(0, 0),
							new Dimension(Short.MAX_VALUE, Short.MAX_VALUE));
				}
			}
		}

		public SpacerElement(int orientation, int size) {
			this.orientation = orientation;
			setSize(size);
		}

		public String getTagName() {
			return "spacer";
		}

		public int getSize() {
			return size;
		}

		public void setSize(int size) {
			this.size = size;
		}

		public int getOrientation() {
			return orientation;
		}

		public Component createComponent() {
			return new Spacer();
		}
	}

	public static class GridElement extends LayoutContainerItem {
		protected int rows = 1;

		protected int cols = 1;

		protected class Grid extends XMLSubPanel {
			/**
			 * 
			 */
			private static final long serialVersionUID = -670246536920157248L;

			public Grid() {
				setLayoutItem(GridElement.this);
			}

			public void guiupdate() throws ExpressionException {
				setLayout(new GridLayout(rows, cols));
				super.guiupdate();
			}
		}

		public GridElement(int rows, int cols) {
			this.rows = rows;
			this.cols = cols;
		}

		public String getTagName() {
			return "grid";
		}

		public Component createComponent() {
			return new Grid();
		}
	}

	public static class CompactGridElement extends LayoutContainerItem {
		protected int rows = -1;

		protected int cols = -1;

		protected int initialX = 0;

		protected int initialY = 0;

		protected int xpad = 0;

		protected int ypad = 0;

		protected class CompactGrid extends XMLSubPanel {

			/**
			 * 
			 */
			private static final long serialVersionUID = 5842644183397051150L;

			public CompactGrid() {
				super();
				setLayoutItem(CompactGridElement.this);
				setLayout(new SpringLayout());
			}

			public void guiupdate() throws ExpressionException {
				super.guiupdate();
				SpringUtilities.makeCompactGrid(this, rows, cols, initialX,
						initialY, xpad, ypad);
				validate();
			}
		}

		public CompactGridElement(int rows, int cols, int initialX,
				int initialY, int xpad, int ypad) {
			this.rows = rows;
			this.cols = cols;
			this.initialX = initialX;
			this.initialY = initialY;
			this.xpad = xpad;
			this.ypad = ypad;
		}

		public String getTagName() {
			return "compactgrid";
		}

		public Component createComponent() {
			return new CompactGrid();
		}
	}

	public static class BoxElement extends LayoutContainerItem {
		public static final int VERTICAL = 0;

		public static final int HORIZONTAL = 1;

		protected int orientation;

		protected class BoxPanel extends XMLSubPanel {

			/**
			 * 
			 */
			private static final long serialVersionUID = 1489573969867316524L;

			public BoxPanel() {
				super();
				setLayoutItem(BoxElement.this);
			}

			public void guiupdate() throws ExpressionException {
				setLayout(new BoxLayout(this,
						(orientation == VERTICAL ? BoxLayout.Y_AXIS
								: BoxLayout.X_AXIS)));
				super.guiupdate();
			}
		}

		public BoxElement(int orientation) {
			this.orientation = orientation;
		}

		public String getTagName() {
			return "box";
		}

		protected Component createComponent() {
			return new BoxPanel();
		}
	}

	public static class IfElement extends LayoutContainerItem {

		protected String expression;

		protected Collection elseItems = new LinkedList();

		public IfElement(String expression) {
			this.expression = expression;
		}

		public String getExpression() {
			return expression;
		}

		protected boolean shouldShow() {
			try {
				return ExpressionUtil.execBoolean(expression, getContext());
			} catch (Exception ex) {
				System.err.println("trying to resolve expression '"
						+ expression + "' with context: " + getContext());
				System.err.println(getContext().getVariableValue("TextEditor"));
				ex.printStackTrace();
				return false;
			}
		}

		public List getComponents() throws ExpressionException {
			List out = new LinkedList();
			if (shouldShow()) {
				Iterator it = getItems().iterator();
				while (it.hasNext()) {
					LayoutItem item = (LayoutItem) it.next();
					if (!(item instanceof ElseElement))
						out.addAll(item.getComponents());
				}
			} else {
				Iterator it = getItems().iterator();
				while (it.hasNext()) {
					LayoutItem item = (LayoutItem) it.next();
					if (item instanceof ElseElement) {
						ElseElement elt = (ElseElement) item;
						Iterator it2 = elt.getItems().iterator();
						while (it2.hasNext()) {
							LayoutItem item2 = (LayoutItem) it2.next();
							out.addAll(item2.getComponents());
						}
						// out.addAll(item.getComponents());
					}
				}
			}
			return out;
		}

		public String getTagName() {
			return "if";
		}

		/**
		 * This will never get called
		 */
		protected Component createComponent() {
			return null;
		}
	}

	public static class ElseElement extends LayoutContainerItem {

		public ElseElement() {
		}

		public String getTagName() {
			return "else";
		}

		public int getMaxItems() {
			return 1;
		}

		/**
		 * This will never get called
		 */
		protected Component createComponent() {
			return null;
		}
	}

	public static class LabelElement extends LayoutItem {
		protected String label;

		protected String iconURL;

		protected Icon icon;

		protected int halign;

		protected int valign;

		protected class XMLLabel extends JLabel implements XMLLayoutComponent {

			/**
			 * 
			 */
			private static final long serialVersionUID = -1800321155756286936L;

			public XMLLabel() {
				super();
				setHorizontalAlignment(halign);
				setVerticalAlignment(valign);
			}

			public void guiupdate() throws ExpressionException {
				String parsedLabel = parse(label);
				String parsedURL = parse(iconURL);

				if (iconURL != null) {
					try {
						URL url = new URL(parsedURL);
						icon = new ImageIcon(url);
					} catch (Exception ex) {
					}
				}
				setText(parsedLabel);
				setIcon(icon);
			}
		}

		public LabelElement(String label, String iconURL, int halign, int valign) {
			this.label = label;
			this.iconURL = iconURL;
			this.halign = halign;
			this.valign = valign;
		}

		public String getTagName() {
			return "label";
		}

		protected Component createComponent() {
			return new XMLLabel();
		}
	}

	public static class TabElement extends LayoutContainerItem {

		protected class Tabs extends JTabbedPane implements XMLLayoutComponent {

			/**
			 * 
			 */
			private static final long serialVersionUID = 2956611404579656343L;

			protected int id;

			public Tabs() {
			}

			public void guiupdate() throws ExpressionException {
				Component selectedComp = getSelectedComponent();

				removeAll();
				Iterator it = getItems().iterator();
				for (int i = 0; it.hasNext(); i++) {
					Object o = it.next();
					TabRecord record = (TabRecord) o;

					if (record == null || record.getComponents().size() < 1)
						continue;
					if (record.getSelectedStr() != null) {
						boolean selected = parse(record.getSelectedStr())
								.equals("true");
						if (selected && selectedComp == null)
							selectedComp = (Component) record.getComponents()
									.get(0);

					}
					String str = parse(record.getName());
					add((Component) record.getComponents().get(0), str);
					if (record.getColor() != null)
						setBackgroundAt(i, record.getColor());
					else if (defaultTabColor != null)
						setBackgroundAt(i, defaultTabColor);
				}
				if (selectedComp != null)
					setSelectedComponent(selectedComp);
			}
		}

		protected Color defaultTabColor;

		public TabElement() {
		}

		public void setDefaultTabColor(Color defaultTabColor) {
			this.defaultTabColor = defaultTabColor;
		}

		public boolean canAddDirectly() {
			return false;
		}

		public String getTagName() {
			return "tabs";
		}

		public Class getSubItemType() {
			return TabRecord.class;
		}

		protected Component createComponent() {
			return new Tabs();
		}
	}

	public static class TabRecord extends LayoutContainerItem {
		String name;

		LayoutItem item;

		Color bgColor;

		String selectedStr;

		// boolean selected = false;

		public String getName() {
			return name;
		}

		public String getTagName() {
			return "tab";
		}

		public String getSelectedStr() {
			return selectedStr;
		}

		public void setSelectedStr(String selectedStr) {
			this.selectedStr = selectedStr;
		}

		/*
		 * public boolean isSelected() { return selected; }
		 * 
		 * public void setSelected(boolean selected) { this.selected = selected; }
		 */
		public void setName(String name) {
			this.name = name;
		}

		public Color getColor() {
			return bgColor;
		}

		public void setColor(Color bgColor) {
			this.bgColor = bgColor;
		}

		public List getComponents() throws ExpressionException {
			return getFirstItem().getComponents();
		}
	}

	public static class ScrollerElement extends LayoutContainerItem {
		public static final int ALWAYS = 0;

		public static final int AS_NEEDED = 1;

		public static final int NEVER = 2;

		protected int horiz = AS_NEEDED;

		protected int vert = NEVER;

		public ScrollerElement(int horiz, int vert) {
			this.horiz = horiz;
			this.vert = vert;
		}

		public String getTagName() {
			return "scroller";
		}

		public int getMaxItems() {
			return 1;
		}

		protected Component createComponent() throws ExpressionException {
			int h = JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED;
			int v = JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED;

			if (horiz == XMLLayout.ScrollerElement.ALWAYS)
				h = JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS;
			else if (horiz == XMLLayout.ScrollerElement.NEVER)
				h = JScrollPane.HORIZONTAL_SCROLLBAR_NEVER;

			if (vert == XMLLayout.ScrollerElement.ALWAYS)
				v = JScrollPane.VERTICAL_SCROLLBAR_ALWAYS;
			else if (vert == XMLLayout.ScrollerElement.NEVER)
				v = JScrollPane.VERTICAL_SCROLLBAR_NEVER;
			JScrollPane out = new JScrollPane(v, h);
			if (getFirstItem().getComponents().size() > 0) {
				out.setViewportView((Component) getFirstItem().getComponents()
						.get(0));
			}
			return out;
		}
	}

	public static class DividerElement extends LayoutContainerItem {

		public static final int VERTICAL = 0;

		public static final int HORIZONTAL = 1;

		protected int dividerLoc = -1;

		protected int width = 3;

		protected int orientation;

		protected int dividerPercentage = -1;

		public DividerElement(int width, int orientation, int dividerLoc,
				int dividerPercentage) {
			this.dividerLoc = dividerLoc;
			this.width = width;
			this.orientation = orientation;
			this.dividerPercentage = dividerPercentage;
		}

		protected class Divider extends JSplitPane implements
				XMLLayoutComponent {

			/**
			 * 
			 */
			private static final long serialVersionUID = 9066556972754310524L;

			public Divider(int orientation) {
				super(orientation == VERTICAL ? JSplitPane.VERTICAL_SPLIT
						: JSplitPane.HORIZONTAL_SPLIT);

				if (width != -1)
					setDividerSize(width);

				if (dividerLoc != -1)
					setDividerLocation(dividerLoc);

				if (dividerPercentage != -1)
					setDividerLocation(((double) dividerPercentage)
							/ ((double) 100));
			}

			public void guiupdate() throws ExpressionException {
				int dividerLoc = getDividerLocation();
				if (getFirstItem() != null
						&& getSecond().getComponents().size() > 0) {
					setLeftComponent((Component) getFirstItem().getComponents()
							.get(0));
				}
				if (getSecond() != null
						&& getSecond().getComponents().size() > 0) {
					setRightComponent((Component) getSecond().getComponents()
							.get(0));
				}
				setDividerLocation(dividerLoc);
				validate();
			}
		}

		public Component createComponent() {
			return new Divider(orientation);
		}

		public boolean canAddDirectly() {
			return false;
		}

		public void setFirst(LayoutItem item) {
			if (getFirstItem() != null)
				throw new RuntimeException(
						"Only one item is allowed in the <first> tag.");
			if (items.size() == 0)
				items.add(item);
			else
				items.set(0, item);
		}

		public void setSecond(LayoutItem item) {
			if (getSecond() != null)
				throw new RuntimeException(
						"Only one item is allowed in the <second> tag.");
			if (items.size() == 1)
				items.add(item);
			else
				items.set(1, item);
		}

		public String getTagName() {
			return "divider";
		}

		public int getMaxItems() {
			return 2;
		}

		public LayoutItem getSecond() {
			if (items.size() < 2)
				return null;
			else
				return (LayoutItem) items.get(1);
		}

		public int getOrientation() {
			return orientation;
		}

		public int getDividerLoc() {
			return dividerLoc;
		}

		public int getWidth() {
			return width;
		}
	}

	public static class ComponentElement extends LayoutItem {
		protected String id;

		protected Properties props;

		protected ComponentNameResolver resolver;

		protected StringBuffer xml = null;

		public ComponentElement(String id, ComponentNameResolver resolver,
				Properties props) {
			this.id = id;
			this.props = props;
			this.resolver = resolver;
		}

		public String getTagName() {
			return "component";
		}

		public void setBackground(Color background, Color defaultBGColor) {
			if (background != null)
				this.background = background;
		}

		public void appendXMLData(String data) {
			if (xml == null)
				xml = new StringBuffer();
			xml.append(data);
		}

		public String getXML() {
			if (xml == null)
				return null;
			else
				return xml.toString();
		}

		public String getID() {
			return id;
		}

		public Properties getProperties() {
			return props;
		}

		/*
		 * public Component getComponent() { if (component == null) { component =
		 * createComponent(); configureBorders(component); if (component
		 * instanceof XMLLayoutComponent) { ((XMLLayoutComponent) component).
		 * guiupdate(); } } return component; }
		 */
		protected Component createComponent() {
			Component out = resolver.resolveName(id, props, getXML());
			return out;
		}

		public String toString() {
			return "component id='" + id + "'";
		}
	}

	public static class PanelElement extends LayoutContainerItem {
		/*
		 * protected LayoutItem north; protected LayoutItem south; protected
		 * LayoutItem east; protected LayoutItem west; protected LayoutItem
		 * center;
		 */
		protected class BorderPanel extends JPanel implements
				XMLLayoutComponent {

			/**
			 * 
			 */
			private static final long serialVersionUID = -3253170914570900063L;

			public BorderPanel() {
				super();
				setLayout(new BorderLayout());
			}

			public void guiupdate() throws ExpressionException {
				removeAll();

				if (getNorth() != null && getNorth().getComponents().size() > 0) {
					add((Component) getNorth().getComponents().get(0), "North");
				}
				if (getSouth() != null && getSouth().getComponents().size() > 0) {
					add((Component) getSouth().getComponents().get(0), "South");
				}
				if (getEast() != null && getEast().getComponents().size() > 0) {
					add((Component) getEast().getComponents().get(0), "East");
				}
				if (getWest() != null && getWest().getComponents().size() > 0) {
					add((Component) getWest().getComponents().get(0), "West");
				}
				if (getCenter() != null
						&& getCenter().getComponents().size() > 0) {
					add((Component) getCenter().getComponents().get(0),
							"Center");
				}
			}
		}

		public PanelElement() {
			for (int i = 0; i < 5; i++)
				addItem(null);
		}

		public int getMaxItems() {
			return 5;
		}

		public String getTagName() {
			return "panel";
		}

		public void setEast(LayoutItem item) {
			if (getEast() != null) {
				throw new RuntimeException(
						"Only one item is allowed in the <east> tag.");
			}
			items.set(0, item);
		}

		public void setWest(LayoutItem item) {
			if (getWest() != null) {
				throw new RuntimeException(
						"Only one item is allowed in the <west> tag.");
			}
			items.set(1, item);
		}

		public void setCenter(LayoutItem item) {
			if (getCenter() != null) {
				throw new RuntimeException(
						"Only one item is allowed in the <center> tag.");
			}
			items.set(2, item);
		}

		public void setNorth(LayoutItem item) {
			if (getNorth() != null) {
				throw new RuntimeException(
						"Only one item is allowed in the <north> tag.");
			}
			items.set(3, item);
		}

		public void setSouth(LayoutItem item) {
			if (getSouth() != null) {
				throw new RuntimeException(
						"Only one item is allowed in the <south> tag.");
			}
			items.set(4, item);
		}

		public LayoutItem getEast() {
			return (LayoutItem) items.get(0);
		}

		public LayoutItem getWest() {
			return (LayoutItem) items.get(1);
		}

		public LayoutItem getCenter() {
			return (LayoutItem) items.get(2);
		}

		public LayoutItem getNorth() {
			return (LayoutItem) items.get(3);
		}

		public LayoutItem getSouth() {
			return (LayoutItem) items.get(4);
		}

		public boolean canAddDirectly() {
			return false;
		}

		protected Component createComponent() {
			return new BorderPanel();
		}
	}

	protected boolean cached = false;

	protected LayoutItem root;

	protected Collection windows = new LinkedList();

	protected ComponentNameResolver resolver;

	protected JexlContext context;

	protected Color defaultTabColor;

	protected Color defaultBGColor;

	protected Font defaultFont;

	protected boolean suppressExpressionExceptions = false;

	protected String xml = "";

	public void setSuppressExpressionExceptions(
			boolean suppressExpressionExceptions) {
		this.suppressExpressionExceptions = suppressExpressionExceptions;
	}

	public Color getDefaultTabColor() {
		return defaultTabColor;
	}

	public Font getDefaultFont() {
		return defaultFont;
	}

	public XMLLayout() {
	}

	public void setLayout(String xml) {
		if (!xml.trim().startsWith("<xmllayout>")) {
			xml = "<xmllayout>" + xml + "</xmllayout>";
		}
		/*
		 * try { InputSource doc = getDocument(xml); Reader reader =
		 * doc.getCharacterStream(); System.err.println("<<<---------Creating
		 * layout from XML--------->>>"); int r; while((r = reader.read()) !=
		 * -1) { System.err.print((char) r); } reader.close();
		 * System.err.println("<<<---------Finishing layout from
		 * XML--------->>>"); } catch (IOException ex) { ex.printStackTrace(); }
		 */
		this.xml = xml;
	}

	public Collection getWindows() throws SAXException, IOException {
		if (!cached) {
			cacheLayout();
			// cache the root
			cached = true;
		}
		return windows;
	}

	public LayoutItem getRoot() throws SAXException, IOException {
		if (!cached) {
			cacheLayout();
			// cache the root
			cached = true;
		}
		return root;
	}

	public void validateLayout() throws SAXException, ExpressionException,
			IOException {
		createLayoutBuilder(false);
		cacheLayout();
		Iterator it = root.getComponents().iterator();
		while (it.hasNext()) {
			Object o = it.next();
			if (o instanceof XMLLayoutComponent)
				((XMLLayoutComponent) o).guiupdate();
		}
	}

	public LayoutBuilder createLayoutBuilder(
			boolean suppressExpressionExceptions) throws SAXException,
			IOException {
		XMLReader parser = null;
		try {
			parser = XMLReaderFactory.createXMLReader();
		} catch (SAXException ex) {
			parser = XMLReaderFactory
					.createXMLReader("org.apache.crimson.parser.XMLReaderImpl");
		}

		LayoutBuilder layoutBuilder = new LayoutBuilder();
		layoutBuilder
				.setSuppressExpressionExceptions(suppressExpressionExceptions);
		layoutBuilder.setComponentNameResolver(resolver);
		layoutBuilder.setContext(context);
		layoutBuilder.setDefaultTabColor(defaultTabColor);
		layoutBuilder.setDefaultBGColor(defaultBGColor);
		layoutBuilder.setDefaultFont(defaultFont);
		parser.setContentHandler(layoutBuilder);
		parser.parse(getDocument(xml));
		return layoutBuilder;
	}

	protected void cacheLayout() throws SAXException, IOException {
		LayoutBuilder layoutBuilder = createLayoutBuilder(suppressExpressionExceptions);
		root = layoutBuilder.getRoot();
		root.setLayoutRoot(layoutRoot);
		windows = layoutBuilder.getWindows();
		Iterator it = windows.iterator();
		while (it.hasNext()) {
			LayoutItem item = (LayoutItem) it.next();
			item.setLayoutRoot(layoutRoot);
		}

	}

	public void setDefaultTabColor(Color defaultTabColor) {
		this.defaultTabColor = defaultTabColor;
		cached = false;
	}

	public void setDefaultBGColor(Color defaultBGColor) {
		this.defaultBGColor = defaultBGColor;
		cached = false;
	}

	public Color getDefaultBGColor() {
		return defaultBGColor;
	}

	public void setDefaultFont(Font defaultFont) {
		this.defaultFont = defaultFont;
		cached = false;
	}

	public void setContext(JexlContext context) {
		if (context == null)
			context = new HashMapContext();

		this.context = context;
		cached = false;
	}
	
	public JexlContext getContext() {
		return context;
	}

	public void setComponentNameResolver(ComponentNameResolver resolver) {
		this.resolver = resolver;
		cached = false;
	}
	
	public ComponentNameResolver getComponentNameResolver() {
		return resolver;
	}

	protected static Color decodeColor(String colorStr) {
		Color color = (Color) namedColors.get(colorStr);
		if (color == null) {
			StringTokenizer tokenizer = new StringTokenizer(colorStr, ",");
			int[] rgb = new int[3];
			boolean failed = false;
			int i;
			for (i = 0; tokenizer.hasMoreElements() && i < rgb.length; i++) {
				String token = tokenizer.nextToken().trim();
				try {
					rgb[i] = Integer.parseInt(token);
				} catch (NumberFormatException ex) {
					failed = true;
					break;
				}
			}
			if (!failed && i == rgb.length && !tokenizer.hasMoreElements()) {
				color = new Color(rgb[0], rgb[1], rgb[2]);
			} else {
				try {
					color = Color.decode(colorStr);
				} catch (Exception e) {
				}
			}
		}
		return color;
	}

	public static InputSource getDocument(String layout) {
		return new InputSource(new StringReader(layout));
	}

	public static InputSource getDocument(URL url) throws IOException {
		return getDocument(url.openStream());
	}

	public static InputSource getDocument(File file) throws IOException {
		return getDocument(new BufferedInputStream(new FileInputStream(file)));
	}

	public static InputSource getDocument(InputStream stream) {
		return new InputSource(stream);
	}

	public XMLLayoutRoot getLayoutRoot(XMLLayout layout) {
		resolver.startParseNotify();
		XMLLayoutRoot c = getXMLLayoutRoot(layout);
		resolver.endParseNotify();
		return c;
	}

	protected XMLLayoutRoot layoutRoot = null;

	protected void setLayoutRoot(XMLLayoutRoot layoutRoot) {
		this.layoutRoot = layoutRoot;
		layoutRoot.setIsLayoutRoot(true);
		if (root != null) {
			root.setLayoutRoot(layoutRoot);
		}
	}

	public static XMLLayoutRoot getXMLLayoutRoot(XMLLayout layout) {
		XMLLayoutPanel out = new XMLLayoutPanel();
		layout.setLayoutRoot(out);
		out.setXMLLayout(layout);
		return out;
	}

	protected static class LayoutBuilder extends DefaultHandler {
		private static String[] componentTags = { "scroller", "panel", "label",
				"spacer", "glue", "component", "grid", "compactgrid", "box",
				"divider", "tabs", "window" };

		private Vector stack;

		private LayoutItem root;

		protected ComponentNameResolver resolver;

		protected JexlContext context;

		protected Color defaultTabColor;

		protected Color defaultBGColor;

		protected Font defaultFont;

		protected int componentDepth = 0;

		protected Collection windows = new LinkedList();

		protected Locator locator;

		protected boolean suppressExpressionExceptions;

		public void setSuppressExpressionExceptions(
				boolean suppressExpressionExceptions) {
			this.suppressExpressionExceptions = suppressExpressionExceptions;
		}

		public JexlContext getContext() {
			return context;
		}

		public void setContext(JexlContext context) {
			this.context = context;
		}

		public void setDocumentLocator(Locator locator) {
			this.locator = locator;
		}

		public void setDefaultTabColor(Color defaultTabColor) {
			this.defaultTabColor = defaultTabColor;
		}

		public void setDefaultBGColor(Color defaultBGColor) {
			this.defaultBGColor = defaultBGColor;
		}

		public void setDefaultFont(Font defaultFont) {
			this.defaultFont = defaultFont;
		}

		public void setComponentNameResolver(ComponentNameResolver resolver) {
			this.resolver = resolver;
		}

		public boolean isComponentTag(String tag) {
			for (int i = 0; i < componentTags.length; i++)
				if (componentTags[i].equalsIgnoreCase(tag))
					return true;
			return false;
		}

		protected String getValue(Attributes atts, String value) {
			return atts.getValue(value);
		}

		protected String getValue(Attributes atts, int val) {
			return atts.getValue(val);
		}

		public LayoutItem getRoot() {
			return root;
		}

		public Collection getWindows() {
			return windows;
		}

		// Parser calls this once at the beginning of a document
		public void startDocument() throws SAXException {
			stack = new Vector();
			windows.clear();
		}

		// Parser calls this for each element in a document
		public void startElement(String namespaceURI, String localName,
				String rawName, Attributes atts) throws SAXException {
			if (componentDepth > 0) {
				if (localName.equalsIgnoreCase("component")) {
					componentDepth++;
				}
				ComponentElement item = (ComponentElement) stack.elementAt(0);
				StringBuffer attString = new StringBuffer();
				for (int i = 0; i < atts.getLength(); i++) {
					String name = atts.getQName(i);
					String value = getValue(atts, i);
					attString.append(" " + name + "=\""
							+ value.replace('"', '\'') + "\"");
				}
				item.appendXMLData("<" + localName + attString.toString()
						+ ">\n");

				// do a straight copy of all tags into the xml attribute of
				// the current component item
				return;
			}

			LayoutItem item = null;
			localName = rawName;
			if (localName.equalsIgnoreCase("xmllayout")) {
			} else if (localName.equalsIgnoreCase("component")) {
				Properties props = new Properties();
				for (int i = 0; i < atts.getLength(); i++) {
					String name = atts.getQName(i);
					String value = getValue(atts, i);
					if (!name.equals("id"))
						props.put(name, value);
				}
				String id = getValue(atts, "id");
				item = new ComponentElement(id, resolver, props);
				stack.insertElementAt(item, 0);
				componentDepth++;
			} else if (localName.equalsIgnoreCase("compactgrid")) {
				int rows = -1;
				int cols = -1;
				int initialX = 0;
				int initialY = 0;
				int xpad = 0;
				int ypad = 0;

				String rowStr = getValue(atts, "rows");
				if (rowStr != null) {
					try {
						rows = Integer.parseInt(rowStr);
					} catch (NumberFormatException ex) {
					}
				}
				String colStr = getValue(atts, "cols");
				if (colStr != null) {
					try {
						cols = Integer.parseInt(colStr);
					} catch (NumberFormatException ex) {
					}
				}
				String initialXStr = getValue(atts, "xinit");
				if (initialXStr != null) {
					try {
						initialX = Integer.parseInt(initialXStr);
					} catch (NumberFormatException ex) {
					}
				}
				String initialYStr = getValue(atts, "yinit");
				if (initialYStr != null) {
					try {
						initialY = Integer.parseInt(initialYStr);
					} catch (NumberFormatException ex) {
					}
				}
				String xpadStr = getValue(atts, "xpad");
				if (xpadStr != null) {
					try {
						xpad = Integer.parseInt(xpadStr);
					} catch (NumberFormatException ex) {
					}
				}
				String ypadStr = getValue(atts, "ypad");
				if (ypadStr != null) {
					try {
						ypad = Integer.parseInt(ypadStr);
					} catch (NumberFormatException ex) {
					}
				}
				item = new CompactGridElement(rows, cols, initialX, initialY,
						xpad, ypad);
				stack.insertElementAt(item, 0);
			} else if (localName.equalsIgnoreCase("grid")) {
				int rows = 1;
				int cols = 1;

				String rowStr = getValue(atts, "rows");
				if (rowStr != null) {
					try {
						rows = Integer.parseInt(rowStr);
					} catch (NumberFormatException ex) {
					}
				}
				String colStr = getValue(atts, "cols");
				if (colStr != null) {
					try {
						cols = Integer.parseInt(colStr);
					} catch (NumberFormatException ex) {
					}
				}

				item = new GridElement(rows, cols);
				stack.insertElementAt(item, 0);
			} else if (localName.equalsIgnoreCase("divider")) {
				String widthStr = getValue(atts, "dividerSize");
				int width = 5;
				if (widthStr != null) {
					try {
						width = Integer.parseInt(widthStr);
					} catch (Exception e) {
					}
				}
				int orientation = DividerElement.VERTICAL;
				if (getValue(atts, "orientation") != null
						&& getValue(atts, "orientation").equalsIgnoreCase(
								"horz"))
					orientation = DividerElement.HORIZONTAL;

				int dividerLoc = -1;
				int dividerPercentage = -1;
				String dividerLocStr = getValue(atts, "dividerLoc");
				if (dividerLocStr != null) {
					if (dividerLocStr.endsWith("%")) {
						dividerLocStr = dividerLocStr.substring(0,
								dividerLocStr.length() - 1);
						dividerPercentage = Integer.parseInt(dividerLocStr);
						dividerLoc = -1;
					} else {
						try {
							dividerLoc = Integer.parseInt(dividerLocStr);
							dividerPercentage = -1;
						} catch (Exception ex) {
						}
					}
				}

				item = new DividerElement(width, orientation, dividerLoc,
						dividerPercentage);
				stack.insertElementAt(item, 0);
			} else if (localName.equalsIgnoreCase("panel")) {
				item = new PanelElement();
				stack.insertElementAt(item, 0);
			} else if (localName.equals("label")) {
				String halignmentStr = getValue(atts, "halign");
				String valignmentStr = getValue(atts, "valign");

				int halign = SwingConstants.LEFT;
				int valign = SwingConstants.CENTER;

				if (halignmentStr != null) {
					if (halignmentStr.equalsIgnoreCase("CENTER"))
						halign = SwingConstants.CENTER;
					else if (halignmentStr.equalsIgnoreCase("RIGHT"))
						halign = SwingConstants.RIGHT;
					else
						halign = SwingConstants.LEFT;
				}

				if (valignmentStr != null) {
					if (valignmentStr.equalsIgnoreCase("TOP"))
						valign = SwingConstants.TOP;
					else if (valignmentStr.equalsIgnoreCase("BOTTOM"))
						valign = SwingConstants.BOTTOM;
					else
						valign = SwingConstants.CENTER;
				}

				item = new LabelElement(getValue(atts, "text"), getValue(atts,
						"icon"), halign, valign);
				stack.insertElementAt(item, 0);
			} else if (localName.equals("spacer")) {
				int orientation = SpacerElement.BOTH;
				if (getValue(atts, "orientation") != null) {
					if (getValue(atts, "orientation").equalsIgnoreCase("horz"))
						orientation = SpacerElement.HORIZONTAL;
					else if (getValue(atts, "orientation").equalsIgnoreCase(
							"vert"))
						orientation = SpacerElement.VERTICAL;
				}
				int size = 10;
				String sizeStr = getValue(atts, "size");
				try {
					size = Integer.parseInt(sizeStr);
				} catch (Exception ex) {
				}
				item = new SpacerElement(orientation, size);
				stack.insertElementAt(item, 0);
			} else if (localName.equals("glue")) {
				int orientation = GlueElement.BOTH;
				if (getValue(atts, "orientation") != null) {
					if (getValue(atts, "orientation").equalsIgnoreCase("horz"))
						orientation = SpacerElement.HORIZONTAL;
					else if (getValue(atts, "orientation").equalsIgnoreCase(
							"vert"))
						orientation = SpacerElement.VERTICAL;
				}
				item = new GlueElement(orientation);
				stack.insertElementAt(item, 0);
			} else if (localName.equalsIgnoreCase("tabs")) {
				item = new TabElement();
				((TabElement) item).setDefaultTabColor(defaultTabColor);
				stack.insertElementAt(item, 0);
			} else if (localName.equalsIgnoreCase("if")) {
				// note: the following is not a mistake. The eval attribute
				// value should be fetched directly without backtick
				// substitution, because the eval value is always an
				// expression
				item = new IfElement(atts.getValue("eval"));
				stack.insertElementAt(item, 0);
			} else if (localName.equalsIgnoreCase("else")) {
				// note: the following is not a mistake. The eval attribute
				// value should be fetched directly without backtick
				// substitution, because the eval value is always an
				// expression
				item = new ElseElement();
				stack.insertElementAt(item, 0);
			} else if (localName.equalsIgnoreCase("tab")) {
				TabElement pitem = (TabElement) stack.elementAt(0);
				TabRecord record = new TabRecord();
				String colorStr = getValue(atts, "color");
				Color color = null;
				if (colorStr != null) {
					color = Color.getColor(colorStr);
					if (color == null) {
						try {
							color = Color.decode(colorStr);
						} catch (Exception e) {
						}
					}
				}
				record.setSelectedStr(getValue(atts, "selected"));
				record.setColor(color);
				record.setName(getValue(atts, "name"));
				pitem.addItem(record);
				// pitem.addName(getValue(atts, "name"));
			} else if (localName.equalsIgnoreCase("box")) {
				int orientation = 0;
				if (getValue(atts, "orientation") != null
						&& getValue(atts, "orientation").equalsIgnoreCase(
								"horz"))
					orientation = 1;
				item = new BoxElement(orientation);
				stack.insertElementAt(item, 0);
			} else if (localName.equalsIgnoreCase("scroller")) {
				item = new ScrollerElement(convertScrollString(getValue(atts,
						"horz")), convertScrollString(getValue(atts, "vert")));
				stack.insertElementAt(item, 0);
			} else if (localName.equalsIgnoreCase("window")) {
				int width = 300;
				int height = 300;
				boolean startMinimized = false;

				String title = getValue(atts, "title");

				String widthStr = getValue(atts, "width");
				String heightStr = getValue(atts, "height");
				String startMinimizedStr = getValue(atts, "startminimized");

				if (widthStr != null) {
					try {
						width = Integer.parseInt(widthStr);
					} catch (NumberFormatException ex) {
					}
				}
				if (heightStr != null) {
					try {
						height = Integer.parseInt(heightStr);
					} catch (NumberFormatException ex) {
					}
				}
				if (startMinimizedStr != null) {
					startMinimized = startMinimizedStr.equals("true");
				}

				item = new WindowElement(width, height, title, startMinimized);
				stack.insertElementAt(item, 0);
			}

			if (item != null) {
				item.setContext(context);
				item.setLineNumber(locator.getLineNumber());
				item
						.setSuppressExpressionExceptions(suppressExpressionExceptions);
			}

			if (isComponentTag(localName)) {
				configureComponent(item, atts);
			}

			if (root == null && item != null
					&& !(item instanceof WindowElement) && stack.size() == 1) {
				root = item;
			}
		}

		private void configureComponent(LayoutItem item, Attributes atts) {
			String fontName = getValue(atts, "font");
			String foregroundStr = getValue(atts, "foreground");
			String backgroundStr = getValue(atts, "background");
			String name = getValue(atts, "name");
			int height = -1;
			int width = -1;
			int maxHeight = -1;
			int maxWidth = -1;
			int leftMargin = 0;
			int rightMargin = 0;
			int bottomMargin = 0;
			int topMargin = 0;
			String borderTitle = null;

			try {
				int margins = Integer.parseInt(getValue(atts, "margins"));
				if (margins < 0)
					margins = 0;
				leftMargin = margins;
				rightMargin = margins;
				bottomMargin = margins;
				topMargin = margins;
			} catch (Exception ex) {
			}

			try {
				leftMargin = Integer.parseInt(getValue(atts, "leftmargin"));
			} catch (Exception ex) {
			}

			try {
				rightMargin = Integer.parseInt(getValue(atts, "rightmargin"));
			} catch (Exception ex) {
			}

			try {
				topMargin = Integer.parseInt(getValue(atts, "topmargin"));
			} catch (Exception ex) {
			}

			try {
				bottomMargin = Integer.parseInt(getValue(atts, "bottommargin"));
			} catch (Exception ex) {
			}

			borderTitle = getValue(atts, "bordertitle");

			try {
				height = Integer.parseInt(getValue(atts, "height"));
			} catch (Exception e) {
			}
			try {
				width = Integer.parseInt(getValue(atts, "width"));
			} catch (Exception e) {
			}

			try {
				maxHeight = Integer.parseInt(getValue(atts, "maxHeight"));
			} catch (Exception e) {
			}
			try {
				maxWidth = Integer.parseInt(getValue(atts, "maxWidth"));
			} catch (Exception e) {
			}

			Font font = null;
			if (fontName != null) {
				font = Font.getFont(fontName);
				if (font == null)
					font = Font.decode(fontName);
			}

			Color foreground = null;
			if (foregroundStr != null) {
				foreground = decodeColor(foregroundStr);
			}

			Color background = null;
			if (backgroundStr != null) {
				background = decodeColor(backgroundStr);
			}

			if (font == null)
				font = defaultFont;

			item.foreground = foreground;
			item.setBackground(background, defaultBGColor);
			item.font = font;
			item.width = width;
			item.height = height;
			item.name = name;
			item.leftMargin = leftMargin;
			item.rightMargin = rightMargin;
			item.bottomMargin = bottomMargin;
			item.topMargin = topMargin;
			item.borderTitle = borderTitle;
			item.maxHeight = maxHeight;
			item.maxWidth = maxWidth;
		}

		private static int convertScrollString(String in) {
			if (in == null)
				return ScrollerElement.AS_NEEDED;
			if (in.equalsIgnoreCase("ALWAYS"))
				return ScrollerElement.ALWAYS;
			else if (in.equalsIgnoreCase("NEVER"))
				return ScrollerElement.NEVER;
			else
				return ScrollerElement.AS_NEEDED;
		}

		public void characters(char[] ch, int start, int length)
				throws SAXException {
			if (componentDepth > 0) {
				ComponentElement item = (ComponentElement) stack.elementAt(0);
				item.appendXMLData(new String(ch, start, length));
			}
		}

		public void endElement(java.lang.String uri,
				java.lang.String localName, java.lang.String qName) {
			localName = qName;
			if (localName.equals("component"))
				componentDepth--;

			if (componentDepth > 0) {
				ComponentElement item = (ComponentElement) stack.elementAt(0);
				item.appendXMLData("</" + localName + ">\n");
				// copy this end element to the xml string
			} else if (localName.equalsIgnoreCase("window")) {
				LayoutItem item = (LayoutItem) stack.elementAt(0);
				stack.removeElementAt(0);
				if (stack.size() == 0)
					windows.add(item);
			} else if (localName.equalsIgnoreCase("first")) {
				LayoutItem item = (LayoutItem) stack.elementAt(0);
				LayoutItem parent = (LayoutItem) stack.elementAt(1);
				((DividerElement) parent).setFirst(item);
				stack.removeElementAt(0);
			} else if (localName.equalsIgnoreCase("second")) {
				LayoutItem item = (LayoutItem) stack.elementAt(0);
				LayoutItem parent = (LayoutItem) stack.elementAt(1);
				((DividerElement) parent).setSecond(item);
				stack.removeElementAt(0);
			} else if (localName.equalsIgnoreCase("tab")) {
				LayoutItem item = (LayoutItem) stack.elementAt(0);
				LayoutItem parent = (LayoutItem) stack.elementAt(1);
				TabRecord record = (TabRecord) ((TabElement) parent).getItems()
						.get(((TabElement) parent).getItems().size() - 1);
				record.getItems().clear();
				record.addItem(item);
				// ((TabElement) parent).addItem(item);
				stack.removeElementAt(0);
			} else if (localName.equalsIgnoreCase("north")) {
				LayoutItem item = (LayoutItem) stack.elementAt(0);
				LayoutItem parent = (LayoutItem) stack.elementAt(1);
				((PanelElement) parent).setNorth(item);
				stack.removeElementAt(0);
			} else if (localName.equalsIgnoreCase("south")) {
				LayoutItem item = (LayoutItem) stack.elementAt(0);
				LayoutItem parent = (LayoutItem) stack.elementAt(1);
				((PanelElement) parent).setSouth(item);
				stack.removeElementAt(0);
			} else if (localName.equalsIgnoreCase("east")) {
				LayoutItem item = (LayoutItem) stack.elementAt(0);
				LayoutItem parent = (LayoutItem) stack.elementAt(1);
				((PanelElement) parent).setEast(item);
				stack.removeElementAt(0);
			} else if (localName.equalsIgnoreCase("west")) {
				LayoutItem item = (LayoutItem) stack.elementAt(0);
				LayoutItem parent = (LayoutItem) stack.elementAt(1);
				((PanelElement) parent).setWest(item);
				stack.removeElementAt(0);
			} else if (localName.equalsIgnoreCase("center")) {
				LayoutItem item = (LayoutItem) stack.elementAt(0);
				LayoutItem parent = (LayoutItem) stack.elementAt(1);
				((PanelElement) parent).setCenter(item);
				stack.removeElementAt(0);
			} else {
				if (stack.size() >= 2) {
					LayoutItem item = (LayoutItem) stack.elementAt(0);
					if (!(stack.get(1) instanceof LayoutContainerItem)) {
						LayoutContainerItem container = null;
						for (int i = 0; i < stack.size(); i++) {
							if (stack.get(i) instanceof LayoutContainerItem) {
								container = (LayoutContainerItem) stack.get(i);
								break;
							}
						}
						if (container == null)
							throw new RuntimeException("Error on line "
									+ locator.getLineNumber()
									+ ". maybe there are too many root tags?");
						else
							throw new RuntimeException(
									"Error on line "
											+ locator.getLineNumber()
											+ ". More than one child tag was added to a container (a <"
											+ container.getTagName()
											+ "> or one of its subtags) that only allows a single child.");
					}
					LayoutContainerItem parent = (LayoutContainerItem) stack
							.elementAt(1);

					if (parent.canAddDirectly()) {
						parent.addItem(item);
						stack.removeElementAt(0);
					}
				}
			}
		}

		// Parser calls this once after parsing a document
		public void endDocument() throws SAXException {

		}
	}
}
