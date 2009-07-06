package org.bbop.swing;

import java.awt.Color;
import java.awt.Container;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.beans.PropertyChangeEvent;
import java.io.StringReader;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.plaf.basic.BasicLabelUI;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.Position;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.ImageView;
import javax.swing.text.html.StyleSheet;

import org.apache.log4j.*;

public class ExtensibleLabelUI extends BasicLabelUI {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ExtensibleLabelUI.class);
	/**
	 * Overrides to the default stylesheet. Should consider just creating a
	 * completely fresh stylesheet.
	 */
	private static final String styleChanges = "p { margin-top: 0; margin-bottom: 0; margin-left: 0; margin-right: 0 }"
			+ "body { margin-top: 0; margin-bottom: 0; margin-left: 0; margin-right: 0 }";

	protected BasicEditorKit editorKit = new BasicEditorKit();

	/**
	 * Root text view that acts as an HTML renderer.
	 */
	public static class Renderer extends View {

		Renderer(JComponent c, ViewFactory f, View v) {
			super(null);
			host = c;
			factory = f;
			view = v;
			view.setParent(this);
			// initially layout to the preferred size
			setSize(view.getPreferredSpan(X_AXIS), view
					.getPreferredSpan(Y_AXIS));
		}

		/**
		 * Fetches the attributes to use when rendering. At the root level there
		 * are no attributes. If an attribute is resolved up the view hierarchy
		 * this is the end of the line.
		 */
		@Override
		public AttributeSet getAttributes() {
			return null;
		}

		/**
		 * Determines the preferred span for this view along an axis.
		 * 
		 * @param axis
		 *            may be either X_AXIS or Y_AXIS
		 * @return the span the view would like to be rendered into. Typically
		 *         the view is told to render into the span that is returned,
		 *         although there is no guarantee. The parent may choose to
		 *         resize or break the view.
		 */
		@Override
		public float getPreferredSpan(int axis) {
			if (axis == X_AXIS) {
				// width currently laid out to
				return width;
			}
			return view.getPreferredSpan(axis);
		}

		/**
		 * Determines the minimum span for this view along an axis.
		 * 
		 * @param axis
		 *            may be either X_AXIS or Y_AXIS
		 * @return the span the view would like to be rendered into. Typically
		 *         the view is told to render into the span that is returned,
		 *         although there is no guarantee. The parent may choose to
		 *         resize or break the view.
		 */
		@Override
		public float getMinimumSpan(int axis) {
			return view.getMinimumSpan(axis);
		}

		/**
		 * Determines the maximum span for this view along an axis.
		 * 
		 * @param axis
		 *            may be either X_AXIS or Y_AXIS
		 * @return the span the view would like to be rendered into. Typically
		 *         the view is told to render into the span that is returned,
		 *         although there is no guarantee. The parent may choose to
		 *         resize or break the view.
		 */
		@Override
		public float getMaximumSpan(int axis) {
			return Integer.MAX_VALUE;
		}

		/**
		 * Specifies that a preference has changed. Child views can call this on
		 * the parent to indicate that the preference has changed. The root view
		 * routes this to invalidate on the hosting component.
		 * <p>
		 * This can be called on a different thread from the event dispatching
		 * thread and is basically unsafe to propagate into the component. To
		 * make this safe, the operation is transferred over to the event
		 * dispatching thread for completion. It is a design goal that all view
		 * methods be safe to call without concern for concurrency, and this
		 * behavior helps make that true.
		 * 
		 * @param child
		 *            the child view
		 * @param width
		 *            true if the width preference has changed
		 * @param height
		 *            true if the height preference has changed
		 */
		@Override
		public void preferenceChanged(View child, boolean width, boolean height) {
			host.revalidate();
			host.repaint();
		}

		/**
		 * Determines the desired alignment for this view along an axis.
		 * 
		 * @param axis
		 *            may be either X_AXIS or Y_AXIS
		 * @return the desired alignment, where 0.0 indicates the origin and 1.0
		 *         the full span away from the origin
		 */
		@Override
		public float getAlignment(int axis) {
			return view.getAlignment(axis);
		}

		/**
		 * Renders the view.
		 * 
		 * @param g
		 *            the graphics context
		 * @param allocation
		 *            the region to render into
		 */
		@Override
		public void paint(Graphics g, Shape allocation) {
			Rectangle alloc = allocation.getBounds();
			view.setSize(alloc.width, alloc.height);
			view.paint(g, allocation);
		}

		/**
		 * Sets the view parent.
		 * 
		 * @param parent
		 *            the parent view
		 */
		@Override
		public void setParent(View parent) {
			throw new Error("Can't set parent on root view");
		}

		/**
		 * Returns the number of views in this view. Since this view simply
		 * wraps the root of the view hierarchy it has exactly one child.
		 * 
		 * @return the number of views
		 * @see #getView
		 */
		@Override
		public int getViewCount() {
			return 1;
		}

		/**
		 * Gets the n-th view in this container.
		 * 
		 * @param n
		 *            the number of the view to get
		 * @return the view
		 */
		@Override
		public View getView(int n) {
			return view;
		}

		/**
		 * Provides a mapping from the document model coordinate space to the
		 * coordinate space of the view mapped to it.
		 * 
		 * @param pos
		 *            the position to convert
		 * @param a
		 *            the allocated region to render into
		 * @return the bounding box of the given position
		 */
		@Override
		public Shape modelToView(int pos, Shape a, Position.Bias b)
				throws BadLocationException {
			return view.modelToView(pos, a, b);
		}

		/**
		 * Provides a mapping from the document model coordinate space to the
		 * coordinate space of the view mapped to it.
		 * 
		 * @param p0
		 *            the position to convert >= 0
		 * @param b0
		 *            the bias toward the previous character or the next
		 *            character represented by p0, in case the position is a
		 *            boundary of two views.
		 * @param p1
		 *            the position to convert >= 0
		 * @param b1
		 *            the bias toward the previous character or the next
		 *            character represented by p1, in case the position is a
		 *            boundary of two views.
		 * @param a
		 *            the allocated region to render into
		 * @return the bounding box of the given position is returned
		 * @exception BadLocationException
		 *                if the given position does not represent a valid
		 *                location in the associated document
		 * @exception IllegalArgumentException
		 *                for an invalid bias argument
		 * @see View#viewToModel
		 */
		@Override
		public Shape modelToView(int p0, Position.Bias b0, int p1,
				Position.Bias b1, Shape a) throws BadLocationException {
			return view.modelToView(p0, b0, p1, b1, a);
		}

		/**
		 * Provides a mapping from the view coordinate space to the logical
		 * coordinate space of the model.
		 * 
		 * @param x
		 *            x coordinate of the view location to convert
		 * @param y
		 *            y coordinate of the view location to convert
		 * @param a
		 *            the allocated region to render into
		 * @return the location within the model that best represents the given
		 *         point in the view
		 */
		@Override
		public int viewToModel(float x, float y, Shape a, Position.Bias[] bias) {
			return view.viewToModel(x, y, a, bias);
		}

		/**
		 * Returns the document model underlying the view.
		 * 
		 * @return the model
		 */
		@Override
		public Document getDocument() {
			return view.getDocument();
		}

		/**
		 * Returns the starting offset into the model for this view.
		 * 
		 * @return the starting offset
		 */
		@Override
		public int getStartOffset() {
			return view.getStartOffset();
		}

		/**
		 * Returns the ending offset into the model for this view.
		 * 
		 * @return the ending offset
		 */
		@Override
		public int getEndOffset() {
			return view.getEndOffset();
		}

		/**
		 * Gets the element that this view is mapped to.
		 * 
		 * @return the view
		 */
		@Override
		public Element getElement() {
			return view.getElement();
		}

		/**
		 * Sets the view size.
		 * 
		 * @param width
		 *            the width
		 * @param height
		 *            the height
		 */
		@Override
		public void setSize(float width, float height) {
			this.width = (int) width;
			view.setSize(width, height);
		}

		/**
		 * Fetches the container hosting the view. This is useful for things
		 * like scheduling a repaint, finding out the host components font, etc.
		 * The default implementation of this is to forward the query to the
		 * parent view.
		 * 
		 * @return the container
		 */
		@Override
		public Container getContainer() {
			return host;
		}

		/**
		 * Fetches the factory to be used for building the various view
		 * fragments that make up the view that represents the model. This is
		 * what determines how the model will be represented. This is
		 * implemented to fetch the factory provided by the associated
		 * EditorKit.
		 * 
		 * @return the factory
		 */
		@Override
		public ViewFactory getViewFactory() {
			return factory;
		}

		private int width;
		private View view;
		private ViewFactory factory;
		private JComponent host;

	}

	protected static class BasicDocument extends HTMLDocument {
		/** The host, that is where we are rendering. */
		// private JComponent host;
		BasicDocument(StyleSheet s, Font defaultFont, Color foreground) {
			super(s);
			setPreservesUnknownTags(false);
			setFontAndColor(defaultFont, foreground);
		}

		/**
		 * Sets the default font and default color. These are set by adding a
		 * rule for the body that specifies the font and color. This allows the
		 * html to override these should it wish to have a custom font or color.
		 */
		private void setFontAndColor(Font font, Color fg) {
			getStyleSheet().addRule(displayPropertiesToCSS(font, fg));
		}
	}

	protected static String displayPropertiesToCSS(Font font, Color fg) {
		StringBuffer rule = new StringBuffer("body {");
		if (font != null) {
			rule.append(" font-family: ");
			rule.append(font.getFamily());
			rule.append(" ; ");
			rule.append(" font-size: ");
			rule.append(font.getSize());
			rule.append("pt ;");
			if (font.isBold()) {
				rule.append(" font-weight: 700 ; ");
			}
			if (font.isItalic()) {
				rule.append(" font-style: italic ; ");
			}
		}
		if (fg != null) {
			rule.append(" color: #");
			if (fg.getRed() < 16) {
				rule.append('0');
			}
			rule.append(Integer.toHexString(fg.getRed()));
			if (fg.getGreen() < 16) {
				rule.append('0');
			}
			rule.append(Integer.toHexString(fg.getGreen()));
			if (fg.getBlue() < 16) {
				rule.append('0');
			}
			rule.append(Integer.toHexString(fg.getBlue()));
			rule.append(" ; ");
		}
		rule.append(" }");
		return rule.toString();
	}

	public static class BasicEditorKit extends PluggableImageHTMLEditorKit {
		/** Shared base style for all documents created by us use. */
		private static StyleSheet defaultStyles;

		protected class BasicHTMLViewFactory extends PluggableImageHTMLFactory {
			@Override
			public View create(Element elem) {
				View view = super.create(elem);

				if (view instanceof ImageView) {
					((ImageView) view).setLoadsSynchronously(true);
				}
				return view;
			}
		}

		/**
		 * Overriden to return our own slimmed down style sheet.
		 */
		@Override
		public StyleSheet getStyleSheet() {
			if (defaultStyles == null) {
				defaultStyles = new StyleSheet();
				StringReader r = new StringReader(styleChanges);
				try {
					defaultStyles.loadRules(r, null);
				} catch (Throwable e) {
					// don't want to die in static initialization...
					// just display things wrong.
				}
				r.close();
				defaultStyles.addStyleSheet(super.getStyleSheet());
			}
			return defaultStyles;
		}

		/**
		 * Sets the async policy to flush everything in one chunk, and to not
		 * display unknown tags.
		 */
		public Document createDefaultDocument(Font defaultFont, Color foreground) {
			StyleSheet styles = getStyleSheet();
			StyleSheet ss = new StyleSheet();
			ss.addStyleSheet(styles);
			BasicDocument doc = new BasicDocument(ss, defaultFont, foreground);
			doc.setAsynchronousLoadPriority(Integer.MAX_VALUE);
			doc.setPreservesUnknownTags(false);
			return doc;
		}

		/**
		 * Returns the ViewFactory that is used to make sure the Views don't
		 * load in the background.
		 */
		@Override
		public ViewFactory getViewFactory() {
			return new BasicHTMLViewFactory();
		}
	}

	boolean disableHTML = false;

	protected IconFactoryRegistry.IconRegistryListener registryListener = new IconFactoryRegistry.IconRegistryListener() {

		public void factoryInstalled(String suffix, IconFactory factory) {
			installFactory(suffix, factory);
		}

		public void factoryRemoved(String suffix, IconFactory factory) {
			uninstallFactory(suffix, factory);
		}
	};

	public ExtensibleLabelUI() {
		for (String suffix : IconFactoryRegistry.getSuffixes())
			installFactory(suffix, IconFactoryRegistry.getFactory(suffix));
		IconFactoryRegistry.addListener(registryListener);
	}

	@Override
	protected void finalize() throws Throwable {
		super.finalize();
		IconFactoryRegistry.removeListener(registryListener);
	}

	@Override
	public void propertyChange(PropertyChangeEvent e) {
		String name = e.getPropertyName();
		if (name == "text" || "font" == name || "foreground" == name) {
			// remove the old html view client property if one
			// existed, and install a new one if the text installed
			// into the JLabel is html source.
			JLabel lbl = ((JLabel) e.getSource());
			String text = lbl.getText();
			updateRenderer(lbl, text);
		} else if (name == "labelFor" || name == "displayedMnemonic") {
			installKeyboardActions((JLabel) e.getSource());
		}
	}

	@Override
	protected void installComponents(JLabel c) {
		updateRenderer(c, c.getText());
		c.setInheritsPopupMenu(true);
	}

	@Override
	protected void uninstallComponents(JLabel c) {
		updateRenderer(c, "");
	}

	public void updateRenderer(JComponent c, String text) {
		View value = null;
		View oldValue = (View) c.getClientProperty(BasicHTML.propertyKey);
		if (!disableHTML && BasicHTML.isHTMLString(text)) {
			value = HTMLLabel.createHTMLView(editorKit, c, text);
		}
		if (value != oldValue && oldValue != null) {
			for (int i = 0; i < oldValue.getViewCount(); i++) {
				oldValue.getView(i).setParent(null);
			}
		}
		c.putClientProperty(BasicHTML.propertyKey, value);
	}

	public void installFactory(String suffix, IconFactory factory) {
		editorKit.installFactory(suffix, factory);
	}

	public void uninstallFactory(String suffix, IconFactory factory) {
		editorKit.uninstallFactory(suffix, factory);
	}

	public boolean isDisableHTML() {
		return disableHTML;
	}

	public void setDisableHTML(boolean disableHTML) {
		this.disableHTML = disableHTML;
	}

	public BasicEditorKit getEditorKit() {
		return editorKit;
	}
}
