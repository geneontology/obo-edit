/**
 * 
 */
package org.bbop.swing;

import java.awt.Color;
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.Shape;
import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.Highlighter;
import javax.swing.text.JTextComponent;
import javax.swing.text.LayeredHighlighter;
import javax.swing.text.Position;
import javax.swing.text.StyledDocument;
import javax.swing.text.View;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.ImageView;
import javax.swing.text.html.StyleSheet;

public abstract class AbstractIconView extends View {

	/**
	 * Icon used if the image could not be found.
	 */
	private static Icon sMissingImageIcon;

	/**
	 * File name for <code>sMissingImageIcon</code>.
	 */
	private static final String MISSING_IMAGE_SRC = "icons/image-failed.gif";

	/**
	 * Document property for image cache.
	 */
	private static final String IMAGE_CACHE_PROPERTY = "imageCache";

	// Height/width to use before we know the real size, these should at
	// least
	// the size of <code>sMissingImageIcon</code> and
	// <code>sPendingImageIcon</code>
	private static final int DEFAULT_WIDTH = 38;
	private static final int DEFAULT_HEIGHT = 38;

	/**
	 * Default border to use if one is not specified.
	 */
	private static final int DEFAULT_BORDER = 2;

	// Bitmask values
	private static final int LOADING_FLAG = 1;
	private static final int LINK_FLAG = 2;
	private static final int WIDTH_FLAG = 4;
	private static final int HEIGHT_FLAG = 8;
	private static final int RELOAD_FLAG = 16;
	private static final int RELOAD_IMAGE_FLAG = 32;
	private static final int SYNC_LOAD_FLAG = 64;

	private AttributeSet attr;
	private int width;
	private int height;
	/**
	 * Bitmask containing some of the above bitmask values. Because the image
	 * loading notification can happen on another thread access to this is
	 * synchronized (at least for modifying it).
	 */
	private int state;
	private Container container;
	private Rectangle fBounds;
	private Color borderColor;
	// Size of the border, the insets contains this valid. For example, if
	// the HSPACE attribute was 4 and BORDER 2, leftInset would be 6.
	private short borderSize;
	// Insets, obtained from the painter.
	private short leftInset;
	private short rightInset;
	private short topInset;
	private short bottomInset;
	private float vAlign;
	private View altView;

	protected Icon icon;

	public AbstractIconView(Element elem) {
		super(elem);
		icon = createIcon(PluggableImageHTMLEditorKit.getImageURL(elem), -1, -1);
	}

	/**
	 * Update any cached values that come from attributes.
	 */
	protected void setPropertiesFromAttributes() {
		StyleSheet sheet = getStyleSheet();
		this.attr = sheet.getViewAttributes(this);

		// Gutters
		borderSize = (short) getIntAttr(HTML.Attribute.BORDER,
				isLink() ? DEFAULT_BORDER : 0);

		leftInset = rightInset = (short) (getIntAttr(HTML.Attribute.HSPACE, 0) + borderSize);
		topInset = bottomInset = (short) (getIntAttr(HTML.Attribute.VSPACE, 0) + borderSize);

		borderColor = ((StyledDocument) getDocument())
				.getForeground(getAttributes());

		AttributeSet attr = getElement().getAttributes();

		// Alignment.
		// PENDING: This needs to be changed to support the CSS versions
		// when conversion from ALIGN to VERTICAL_ALIGN is complete.
		Object alignment = attr.getAttribute(HTML.Attribute.ALIGN);

		vAlign = 1.0f;
		if (alignment != null) {
			alignment = alignment.toString();
			if ("top".equals(alignment)) {
				vAlign = 0f;
			} else if ("middle".equals(alignment)) {
				vAlign = .5f;
			}
		}

		AttributeSet anchorAttr = (AttributeSet) attr.getAttribute(HTML.Tag.A);
		if (anchorAttr != null && anchorAttr.isDefined(HTML.Attribute.HREF)) {
			synchronized (this) {
				state |= LINK_FLAG;
			}
		} else {
			synchronized (this) {
				state = (state | LINK_FLAG) ^ LINK_FLAG;
			}
		}

	}

	/**
	 * Convenience method to get the StyleSheet.
	 */
	protected StyleSheet getStyleSheet() {
		HTMLDocument doc = (HTMLDocument) getDocument();
		return doc.getStyleSheet();
	}

	/**
	 * Convenience method for getting an integer attribute from the elements
	 * AttributeSet.
	 */
	private int getIntAttr(HTML.Attribute name, int deflt) {
		AttributeSet attr = getElement().getAttributes();
		if (attr.isDefined(name)) { // does not check parents!
			int i;
			String val = (String) attr.getAttribute(name);
			if (val == null) {
				i = deflt;
			} else {
				try {
					i = Math.max(0, Integer.parseInt(val));
				} catch (NumberFormatException x) {
					i = deflt;
				}
			}
			return i;
		} else
			return deflt;
	}

	/**
	 * Returns true if this image within a link?
	 */
	private boolean isLink() {
		return ((state & LINK_FLAG) == LINK_FLAG);
	}

	public abstract Icon createIcon(URL url, int width, int height);

	/**
	 * Paints the View.
	 * 
	 * @param g
	 *            the rendering surface to use
	 * @param a
	 *            the allocated region to render into
	 * @see View#paint
	 */
	public void paint(Graphics g, Shape a) {
		Rectangle rect = (a instanceof Rectangle) ? (Rectangle) a : a
				.getBounds();

		Rectangle clip = g.getClipBounds();

		fBounds.setBounds(rect);
		paintHighlights(g, a);
		paintBorder(g, rect);
		if (clip != null) {
			g.clipRect(rect.x + leftInset, rect.y + topInset, rect.width
					- leftInset - rightInset, rect.height - topInset
					- bottomInset);
		}
		if (icon != null) {
			icon.paintIcon(getContainer(), g, rect.x + leftInset, rect.y
					+ topInset);
		} else {
			Icon icon = getNoImageIcon();

			if (icon != null) {
				icon.paintIcon(getContainer(), g, rect.x + leftInset, rect.y
						+ topInset);
			}
		}
		if (clip != null) {
			// Reset clip.
			g.setClip(clip.x, clip.y, clip.width, clip.height);
		}
	}

	private void paintHighlights(Graphics g, Shape shape) {
		if (container instanceof JTextComponent) {
			JTextComponent tc = (JTextComponent) container;
			Highlighter h = tc.getHighlighter();
			if (h instanceof LayeredHighlighter) {
				((LayeredHighlighter) h).paintLayeredHighlights(g,
						getStartOffset(), getEndOffset(), shape, tc, this);
			}
		}
	}

	/**
	 * Returns the icon to use if the image couldn't be found.
	 */
	public Icon getNoImageIcon() {
		loadDefaultIconsIfNecessary();
		return sMissingImageIcon;
	}

	private void loadDefaultIconsIfNecessary() {
		try {
			if (sMissingImageIcon == null)
				sMissingImageIcon = makeIcon(MISSING_IMAGE_SRC);
		} catch (Exception x) {
			System.err.println("ImageView: Couldn't load image icons");
		}
	}

	private Icon makeIcon(final String gifFile) throws IOException {
		/*
		 * Copy resource into a byte array. This is necessary because several
		 * browsers consider Class.getResource a security risk because it can be
		 * used to load additional classes. Class.getResourceAsStream just
		 * returns raw bytes, which we can convert to an image.
		 */
		InputStream resource = getClass().getResourceAsStream(gifFile);

		if (resource == null) {
			System.err.println(ImageView.class.getName() + "/" + gifFile
					+ " not found.");
			return null;
		}
		BufferedInputStream in = new BufferedInputStream(resource);
		ByteArrayOutputStream out = new ByteArrayOutputStream(1024);
		byte[] buffer = new byte[1024];
		int n;
		while ((n = in.read(buffer)) > 0) {
			out.write(buffer, 0, n);
		}
		in.close();
		out.flush();

		buffer = out.toByteArray();
		if (buffer.length == 0) {
			System.err.println("warning: " + gifFile + " is zero-length");
			return null;
		}
		return new ImageIcon(buffer);
	}

	private void paintBorder(Graphics g, Rectangle rect) {
		Color color = borderColor;

		if ((borderSize > 0 || icon == null) && color != null) {
			int xOffset = leftInset - borderSize;
			int yOffset = topInset - borderSize;
			g.setColor(color);
			int n = (icon == null) ? 1 : borderSize;
			for (int counter = 0; counter < n; counter++) {
				g.drawRect(rect.x + xOffset + counter, rect.y + yOffset
						+ counter, rect.width - counter - counter - xOffset
						- xOffset - 1, rect.height - counter - counter
						- yOffset - yOffset - 1);
			}
		}
	}

	@Override
	public float getPreferredSpan(int axis) {
		switch (axis) {
		case View.X_AXIS:
			return width + leftInset + rightInset;
		case View.Y_AXIS:
			return height + topInset + bottomInset;
		default:
			throw new IllegalArgumentException("Invalid axis: " + axis);
		}
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
	 * @exception BadLocationException
	 *                if the given position does not represent a valid location
	 *                in the associated document
	 * @see View#modelToView
	 */
	public Shape modelToView(int pos, Shape a, Position.Bias b)
			throws BadLocationException {
		int p0 = getStartOffset();
		int p1 = getEndOffset();
		if ((pos >= p0) && (pos <= p1)) {
			Rectangle r = a.getBounds();
			if (pos == p1) {
				r.x += r.width;
			}
			r.width = 0;
			return r;
		}
		return null;
	}

	/**
	 * Provides a mapping from the view coordinate space to the logical
	 * coordinate space of the model.
	 * 
	 * @param x
	 *            the X coordinate
	 * @param y
	 *            the Y coordinate
	 * @param a
	 *            the allocated region to render into
	 * @return the location within the model that best represents the given
	 *         point of view
	 * @see View#viewToModel
	 */
	public int viewToModel(float x, float y, Shape a, Position.Bias[] bias) {
		Rectangle alloc = (Rectangle) a;
		if (x < alloc.x + alloc.width) {
			bias[0] = Position.Bias.Forward;
			return getStartOffset();
		}
		bias[0] = Position.Bias.Backward;
		return getEndOffset();
	}
}