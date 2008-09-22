package org.oboedit.gui.widget;

import java.awt.*;
import javax.swing.*;
import org.oboedit.gui.Preferences;

import org.apache.log4j.*;

public class TextIcon implements Icon {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(TextIcon.class);
	private static JComponent tempComponent = new JPanel();
	private String text;
	private Font font;
	private FontMetrics metrics;
	private Color foreground;
	private Color background;

	private int iconWidth;
	private int iconHeight;

	public TextIcon(String text) {
// For icons in GraphEditor the relationship name is split on _ to /n to increase readability
//		String newtext = text.replace("_","\n");
//		logger.debug("newtext:" + newtext);
//		logger.debug("newtext.length()" + newtext.length());
//needs to recalculate bounds - font metrics gets messy
		//		setText(newtext, true);

		setText(text, false);
		//setFont(new Font("Arial", 0, 12), false);
		setFont(Preferences.getPreferences().getFont(), false);
		setBackground(Color.lightGray);
		setForeground(Color.black);
		recalculateBounds();
//		logger.info("Created TextIcon(" + text + "), width = " + iconWidth + ", height = " + iconHeight); // DEL
	}

	public void setForeground(Color foreground) {
		this.foreground = foreground;
	}

	public void setBackground(Color background) {
		this.background = background;
	}

	public void setFont(Font font) {
		setFont(font, true);
	}

	private void setFont(Font font, boolean recalculateBounds) {
		this.font = font;
		metrics = tempComponent.getFontMetrics(font);
		if (recalculateBounds)
			recalculateBounds();
	}

	public void setText(String text) {
		setText(text, false);
	}

	private void setText(String text, boolean recalculateBounds) {
		this.text = text;
		if (recalculateBounds)
			recalculateBounds();
	}

	protected void recalculateBounds() { 
		iconWidth = 1 + metrics.stringWidth(text);
//		iconHeight = 1 + metrics.getHeight();
		iconHeight = metrics.getHeight()-2;  // Leave a little vertical space between text icons
	}

	public int getIconWidth() {
		return iconWidth;
	}

	public int getIconHeight() {
		return iconHeight;
	}

	public void paintIcon(Component c, Graphics g, int x, int y) {
		Color oldColor = g.getColor();
		Font oldFont = g.getFont();
		g.setFont(font);
		if (background != null) {
			g.setColor(background);
			g.fillRect(x, y, getIconWidth(), getIconHeight());
		}
		g.setColor(foreground);
//		g.drawString(text, x, y + iconHeight - 4);
		g.drawString(text, x, y + iconHeight - 2);
		g.setColor(oldColor);
		g.setFont(oldFont);
	}
}
