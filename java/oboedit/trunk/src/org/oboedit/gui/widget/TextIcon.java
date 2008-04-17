package org.oboedit.gui.widget;

import java.awt.*;
import javax.swing.*;
import org.oboedit.gui.Preferences;

public class TextIcon implements Icon {
	private static JComponent tempComponent = new JPanel();
	private String text;
	private Font font;
	private FontMetrics metrics;
	private Color foreground;
	private Color background;

	private int iconWidth;
	private int iconHeight;

	public TextIcon(String text) {
		setText(text, false);
//		setFont(new Font("Arial", 0, 9), false);
		// Check user's font setting (Preferences.getPreferences().getFont()) and use that
		setFont(Preferences.getPreferences().getFont(), false);
		setBackground(Color.black);
		setForeground(Color.white);
		recalculateBounds();
//		System.out.println("Created TextIcon(" + text + "), width = " + iconWidth + ", height = " + iconHeight); // DEL
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
//		iconWidth = 2 + metrics.stringWidth(text);
		iconWidth = 1 + metrics.stringWidth(text);
		iconHeight = 1 + metrics.getHeight();
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
//		g.drawString(text, x + 1, y + iconHeight - 2);
		g.drawString(text, x, y + iconHeight - 4);
		g.setFont(oldFont);
		g.setColor(oldColor);
	}
}
