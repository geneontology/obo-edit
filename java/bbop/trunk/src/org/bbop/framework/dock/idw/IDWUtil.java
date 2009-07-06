package org.bbop.framework.dock.idw;

import java.awt.Color;
import java.awt.Insets;
import java.awt.event.ActionListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;

import javax.swing.AbstractButton;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JToggleButton;
import javax.swing.SwingConstants;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ButtonUI;

import net.infonode.docking.View;
import net.infonode.gui.ComponentUtil;
import net.infonode.gui.FlatIconButtonUI;
import net.infonode.gui.UIManagerUtil;
import net.infonode.gui.colorprovider.FixedColorProvider;
import net.infonode.gui.componentpainter.SolidColorComponentPainter;
import net.infonode.util.ColorUtil;

import org.apache.log4j.*;

public class IDWUtil {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IDWUtil.class);
	protected static class ButtonHighlighter implements ComponentListener,
			HierarchyListener {
		/*
		 * private static final Color HIGHLIGHTED_COLOR = new Color(140, 160,
		 * 255); private static final Color PRESSED_COLOR = new Color(60, 80,
		 * 200);
		 */
		private AbstractButton button;

		private Border pressedBorder;

		private Border highlightedBorder;

		private Border normalBorder;

		private boolean rollover;

		private long rolloverStart; // Ugly hack to avoid false rollover
									// callbacks which occur when the button is
									// moved

		ButtonHighlighter(AbstractButton button, int padding) {
			this.button = button;

			normalBorder = new EmptyBorder(padding + 2, padding + 2,
					padding + 2, padding + 2);
			pressedBorder = new EmptyBorder(padding + 2, padding + 2, padding,
					padding);
			highlightedBorder = new EmptyBorder(padding + 1, padding + 1,
					padding + 1, padding + 1);

			button.setContentAreaFilled(false);
			setNormalState();

			button.addChangeListener(new ChangeListener() {
				public void stateChanged(ChangeEvent e) {
					rollover = (System.currentTimeMillis() - rolloverStart) > 20
							&& ButtonHighlighter.this.button.getModel()
									.isRollover();
					update();

					if (ButtonHighlighter.this.button.getModel().isRollover())
						rolloverStart = 0;
				}
			});

			button.addHierarchyListener(this);
			button.addComponentListener(this);
		}

		private void setNormalState() {
			button.setBackground(null);
			button.setOpaque(false);
			button.setBorder(normalBorder);
			rollover = false;
		}

		public void componentHidden(ComponentEvent e) {
			setNormalState();
			rolloverStart = System.currentTimeMillis();
		}

		public void componentMoved(ComponentEvent e) {
			setNormalState();
			rolloverStart = System.currentTimeMillis();
		}

		public void componentResized(ComponentEvent e) {
			setNormalState();
			rolloverStart = System.currentTimeMillis();
		}

		public void componentShown(ComponentEvent e) {
			setNormalState();
			rolloverStart = System.currentTimeMillis();
		}

		public void hierarchyChanged(HierarchyEvent e) {
			setNormalState();
			rolloverStart = System.currentTimeMillis();
		}

		private void update() {
			boolean pressed = button.getModel().isArmed();

			if (button.isEnabled() && (rollover || pressed)) {
				button.setOpaque(true);
				Color backgroundColor = ComponentUtil.getBackgroundColor(button
						.getParent());
				backgroundColor = backgroundColor == null ? UIManagerUtil
						.getColor("control", Color.LIGHT_GRAY)
						: backgroundColor;
				button.setBackground(ColorUtil.mult(backgroundColor,
						pressed ? 0.8 : 1.15));

				button.setBorder(pressed ? new CompoundBorder(new LineBorder(
						ColorUtil.mult(backgroundColor, 0.3)), pressedBorder)
						: new CompoundBorder(new LineBorder(ColorUtil.mult(
								backgroundColor, 0.5)), highlightedBorder));
			} else {
				setNormalState();
			}
		}

	}

	public static void setTitleBarColor(View view, Color focused,
			Color unfocused) {
		view.getViewProperties().getViewTitleBarProperties()
				.getNormalProperties().getShapedPanelProperties()
				.setComponentPainter(
						new SolidColorComponentPainter(new FixedColorProvider(
								unfocused))).setOpaque(false);
		view.getViewProperties().getViewTitleBarProperties()
				.getFocusedProperties().getShapedPanelProperties()
				.setComponentPainter(
						new SolidColorComponentPainter(new FixedColorProvider(
								focused))).setOpaque(false);
	}
	
	public static JButton createFlatHighlightButton(Icon icon,
			String tooltipText, int padding, ActionListener action) {
		final JButton b = new JButton(icon) {
			@Override
			public void setUI(ButtonUI ui) {
				super.setUI(new FlatIconButtonUI());
			}
		};
		b.setFocusable(false);
		b.setVerticalAlignment(SwingConstants.CENTER);
		b.setToolTipText(tooltipText);
		b.setMargin(new Insets(0, 0, 0, 0));
		new ButtonHighlighter(b, padding);

		b.setRolloverEnabled(true);

		if (action != null)
			b.addActionListener(action);

		return b;
	}

	public static JToggleButton createFlatHighlightToggleButton(Icon icon,
			String tooltipText, int padding, ActionListener action) {
		final JToggleButton b = new JToggleButton(icon) {
			@Override
			public void setUI(ButtonUI ui) {
				super.setUI(new FlatIconButtonUI());
			}
		};
		b.setFocusable(false);
		b.setVerticalAlignment(SwingConstants.CENTER);
		b.setToolTipText(tooltipText);
		b.setMargin(new Insets(0, 0, 0, 0));
		new ButtonHighlighter(b, padding);

		b.setRolloverEnabled(true);

		if (action != null)
			b.addActionListener(action);

		return b;
	}
}
