package org.bbop.framework.dock.idw;

/*
 * Copyright (C) 2004 NNL Technology AB
 * Visit www.infonode.net for information about InfoNode(R) 
 * products and how to contact NNL Technology AB.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, 
 * MA 02111-1307, USA.
 */

import net.infonode.gui.colorprovider.ColorBlender;
import net.infonode.gui.colorprovider.ColorProvider;
import net.infonode.gui.colorprovider.FixedColorProvider;
import net.infonode.gui.componentpainter.ComponentPainter;
import net.infonode.gui.componentpainter.FixedTransformComponentPainter;
import net.infonode.gui.componentpainter.GradientComponentPainter;
import net.infonode.gui.hover.HoverListener;
import net.infonode.gui.shaped.border.RoundedCornerBorder;
import net.infonode.properties.base.Property;
import net.infonode.properties.gui.util.ComponentProperties;
import net.infonode.tabbedpanel.TabbedPanelProperties;
import net.infonode.tabbedpanel.hover.TitledTabHoverAction;
import net.infonode.tabbedpanel.theme.TabbedPanelTitledTabTheme;
import net.infonode.tabbedpanel.titledtab.TitledTabProperties;
import net.infonode.util.ColorUtil;
import net.infonode.util.Direction;

import javax.swing.border.Border;
import javax.swing.border.EmptyBorder;

import java.awt.*;

/**
 * A light blue theme with gradients and rounded corners.
 * 
 * @author $Author: jmr39 $
 * @version $Revision: 1.2 $
 * @since ITP 1.2.0
 */
public class BBOPTabTheme extends TabbedPanelTitledTabTheme {

	// private ColorProvider darkColor;

	// private ColorProvider lightColor;

	private TabbedPanelProperties tabbedPanelProperties = new TabbedPanelProperties();

	private TitledTabProperties titledTabProperties = new TitledTabProperties();

	public void configureLook(ColorProvider darkColor,
			ColorProvider lightColor, Font font) {
		configureLook(darkColor, lightColor, font, 4);
	}

	public void configureLook(ColorProvider darkColor,
			ColorProvider lightColor, Font font, int cornerType) {

		ColorProvider light = lightColor;
		ColorProvider dark = new ColorBlender(darkColor, lightColor, 0.3f);
		ColorProvider dark2 = new ColorBlender(darkColor,
				FixedColorProvider.WHITE, 0.1f);
		ColorProvider dark3 = darkColor;

		Border roundedBorder = new RoundedCornerBorder(dark3, light,
				cornerType, cornerType, 0, 0, true, true, true, true);

		Border tabNormalBorder = roundedBorder;
		
		Border contentBorder = new RoundedCornerBorder(dark3, light,
				cornerType, cornerType, cornerType, cornerType, false, true,
				true, true);

		ComponentPainter areaPainter = new FixedTransformComponentPainter(
				new GradientComponentPainter(dark2, light, light, dark2));
		ComponentPainter contentPainter = new FixedTransformComponentPainter(
				new GradientComponentPainter(light, dark2, dark2, light));
		ComponentPainter highlightPainter = new FixedTransformComponentPainter(
				new GradientComponentPainter(FixedColorProvider.WHITE, light,
						light, light));
		ComponentPainter normalPainter = new FixedTransformComponentPainter(
				new GradientComponentPainter(light, dark, dark, dark));

		tabbedPanelProperties.setPaintTabAreaShadow(true).setTabSpacing(2)
				.setShadowEnabled(false);
		tabbedPanelProperties.getTabAreaProperties().getComponentProperties()
				.setInsets(new Insets(2, 2, 3, 3));

		// why was this commented out?
//		tabbedPanelProperties.getTabAreaProperties().getComponentProperties()
//				.setBorder(roundedBorder).setInsets(new Insets(2, 2, 3, 3));
//
//		tabbedPanelProperties.getTabAreaProperties().getShapedPanelProperties()
//				.setClipChildren(true).setComponentPainter(areaPainter)
//				.setOpaque(false);
		// end mystery comment region

		tabbedPanelProperties.getTabAreaComponentsProperties()
				.setStretchEnabled(true).getComponentProperties().setBorder(
						null).setInsets(new Insets(0, 0, 0, 0));

		tabbedPanelProperties.getTabAreaComponentsProperties()
				.getShapedPanelProperties().setOpaque(false);

		tabbedPanelProperties.getContentPanelProperties()
				.getComponentProperties().setBorder(contentBorder).setInsets(
						new Insets(3, 3, 4, 4));

		tabbedPanelProperties.getContentPanelProperties()
				.getShapedPanelProperties().setComponentPainter(contentPainter)
				.setClipChildren(true).setOpaque(false);

		titledTabProperties.setHighlightedRaised(0);
		tabbedPanelProperties.setTabAreaOrientation(Direction.UP);

		/*
		 * Font font = titledTabProperties.getNormalProperties()
		 * .getComponentProperties().getFont();
		 */

		if (font != null)
			font = font.deriveFont(Font.PLAIN).deriveFont(11f);

		titledTabProperties.getNormalProperties().getComponentProperties()
				.setBorder(tabNormalBorder).setInsets(new Insets(1, 4, 2, 5))
				.setBackgroundColor(
						titledTabProperties.getHighlightedProperties()
								.getComponentProperties().getBackgroundColor())
				.setFont(font);

		titledTabProperties.getNormalProperties().getShapedPanelProperties()
				.setComponentPainter(normalPainter).setOpaque(false);

		Property[] linkedProperties = { ComponentProperties.BORDER,
				ComponentProperties.INSETS, ComponentProperties.FONT };

		for (int i = 0; i < linkedProperties.length; i++) {
			titledTabProperties.getHighlightedProperties()
					.getComponentProperties().getMap().createRelativeRef(
							linkedProperties[i],
							titledTabProperties.getNormalProperties()
									.getComponentProperties().getMap(),
							linkedProperties[i]);
		}

		titledTabProperties.getHighlightedProperties()
				.getShapedPanelProperties().setComponentPainter(
						highlightPainter);
	}

	/**
	 * Constructor.
	 * 
	 * @param darkColor
	 *            the dark color used in gradients
	 * @param lightColor
	 *            the light color used in gradients
	 * @param cornerType
	 *            the amount of rounding to use for corners, 0-4
	 */
	public BBOPTabTheme(ColorProvider darkColor, ColorProvider lightColor,
			Font font, int cornerType) {
		configureLook(lightColor, darkColor, font);

	}

	public String getName() {
		return "Default BBOP Tab Theme";
	}

	public TabbedPanelProperties getTabbedPanelProperties() {
		return tabbedPanelProperties;
	}

	public TitledTabProperties getTitledTabProperties() {
		return titledTabProperties;
	}
}
