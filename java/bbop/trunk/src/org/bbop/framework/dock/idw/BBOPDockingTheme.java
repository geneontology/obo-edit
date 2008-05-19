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

import net.infonode.docking.properties.RootWindowProperties;
import net.infonode.docking.theme.DockingWindowsTheme;
import net.infonode.docking.util.PropertiesUtil;
import net.infonode.gui.colorprovider.ColorBlender;
import net.infonode.gui.colorprovider.ColorProvider;
import net.infonode.gui.colorprovider.FixedColorProvider;
import net.infonode.gui.componentpainter.ComponentPainter;
import net.infonode.gui.componentpainter.SolidColorComponentPainter;
import net.infonode.gui.laf.InfoNodeLookAndFeel;
import net.infonode.gui.shaped.border.FixedInsetsShapedBorder;
import net.infonode.gui.shaped.border.RoundedCornerBorder;
import net.infonode.gui.shaped.border.ShapedBorder;
import net.infonode.tabbedpanel.TabAreaProperties;

import java.awt.*;

/**
 * A light blue theme with gradients and rounded corners.
 * 
 * @author $Author: jmr39 $
 * @version $Revision: 1.3 $
 */
import org.apache.log4j.*;

public class BBOPDockingTheme extends DockingWindowsTheme {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(BBOPDockingTheme.class);
	private RootWindowProperties rootWindowProperties = new RootWindowProperties();

	BBOPTabTheme theme;

	/**
	 * Constructor.
	 * 
	 * @param darkColor
	 *            the dark color used in the gradients
	 * @param lightColor
	 *            the light color used in the gradients
	 * @param cornerType
	 *            how much rounding to apply to corners
	 * @param slim
	 *            if true there is less spacing in the tab area
	 */
	public BBOPDockingTheme(ColorProvider darkColor,
			ColorProvider lightColor, ColorProvider background, int cornerType,
			Font font) {
		configure(darkColor, lightColor, background, cornerType, font);
	}

	public void configure(Color darkColor, Color lightColor,
			Color backgroundColor, int cornerType, Font font) {
		configure(new FixedColorProvider(darkColor), new FixedColorProvider(
				lightColor), new FixedColorProvider(backgroundColor),
				cornerType, font);
	}

	public void configure(ColorProvider darkColor, ColorProvider lightColor,
			ColorProvider backgroundColor, int cornerType,
			Font font) {
		theme = new BBOPTabTheme(darkColor, lightColor, rootWindowProperties
				.getComponentProperties().getFont(), cornerType);
		
		RootWindowProperties titleBarStyleProperties = PropertiesUtil.createTitleBarStyleRootWindowProperties();
		rootWindowProperties.addSuperObject(titleBarStyleProperties);
		
		rootWindowProperties.setRecursiveTabsEnabled(false);
		// TODO Come back here if the font messes up in IDW
		// rootWindowProperties.getComponentProperties().setFont(Preferences.defaultFont());
		rootWindowProperties.getWindowAreaProperties().setBorder(null)
				.setInsets(new Insets(2, 2, 2, 2));
		rootWindowProperties.getWindowAreaShapedPanelProperties()
				.setComponentPainter(
						new SolidColorComponentPainter(new ColorBlender(
								darkColor, lightColor, 0.5f)));

		rootWindowProperties.getTabWindowProperties()
				.getTabbedPanelProperties().addSuperObject(
						theme.getTabbedPanelProperties());
		rootWindowProperties.getTabWindowProperties().getTabProperties()
				.getTitledTabProperties().addSuperObject(
						theme.getTitledTabProperties());

		rootWindowProperties.getShapedPanelProperties().setComponentPainter(
				theme.getTabbedPanelProperties().getTabAreaProperties()
						.getShapedPanelProperties().getComponentPainter());

		rootWindowProperties.getTabWindowProperties()
				.getTabbedPanelProperties().getContentPanelProperties()
				.getShapedPanelProperties().setClipChildren(true);
		rootWindowProperties.getViewProperties().getViewTitleBarProperties()
				.getNormalProperties().getComponentProperties()
				.setForegroundColor(Color.BLACK).setInsets(
						new Insets(0, 2, 0, 2)).setFont(
						font.deriveFont(Font.BOLD));
		rootWindowProperties.getViewProperties().getViewTitleBarProperties()
				.getNormalProperties().getShapedPanelProperties()
				.setComponentPainter(
						new SolidColorComponentPainter(backgroundColor))
				.setOpaque(false);
		rootWindowProperties.getViewProperties().getViewTitleBarProperties()
				.getNormalProperties().getComponentProperties().setBorder(
						new RoundedCornerBorder(null, lightColor, cornerType,
								cornerType, 0, 0, true, true, true, true));
		rootWindowProperties.getViewProperties().getViewTitleBarProperties()
				.getFocusedProperties().getShapedPanelProperties()
				.setComponentPainter(new SolidColorComponentPainter(darkColor));
		/*
		 * rootWindowProperties.getViewProperties().getViewTitleBarProperties()
		 * .getNormalProperties().getShapedPanelProperties()
		 * .setComponentPainter(null).setOpaque(false);
		 * rootWindowProperties.getViewProperties().getViewTitleBarProperties()
		 * .getFocusedProperties().getShapedPanelProperties()
		 * .setComponentPainter(null);// .setOpaque(false);
		 */
		rootWindowProperties.getViewProperties().getViewTitleBarProperties()
				.getFocusedProperties().getComponentProperties()
				.setForegroundColor(Color.BLACK);

		TabAreaProperties p = rootWindowProperties.getWindowBarProperties()
				.getTabWindowProperties().getTabbedPanelProperties()
				.getTabAreaProperties();

		p.getShapedPanelProperties().setComponentPainter(null);
		p.getComponentProperties().setBorder(null);

		rootWindowProperties.getWindowBarProperties().getTabWindowProperties()
				.getTabbedPanelProperties().getTabAreaComponentsProperties()
				.getComponentProperties().setBorder(null);

		rootWindowProperties.getWindowBarProperties().getTabWindowProperties()
				.getTabbedPanelProperties().getContentPanelProperties()
				.getShapedPanelProperties().setOpaque(false);

	}

	public String getName() {
		return "Default BBOP Docking Theme";
	}

	public RootWindowProperties getRootWindowProperties() {
		return rootWindowProperties;
	}
}
