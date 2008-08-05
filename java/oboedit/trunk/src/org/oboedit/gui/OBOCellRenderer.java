package org.oboedit.gui;

import org.bbop.swing.*;
import org.bbop.util.ObjectUtil;
import org.obo.datamodel.*;
import org.obo.filters.*;
import org.obo.util.FilterUtil;
import org.obo.util.TermUtil;
import org.oboedit.controller.FilterManager;
import org.oboedit.controller.SelectionManager;
import org.oboedit.gui.components.OBOTermPanel;
import org.oboedit.gui.filter.BackgroundColorSpecField;
import org.oboedit.gui.filter.ColorProvider;
import org.oboedit.gui.filter.ForegroundColorSpecField;
import org.oboedit.gui.filter.ConfiguredColor;
import org.oboedit.gui.filter.GeneralRendererSpec;
import org.oboedit.gui.filter.GeneralRendererSpecField;
import org.oboedit.gui.filter.LineTypeSpecField;
import org.oboedit.gui.filter.LineWidthSpecField;
import org.oboedit.gui.filter.LinkRenderSpec;
import org.oboedit.gui.filter.ObjectRenderSpec;
import org.oboedit.gui.filter.RenderSpec;
import org.oboedit.gui.filter.RenderedFilter;
import org.oboedit.gui.widget.TextIcon;
import org.oboedit.util.GUIUtil;
import org.oboedit.util.PathUtil;

import javax.swing.tree.*;
import javax.swing.border.*;
import javax.swing.*;
import java.awt.*;
import java.awt.geom.*;
import java.net.*;
import java.util.*;

import org.apache.log4j.*;

public class OBOCellRenderer extends JLabel implements TreeCellRenderer,
	ListCellRenderer {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBOCellRenderer.class);

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected final static Color ignoreSelectionColor = new Color(204, 255, 204);
//    protected final static Color ignoreSelectionColor = Color.red;  // for debugging

	protected final static Color highlightColor = Color.yellow;

	protected final static Color circularLinkColor = Color.gray;

	protected final static Color clickBorderColor = Color.black;

	protected final static Color tabBorderColor = Color.blue;

	protected static LineBorder tabBorder = new LineBorder(tabBorderColor);

	protected LinkRenderSpec linkSpec = new LinkRenderSpec();

	protected static HashMap fontHash = new HashMap();

	protected static HashMap strokeHash = new HashMap();

	protected java.util.List defaultSpecs = new ArrayList();

	protected MultiIcon multiIcon = new MultiIcon();

	protected Icon nec_inv_icon;

	protected Icon nec_icon;

	protected Icon inv_icon;

	protected Icon completes_icon;

	protected JComponent renderComponent = this;

	protected boolean DONT_SHOW_DISJOINT_FROM = true;

	protected void createDefaultSpecs() {
	}

	public OBOCellRenderer() {
		super();
//		createDefaultSpecs();
		nec_inv_icon = Preferences.loadLibraryIcon("inv_and_nec_icon.gif");
		inv_icon = Preferences.loadLibraryIcon("inv_icon.gif");
		nec_icon = Preferences.loadLibraryIcon("nec_icon.gif");
		completes_icon = Preferences.loadLibraryIcon("completes.gif");

		// Use preferred font for text
		setFont(Preferences.getPreferences().getFont());
//		logger.info("OBOCellRenderer.setFont: " + Preferences.getPreferences().getFont());  // DEL
		setIcon(multiIcon);
	}

	@Override
	public boolean isShowing() {
		return true;
	}

	@Override
	public Dimension getSize() {
		Dimension d = super.getSize();
		d.height += 4;
		return d;
	}

	protected ArrowIcon linkIcon = new ArrowIcon();
	protected ScaledIcon scaledIcon = new ScaledIcon(null);

	public Component getListCellRendererComponent(JList list, Object value,
			int index, boolean isSelected, boolean cellHasFocus) {
		if (isSelected) {
			setOpaque(true);
//			logger.info("getListCellRendererComponent: choosing darker selection color"); // DEL
			setBackground(Preferences.defaultSelectionColor());
		} else {
			setOpaque(false);
			setBackground(null);
		}
		setBorder(null);

		IdentifiedObject term = (IdentifiedObject) value;
		String text = term.toString();

		if (TermUtil.isObsolete(term))
			setForeground(Color.red);
		else
			setForeground(Color.black);

		setText(text);

		return this;
	}

	protected Font getFont(ObjectRenderSpec spec) {
		Font font = Preferences.getPreferences().getFont();
		String fontName = font.getFontName();
		int size = font.getSize();
		int style = Font.PLAIN;
		if (spec.getBold() && spec.getItalic())
			style = Font.BOLD | Font.ITALIC;
		else if (spec.getBold())
			style = Font.BOLD;
		else if (spec.getItalic())
			style = Font.ITALIC;
		if (spec.getFontName() != null)
			fontName = spec.getFontName();
		if (spec.getFontSize() > 0)
			size = spec.getFontSize();

		String hashval = fontName + "-" + size + "-" + style;

		font = (Font) fontHash.get(hashval);
		if (font == null) {
			font = new Font(fontName, style, size);
			fontHash.put(hashval, font);
		}
		return font;
	}

	public Component getTreeCellRendererComponent(JTree tree, Object value,
			boolean selected, boolean expanded, boolean leaf, int row,
			boolean hasFocus) {
		boolean highlighted = false;
		boolean clickTarget = false;
		boolean tabRow = false;
		boolean ignoreSelection = false;

		if (tree instanceof OBOTermPanel) {
			OBOTermPanel ot = (OBOTermPanel) tree;
			highlighted = row == ot.getHighlightRow();
			clickTarget = row == ot.getClickTarget();
			tabRow = row == ot.getTabRow();
			ignoreSelection = ot.ignoreSelection();
		}
		return getTreeCellRendererComponent(tree, value, selected, expanded,
				leaf, highlighted, clickTarget, tabRow, ignoreSelection, row,
				hasFocus);
	}

	public Component getTreeCellRendererComponent(JTree tree, Object value,
			boolean selected, boolean expanded, boolean leaf,
			boolean highlighted, boolean clickTarget, boolean tabRow,
			boolean ignoreSelection, int row, boolean hasFocus) {
		try {
//			setOpaque(false);
			multiIcon.clearIcons();
			if (value.equals(PathUtil.OBSOLETE)) {
				setText("Obsolete");
				setForeground(Color.red);
				setBorder(null);
				setOpaque(false);
				setBackground(null);
				return this;
			} else if (value.equals(PathUtil.TYPES)) {
				setText("Relations");
				setForeground(Color.blue);
				setBorder(null);
				setOpaque(false);
				setBackground(null);
				return this;
			} else if (value.equals(PathUtil.INSTANCES)) {
				setText("Instances");
				setForeground(Color.green);
				setBorder(null);
				setOpaque(false);
				setBackground(null);
				return this;
			} else if (value.equals(PathUtil.CLASSES)) {
				setText("Classes");
				setForeground(Color.black);
				setBorder(null);
				setOpaque(false);
				setBackground(null);
				return this;
			} else
				setForeground(Color.black);

			multiIcon.addIcon(linkIcon);
			multiIcon.addIcon(scaledIcon);
			linkIcon.setColor(Color.black);
			linkIcon.setLineWidth(1);
			linkIcon.setLineType(LineType.SOLID_LINE);
			if (!(value instanceof Relationship)) {
				setText("Some unknown item " + value);
				return this;
			}

			Relationship link = (Relationship) value;

			if (link.getType() != null) {
				Color c = Preferences.getPreferences()
						.getColorForRelationshipType(link.getType());
				if (c != null)
					linkIcon.setColor(c);
			}

			RenderSpec spec;
			if (tree instanceof FilteredRenderable) {
				FilteredRenderable fr = (FilteredRenderable) tree;
				NodeLabelProvider provider = fr.getNodeLabelProvider();
				LinkedObject lo = link.getChild();
				String s = provider.getLabel(fr, lo);
				setText(s);
				spec = GUIUtil.getSpec(fr, link, FilterManager.getManager()
						.getGlobalLinkRenderers(), fr.getLinkRenderers());
			} else {
				spec = GUIUtil.getSpec(null, link, FilterManager.getManager()
						.getGlobalLinkRenderers());
				setText(link.getChild().getName());
			}
			if (spec instanceof GeneralRendererSpec) {
				GeneralRendererSpec s = (GeneralRendererSpec) spec;
				ColorProvider f = s.getValue(ForegroundColorSpecField.FIELD);
				if (f != null) {
					FilteredRenderable fr = null;
					if (tree instanceof FilteredRenderable)
						fr = (FilteredRenderable) tree;
					linkIcon.setColor(f.getColor(fr, link));
				}
				Integer width = s.getValue(LineWidthSpecField.FIELD);
				if (width != null)
					linkIcon.setLineWidth(width.intValue());
				LineType type = s.getValue(LineTypeSpecField.FIELD);
				if (type != null) {
					linkIcon.setLineType(type);
				}

			}

			linkIcon.setLeaf(leaf);
			linkIcon.setPath(tree.getPathForRow(row));

			Icon icon = null;

			if (link.getType() != null) {
				// if (link.getType().equals(OBOProperty.IS_A))
				// icon = new SVGIcon("file:/Users/jrichter/drawing.svg",
				// Preferences.getPreferences().getFont().getSize());
				// else
				icon = Preferences.getPreferences().getIconForRelationshipType(
					link.getType());
			}
			scaledIcon.setIcon(icon);
			Icon origIcon = icon;
			setIcon(null);
			// Don't scale TextIcons
			if (origIcon instanceof TextIcon) {
				setIcon(multiIcon);
			}
			else {
				int newHeight = (int) getPreferredSize().getHeight() - 2;
				setIcon(multiIcon);
				scaledIcon.setDimension(newHeight);
			}

			if (link instanceof OBORestriction) {
				OBORestriction tr = (OBORestriction) link;
				if (!tr.isNecessarilyTrue() && tr.isInverseNecessarilyTrue())
					multiIcon.addIcon(nec_inv_icon);
				else if (!tr.isNecessarilyTrue()) {
					multiIcon.addIcon(nec_icon);
				} else if (tr.isInverseNecessarilyTrue()) {
					multiIcon.addIcon(inv_icon);
				}

				if (tr.completes()) {
					multiIcon.addIcon(completes_icon);
				}
			}

			// This attempt to disable "circular" relation nodes* was making them show up without their arrows.
			// *For example, in SO, genome is a child of disjoint_from chromosome_variation, which is a child of genome.
			// Instead, color the "circular" nodes gray to discourage users from clicking on them.
 			TreePath path = tree.getPathForRow(row);
  			if (path != null && PathUtil.pathIsCircular(path)) {
// // 				setEnabled(false);  // Doesn't work--doesn't disable the +/- icon for opening/closing the node; just makes the link icon not show up and things go strange.
 				setForeground(circularLinkColor);
  			}
// 			else  // don't need--default is enabled anyway
// 				setEnabled(true);

			if (highlighted) {
				setOpaque(true);
				setBackground(highlightColor);
			} else if (selected) {
				setOpaque(true);
				if (ignoreSelection) {
//				    logger.info("Ignoring selection for link = " + link.getType()); // DEL
					setBackground(ignoreSelectionColor);
				}
				else {
					Selection selection = SelectionManager.getManager()
							.getGlobalSelection();

					// Note on this if:
					// As it was, with the ||, you could end up with two dark-shaded terms
					// (even though only one was the true subselection).
					// Without the ||, you sometimes end up with no dark-shaded term (this
					// happens if you deselect the current subselection), even though there
					// is still a subselection.
					// Both behaviors are slightly wrong; I decided the second was less wrong.
					// I'm not sure how to fix it completely.
					if (// link.getChild().equals(selection.getTermSubSelection()) ||
					     ObjectUtil.equals(link, selection
								 .getLinkSubSelection())) {
					    // Use darker color (this is the subselection)
					    setBackground(Preferences.defaultSelectionColor());
					} else {
					    // Use lighter color (this is NOT the subselection)
					    setBackground(Preferences.lightSelectionColor());
					}
				}
			} else {
				setBackground(null);

				if (tree instanceof FilteredRenderable) {
					FilteredRenderable fr = (FilteredRenderable) tree;
					RenderSpec bgspec = GUIUtil
							.getSpec(fr, link.getChild(), FilterManager
									.getManager().getGlobalTermRenderers(), fr
									.getObjectRenderers(), fr
									.getAutomaticObjectRenderers());
					if (bgspec instanceof GeneralRendererSpec) {
						ColorProvider c = ((GeneralRendererSpec) bgspec)
								.getValue(BackgroundColorSpecField.FIELD);
						if (c != null) {
							setOpaque(true);
							setBackground(c.getColor(fr, link.getChild()));
//							logger.info("Set background color to " + getBackground()); // DEL
						}
					}
				}
			}
			if (tabRow) {
				setBorder(tabBorder);
			} else {
				setBorder(null);
			}

		} catch (Throwable t) {
		    logger.info("getTreeCellRendererComponent: caught error.  Stack trace:");
			t.printStackTrace();
		}
		validate();
		return this;
	}
}
