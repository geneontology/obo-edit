package org.oboedit.gui;

import org.bbop.swing.*;
import org.bbop.util.ObjectUtil;
import org.obo.datamodel.*;
import org.obo.filters.*;
import org.obo.util.TermUtil;
import org.oboedit.controller.FilterManager;
import org.oboedit.controller.SelectionManager;
import org.oboedit.gui.components.OBOTermPanel;
import org.oboedit.util.PathUtil;

import javax.swing.tree.*;
import javax.swing.border.*;
import javax.swing.*;
import java.awt.*;
import java.awt.geom.*;
import java.net.*;
import java.util.*;

public class OBOCellRenderer extends JLabel implements TreeCellRenderer,
		ListCellRenderer, LineRenderer {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	protected final static Color ignoreSelectionColor = new Color(204, 255, 204);

	protected final static Color highlightColor = Color.yellow;

	protected final static Color clickBorderColor = Color.black;

	protected final static Color tabBorderColor = Color.blue;

	protected static LineBorder clickBorder = new LineBorder(clickBorderColor);

	protected static LineBorder tabBorder = new LineBorder(tabBorderColor);

	protected ObjectRenderSpec objectSpec = new ObjectRenderSpec();

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

	protected void createDefaultSpecs() {
		/*
		 * defaultSpecs.add(createPropertyRenderer());
		 * defaultSpecs.add(createRedundantRenderer());
		 * defaultSpecs.add(createObsoleteRenderer());
		 * defaultSpecs.add(createImpliedRenderer());
		 */
	}

	protected void mergeRenderers(RenderSpec targetSpec, Object o,
			Collection<RenderedFilter> specs) {
		Iterator<RenderedFilter> it = specs.iterator();
		while (it.hasNext()) {
			RenderedFilter pair = it.next();
			if (pair.getFilter().satisfies(o))
				targetSpec.merge(pair.getSpec());
		}
	}

	public OBOCellRenderer() {
		super();
		createDefaultSpecs();
		nec_inv_icon = Preferences.loadLibraryIcon("inv_and_nec_icon.gif");
		inv_icon = Preferences.loadLibraryIcon("inv_icon.gif");
		nec_icon = Preferences.loadLibraryIcon("nec_icon.gif");
		completes_icon = Preferences.loadLibraryIcon("completes.gif");

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

	/*
	 * public Color getLineColor(JTree tree, Object value, int row) { if (value
	 * instanceof OBORestriction && ((OBORestriction) value).isImplied()) return
	 * Color.cyan; else return null; }
	 */
	protected final int[] triangleXBuffer = new int[3];

	protected final int[] triangleYBuffer = new int[3];

	protected final int triangleYSize = 4;

	protected final int triangleXSize = 4;

	protected final int triangleOffset = 2;

	protected final boolean arrowheadLeft = true;

	protected final int controlWidth = 8;

	public void paintLine(Graphics g, Component c, int y, int left, int right,
			boolean isLeaf, TreePath path) {
		try {
			Object value = path.getLastPathComponent();
			Color lineColor = Color.black;
			int lineWidth = 1;
			int lineStyle = LinkRenderSpec.SOLID_LINE;

			if (value instanceof Link) {
				Link link = (Link) path.getLastPathComponent();
				linkSpec.clear();
				// mergeRenderers(linkSpec, link, defaultSpecs);
				mergeRenderers(linkSpec, link, FilterManager.getManager()
						.getGlobalLinkRenderers());

				if (c instanceof FilteredRenderable)
					mergeRenderers(linkSpec, link, ((FilteredRenderable) c)
							.getLinkRenderers());

				if (linkSpec.getLinkColor() != null)
					lineColor = linkSpec.getLinkColor();
				if (linkSpec.getLineWidth() != -1)
					lineWidth = linkSpec.getLineWidth();
				if (linkSpec.getLineType() != -1)
					lineStyle = linkSpec.getLineType();
			}
			String hashVal = lineWidth + "-" + lineStyle;
			Stroke stroke = (Stroke) strokeHash.get(hashVal);
			if (stroke == null) {
				float[] dash_pattern = null;

				if (lineStyle == LinkRenderSpec.DASHED_LINE) {
					dash_pattern = new float[2];
					dash_pattern[0] = lineWidth;
					dash_pattern[1] = lineWidth;
				}
				stroke = new BasicStroke(lineWidth, BasicStroke.CAP_BUTT,
						BasicStroke.JOIN_BEVEL, 1.0f, dash_pattern, 0);
				strokeHash.put(hashVal, stroke);
			}

			int xoffset = triangleOffset;
			if (!isLeaf) {
				xoffset += 2 + controlWidth / 2;
			}

			boolean drawArrow = path.getPathCount() > 3;

			int modifiedLeft;

			if (drawArrow)
				modifiedLeft = left + xoffset + triangleXSize - 1;
			else
				modifiedLeft = left;

			Stroke oldStroke = ((Graphics2D) g).getStroke();

			g.setColor(lineColor);
			((Graphics2D) g).setStroke(stroke);
			if (lineStyle == LinkRenderSpec.WAVY_LINE) {
				int length = right - modifiedLeft;
				CubicCurve2D curve = new CubicCurve2D.Float(modifiedLeft, y,
						modifiedLeft, y - triangleYSize / 2, modifiedLeft
								+ length / 2, y - triangleYSize / 2,
						modifiedLeft + length / 2, y);
				((Graphics2D) g).draw(curve);
				curve = new CubicCurve2D.Float(modifiedLeft + length / 2, y,
						modifiedLeft + length / 2, y + triangleYSize / 2,
						right, y + triangleYSize / 2, right, y);
				((Graphics2D) g).draw(curve);
			} else
				g.drawLine(modifiedLeft, y, right, y);

			((Graphics2D) g).setStroke(oldStroke);

			if (drawArrow) {
				triangleYBuffer[0] = y - triangleYSize;
				triangleYBuffer[1] = y + triangleYSize;
				triangleYBuffer[2] = y;

				if (arrowheadLeft) {
					triangleXBuffer[0] = left + xoffset + triangleXSize;
					triangleXBuffer[1] = left + xoffset + triangleXSize;
					triangleXBuffer[2] = left + xoffset;
				} else {
					triangleXBuffer[0] = right - xoffset - triangleXSize;
					triangleXBuffer[1] = right - xoffset - triangleXSize;
					triangleXBuffer[2] = right - xoffset;
				}
				g.fillPolygon(triangleXBuffer, triangleYBuffer, 3);
			}
		} catch (Throwable t) {
			t.printStackTrace();
		}
	}

	public Component getListCellRendererComponent(JList list, Object value,
			int index, boolean isSelected, boolean cellHasFocus) {
		if (isSelected) {
			setOpaque(true);
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
			multiIcon.clearIcons();
			if (value.equals(TermModel.OBSOLETE)) {
				setText("Obsolete");
				setForeground(Color.red);
				setBorder(null);
				setOpaque(false);
				setBackground(null);
				return this;
			} else if (value.equals(TermModel.TYPES)) {
				setText("Relations");
				setForeground(Color.blue);
				setBorder(null);
				setOpaque(false);
				setBackground(null);
				return this;
			} else if (value.equals(TermModel.INSTANCES)) {
				setText("Instances");
				setForeground(Color.green);
				setBorder(null);
				setOpaque(false);
				setBackground(null);
				return this;
			} else if (value.equals(TermModel.CLASSES)) {
				setText("Classes");
				setForeground(Color.black);
				setBorder(null);
				setOpaque(false);
				setBackground(null);
				return this;
			}

			if (!(value instanceof Relationship)) {
				setText("Some unknown item " + value);
				return this;
			}

			objectSpec.clear();

			Relationship link = (Relationship) value;
			String text = link.getChild().getName();

			Icon icon = null;

			if (link.getType() != null) {
				icon = Preferences.getPreferences().getIconForRelationshipType(
						link.getType());
			}

			if (icon != null) {
				multiIcon.addIcon(icon);
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

			TreePath path = tree.getPathForRow(row);
			if (path != null && PathUtil.pathIsCircular(path))
				setEnabled(false);
			else
				setEnabled(true);

			if (highlighted) {
				setOpaque(true);
				setBackground(highlightColor);
			} else if (selected) {
				setOpaque(true);
				if (ignoreSelection)
					setBackground(ignoreSelectionColor);
				else {
					Selection selection = SelectionManager.getManager()
							.getGlobalSelection();
					if (link.getChild().equals(selection.getTermSubSelection())
							|| ObjectUtil.equals(link, selection
									.getLinkSubSelection())) {
						setBackground(Preferences.defaultSelectionColor());
					} else {
						setBackground(Preferences.lightSelectionColor());
					}
				}
			} else {
				setOpaque(false);
				setBackground(null);
			}
			if (clickTarget) {
				setBorder(clickBorder);
			} else if (tabRow) {
				setBorder(tabBorder);
			} else {
				setBorder(null);
			}

			// mergeRenderers(objectSpec, link.getChild(), defaultSpecs);
			mergeRenderers(objectSpec, link.getChild(), FilterManager
					.getManager().getGlobalTermRenderers());

			if (tree instanceof FilteredRenderable) {
				mergeRenderers(objectSpec, link.getChild(),
						((FilteredRenderable) tree).getObjectRenderers());
			}

			Font font = getFont(objectSpec);
			setFont(font);

			Color color = Color.black;

			if (objectSpec.getForegroundColor() != null)
				color = objectSpec.getForegroundColor();
			setForeground(color);

			if (objectSpec.getUnderlined())
				setText("<html><u>" + text + "</u></html>");
			else
				setText(text);
		} catch (Throwable t) {
			t.printStackTrace();
		}
		return this;
	}
}
