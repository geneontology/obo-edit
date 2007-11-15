/**
 * 
 */
package org.oboedit.gui.components;

import javax.swing.JTable;

import org.bbop.swing.HTMLTableRenderer;
import org.obo.util.FilterUtil;
import org.obo.util.HTMLUtil;
import org.oboedit.gui.filter.RenderedFilter;

public class RendererRenderer extends HTMLTableRenderer {
	protected boolean isLink;

	public RendererRenderer(boolean isLink) {
		this.isLink = isLink;
	}

	public String getHTML(JTable table, Object value, boolean isSelected,
			boolean hasFocus, int row, int column) {
		if (value instanceof RenderedFilter) {
			RenderedFilter fr = (RenderedFilter) value;
			if (isLink)
				return "<html>Display link lines as <b>"
						+ HTMLUtil.escapeHTML(fr.getSpec().toString())
						+ "</b> for links that match<br><i>"
						+ FilterUtil.getOBOFilterExpression(fr.getFilter())
						+ "</i></html>";
			else
				return "<html>Display terms lines as <b>"
						+ HTMLUtil.escapeHTML(fr.getSpec().toString())
						+ "</b> for terms that match<br><i>"
						+ FilterUtil.getOBOFilterExpression(fr.getFilter())
						+ "</i></html>";
		} else
			return "<html>" + value + "</value>";
	}

}