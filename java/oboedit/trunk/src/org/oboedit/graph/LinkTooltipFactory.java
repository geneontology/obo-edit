package org.oboedit.graph;

import org.obo.datamodel.Link;
import org.obo.util.ExplanationUtil;
import org.obo.util.HTMLUtil;
import org.oboedit.gui.components.LinkDatabaseCanvas;
import org.oboedit.piccolo.ViewRenderedStyleText;

import edu.umd.cs.piccolo.PNode;

public class LinkTooltipFactory extends AbstractTooltipFactory {

	ViewRenderedStyleText text = new ViewRenderedStyleText();

	public PNode getTooltip(LinkDatabaseCanvas canvas, PNode node) {
		if (node instanceof OELink && canvas.getReasoner() != null) {
			Link link = ((OELink) node).getLink();
			if (link != null) {
				String html = "<html>\n" + "<body>\n";

				html += ExplanationUtil.getDescriptionReasoned(canvas
						.getReasoner(), canvas.getLinkProviderDatabase(),
						link, false);
				html += "</body></html>";
				// PiccoloUtil.setHTML(text,
				// HTMLUtil.removeHyperlinks(html));
				text.setWidth(canvas.getWidth() * .6);
				text.setText(HTMLUtil.removeHyperlinks(html), true);
				// text.recomputeLayout();
				return text;
			}
		}
		return null;
	}

}
