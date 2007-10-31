package org.oboedit.gui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.StringTokenizer;

import org.obo.datamodel.IdentifiedObject;
import org.obo.filters.SearchCriterion;
import org.obo.util.IDUtil;
import org.oboedit.controller.FilterManager;
import org.oboedit.gui.filter.RenderedFilter;
import org.oboedit.util.GUIUtil;

public class HTMLNodeLabelProvider implements NodeLabelProvider {

	protected FilteredRenderable renderable;
	protected Collection<RenderedFilter> preRenderers = new ArrayList<RenderedFilter>();
	protected Collection<RenderedFilter> postRenderers = new ArrayList<RenderedFilter>();
	protected String htmlExpression = "$name$";

	public HTMLNodeLabelProvider(FilteredRenderable renderable,
			Collection<RenderedFilter> postRenderers) {
		this(renderable, null, postRenderers);
	}

	public HTMLNodeLabelProvider(FilteredRenderable renderable,
			Collection<RenderedFilter> preRenderers,
			Collection<RenderedFilter> postRenderers) {
		this.renderable = renderable;
		if (preRenderers != null)
			this.preRenderers.addAll(preRenderers);
		if (postRenderers != null)
			this.postRenderers.addAll(postRenderers);
	}

	public HTMLNodeLabelProvider(FilteredRenderable renderable,
			RenderedFilter... renderers) {
		this.renderable = renderable;
		postRenderers.addAll(Arrays.asList(renderers));
	}

	public static String resolveHTMLExpression(String exp, IdentifiedObject lo) {
		StringBuffer out = new StringBuffer();
		List tokens = IDUtil.parseVarString(exp);
		for (Object token : tokens) {
			if (token instanceof IDUtil.Variable) {
				IDUtil.Variable var = (IDUtil.Variable) token;
				SearchCriterion sc = FilterManager.getManager().getCriterion(
						var.getName());
				List vals = (List) sc.getValues(new ArrayList(), lo);
				int index = 0;
				String defaultVal = "";
				if (var.getParams().size() > 0) {
					defaultVal = var.getParams().get(0);
				}
				if (var.getParams().size() > 1) {
					try {
						index = Integer.parseInt(var.getParams().get(1));
					} catch (NumberFormatException ex) {
					}
				}
				if (index > vals.size())
					out.append(defaultVal);
				else {
					out.append(vals.get(index).toString());
				}
			} else
				out.append(token.toString());
		}
		return out.toString();
	}

	public String getLabel(IdentifiedObject lo) {
		return GUIUtil.renderHTML(resolveHTMLExpression(htmlExpression, lo),
				lo, preRenderers, renderable.getObjectRenderers(),
				FilterManager.getManager().getGlobalTermRenderers(),
				postRenderers);
	}

	public String getHtmlExpression() {
		return htmlExpression;
	}

	public void setHtmlExpression(String htmlExpression) {
		if (htmlExpression != null)
			this.htmlExpression = htmlExpression;
		else
			this.htmlExpression = "$name$";
	}

}
