package org.oboedit.gui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.StringTokenizer;

import org.obo.datamodel.IdentifiedObject;
import org.obo.filters.SearchCriterion;
import org.obo.util.HTMLUtil;
import org.obo.util.IDUtil;
import org.oboedit.controller.FilterManager;
import org.oboedit.gui.filter.GeneralRendererSpecField;
import org.oboedit.gui.filter.RenderedFilter;
import org.oboedit.util.GUIUtil;

public class HTMLNodeLabelProvider implements NodeLabelProvider {

	protected FilteredRenderable renderable;
	protected Collection<RenderedFilter> preRenderers = new ArrayList<RenderedFilter>();
	protected Collection<RenderedFilter> postRenderers = new ArrayList<RenderedFilter>();
	protected Collection<GeneralRendererSpecField<?>> ignoreThese = new ArrayList<GeneralRendererSpecField<?>>();
	protected String htmlExpression = "$name$";

	public HTMLNodeLabelProvider() {
		this(null, (Collection) null);
	}
	
	public HTMLNodeLabelProvider(FilteredRenderable renderable,
			Collection<RenderedFilter> postRenderers) {
		this(renderable, null, null, postRenderers);
	}

	public HTMLNodeLabelProvider(FilteredRenderable renderable,
			Collection<? extends GeneralRendererSpecField<?>> ignoreThese,
			Collection<? extends RenderedFilter> preRenderers,
			Collection<? extends RenderedFilter> postRenderers) {
		this.renderable = renderable;
		if (ignoreThese != null)
			this.ignoreThese.addAll(ignoreThese);
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
				if (sc == null)
					out.append("??cannot-resolve-"+var.getName()+"??");
				List vals = (List) sc.getValues(new ArrayList(), lo);
				int index = 0;
				String defaultVal = "";
				String repeatHTML = "$item$<br>\n";
				if (var.getParams().size() > 0) {
					defaultVal = var.getParams().get(0);
				}
				if (var.getParams().size() > 1) {
					if (var.getParams().get(1).equals("*")) {
						index = -1;
						if (var.getParams().size() > 2) {
							repeatHTML = var.getParams().get(2);
						}
					} else {
						try {
							index = Integer.parseInt(var.getParams().get(1));
						} catch (NumberFormatException ex) {
						}
					}
				}
				if (vals.size() == 0 || index > vals.size())
					out.append(defaultVal);
				else if (index == -1) {
					for (Object val : vals) {
						out
								.append(repeatHTML.replace("$item$", HTMLUtil.escapeHTML(val
										.toString())));
					}
				} else {
					out.append(HTMLUtil.escapeHTML(vals.get(index).toString()));
				}
			} else
				out.append(token.toString());
		}
		return out.toString();
	}

	public String getLabel(ObjectSelector selector, IdentifiedObject lo) {
		FilteredRenderable fr = null;
		if (selector instanceof FilteredRenderable)
			fr = (FilteredRenderable) selector;
		if (renderable != null)
			return GUIUtil.renderHTML(fr, resolveHTMLExpression(htmlExpression, lo),
				lo, ignoreThese, preRenderers, renderable.getObjectRenderers(),
				FilterManager.getManager().getGlobalTermRenderers(),
				renderable.getAutomaticObjectRenderers(),
				postRenderers);
		else
			return GUIUtil.renderHTML(fr, resolveHTMLExpression(htmlExpression, lo),
					lo, ignoreThese, preRenderers,
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
