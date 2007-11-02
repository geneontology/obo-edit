/**
 * 
 */
package org.oboedit.gui.components;

import java.awt.GridLayout;

import javax.swing.JComponent;
import javax.swing.JSplitPane;

import org.bbop.swing.tablelist.AbstractListTableEditor;
import org.obo.filters.LinkFilterImpl;
import org.obo.filters.ObjectFilterImpl;
import org.oboedit.gui.FilterComponent;
import org.oboedit.gui.LinkFilterEditorFactory;
import org.oboedit.gui.TermFilterEditorFactory;
import org.oboedit.gui.filter.GeneralSpecEditor;
import org.oboedit.gui.filter.LinkRenderSpec;
import org.oboedit.gui.filter.ObjectRenderSpec;
import org.oboedit.gui.filter.RenderedFilter;
import org.oboedit.gui.filter.SpecEditor;
import org.oboedit.gui.widget.LinkSpecEditor;
import org.oboedit.gui.widget.ObjectSpecEditor;

public class RendererEditor extends AbstractListTableEditor<RenderedFilter> {

	protected boolean isLinkRenderer;
	protected FilterComponent filterComponent;
	protected SpecEditor rendererEditor;

	public RendererEditor(boolean isLinkRenderer) {
		this.isLinkRenderer = isLinkRenderer;
		filterComponent = new FilterComponent(
				isLinkRenderer ? new LinkFilterEditorFactory()
						: new TermFilterEditorFactory());
		filterComponent.setButtonVisible(false);
		/*
		if (isLinkRenderer)
			rendererEditor = new LinkSpecEditor();
		else
			rendererEditor = new ObjectSpecEditor();
			*/
		rendererEditor = new GeneralSpecEditor(isLinkRenderer);
		JSplitPane pane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
				filterComponent,
				(JComponent) rendererEditor);
		setLayout(new GridLayout(1,1));
		add(pane);
	}

	public RenderedFilter createNewValue() {
		return new RenderedFilter((isLinkRenderer ? new LinkFilterImpl()
				: new ObjectFilterImpl()),
				isLinkRenderer ? new LinkRenderSpec() : new ObjectRenderSpec());
	}

	public RenderedFilter getValue() {
		RenderedFilter filter = new RenderedFilter(filterComponent.getFilter(),
				rendererEditor.getSpec());
		return filter;
	}

	public void setValue(RenderedFilter value) {
		filterComponent.setFilter(value.getFilter());
		rendererEditor.setSpec(value.getSpec());
	}
}