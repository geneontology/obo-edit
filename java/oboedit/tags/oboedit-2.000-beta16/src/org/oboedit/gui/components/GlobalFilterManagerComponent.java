package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.border.TitledBorder;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.swing.HTMLTableRenderer;
import org.bbop.swing.tablelist.AbstractListTableEditor;
import org.bbop.swing.widget.TableList;
import org.bbop.util.CollectionUtil;
import org.obo.filters.Filter;
import org.obo.filters.LinkRenderSpec;
import org.obo.filters.ObjectRenderSpec;
import org.obo.filters.RenderSpec;
import org.obo.filters.RenderedFilter;
import org.obo.filters.SpecEditor;
import org.obo.util.FilterUtil;
import org.oboedit.controller.FilterManager;
import org.oboedit.gui.FilterComponent;
import org.oboedit.gui.FilteredRenderable;
import org.oboedit.gui.LinkFilterEditorFactory;
import org.oboedit.gui.TermFilterEditorFactory;
import org.oboedit.gui.widget.LinkSpecEditor;
import org.oboedit.gui.widget.ObjectSpecEditor;

public class GlobalFilterManagerComponent extends AbstractGUIComponent {

	protected JButton saveButton = new JButton("Save Changes");
	protected JButton revertButton = new JButton("Revert");
	protected JTabbedPane tabbedPane = new JTabbedPane();

	protected static class RendererRenderer extends HTMLTableRenderer {
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
							+ fr.getSpec().toString()
							+ "</b> for links that match<br><i>"
							+ FilterUtil.getOBOFilterExpression(fr.getFilter())
							+ "</i></html>";
				else
					return "<html>Display terms lines as <b>"
					+ fr.getSpec().toString()
					+ "</b> for terms that match<br><i>"
					+ FilterUtil.getOBOFilterExpression(fr.getFilter())
					+ "</i></html>";
			} else
				return "<html>" + value.toString() + "</value>";
		}

	}

	protected FilterComponent termFilterComponent;
	protected FilterComponent linkFilterComponent;
	protected TableList termRendererList;
	protected TableList linkRendererList;

	protected class RendererEditor extends
			AbstractListTableEditor<RenderedFilter> {

		protected boolean isLinkRenderer;
		protected FilterComponent filterComponent;
		protected SpecEditor rendererEditor;

		public RendererEditor(boolean isLinkRenderer) {
			setLayout(new BorderLayout());
			this.isLinkRenderer = isLinkRenderer;
			filterComponent = new FilterComponent(
					isLinkRenderer ? new LinkFilterEditorFactory()
							: new TermFilterEditorFactory());
			filterComponent.setButtonVisible(false);
			if (isLinkRenderer)
				rendererEditor = new LinkSpecEditor();
			else
				rendererEditor = new ObjectSpecEditor();
			add(filterComponent, "Center");
			add((Component) rendererEditor, "East");
		}

		public RenderedFilter createNewValue() {
			return new RenderedFilter(null,
					isLinkRenderer ? new LinkRenderSpec()
							: new ObjectRenderSpec());
		}

		public RenderedFilter getValue() {
			RenderedFilter filter = new RenderedFilter(filterComponent
					.getFilter(), rendererEditor.getSpec());
			return filter;
		}

		public void setValue(RenderedFilter value) {
			filterComponent.setFilter(value.getFilter());
			rendererEditor.setSpec(value.getSpec());
		}
	}

	public GlobalFilterManagerComponent(String id) {
		super(id);
		termFilterComponent = new FilterComponent(new TermFilterEditorFactory());
		termFilterComponent.setButtonVisible(false);
		linkFilterComponent = new FilterComponent(new LinkFilterEditorFactory());
		linkFilterComponent.setButtonVisible(false);
		termRendererList = new TableList();
		termRendererList.setRenderer(new RendererRenderer(false));
		termRendererList.setEditor(new RendererEditor(false));
		linkRendererList = new TableList();
		linkRendererList.setRenderer(new RendererRenderer(true));
		linkRendererList.setEditor(new RendererEditor(true));

		setLayout(new BorderLayout());
		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new BorderLayout());
		buttonPanel.add(saveButton, "Center");
		buttonPanel.add(revertButton, "West");

		revertButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				loadFilters();
			}
		});
		saveButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				commit();
			}
		});

		tabbedPane.addTab("Term filters", termFilterComponent);
		tabbedPane.addTab("Link filters", linkFilterComponent);
		tabbedPane.addTab("Term renderers", termRendererList);
		tabbedPane.addTab("Link renderers", linkRendererList);

		add(tabbedPane, "Center");
		add(buttonPanel, "South");
	}

	public void commit() {
		FilterManager.getManager().setGlobalFilters(
				termFilterComponent.getFilter(),
				linkFilterComponent.getFilter());
		FilterManager.getManager().setGlobalTermRenderers(
				termRendererList.getData());
		FilterManager.getManager().setGlobalLinkRenderers(
				linkRendererList.getData());
	}

	@Override
	public void init() {
		super.init();
		loadFilters();
	}
	
	@Override
	public void cleanup() {
		commit();
		super.cleanup();
	}

	protected void loadFilters() {
		termFilterComponent.setFilter(FilterManager.getManager()
				.getGlobalTermFilter());
		linkFilterComponent.setFilter(FilterManager.getManager()
				.getGlobalLinkFilter());
		
		termRendererList.setData(CollectionUtil.deepCopy(FilterManager
				.getManager().getGlobalTermRenderers()));
		linkRendererList.setData(CollectionUtil.deepCopy(FilterManager
				.getManager().getGlobalLinkRenderers()));
	}

}
