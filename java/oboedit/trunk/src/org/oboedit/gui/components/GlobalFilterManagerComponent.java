package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.border.TitledBorder;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.swing.widget.TableList;
import org.bbop.util.CollectionUtil;
import org.obo.filters.Filter;
import org.oboedit.controller.FilterManager;
import org.oboedit.gui.FilterComponent;
import org.oboedit.gui.FilteredRenderable;
import org.oboedit.gui.LinkFilterEditorFactory;
import org.oboedit.gui.TermFilterEditorFactory;
import org.oboedit.gui.filter.RenderSpec;

import org.apache.log4j.*;

public class GlobalFilterManagerComponent extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(GlobalFilterManagerComponent.class);

	protected JButton saveButton = new JButton("Save Changes");
	protected JButton revertButton = new JButton("Revert");
	protected JTabbedPane tabbedPane = new JTabbedPane();

	protected FilterComponent termFilterComponent;
	protected FilterComponent linkFilterComponent;
	protected TableList termRendererList;
	protected TableList linkRendererList;

	public GlobalFilterManagerComponent(String id) {
		super(id);
		termFilterComponent = new FilterComponent(new TermFilterEditorFactory());
		termFilterComponent.setButtonVisible(false);
		linkFilterComponent = new FilterComponent(new LinkFilterEditorFactory());
		linkFilterComponent.setButtonVisible(false);
		termRendererList = new TableList(true, true);
		termRendererList.setRenderer(new RendererRenderer(false));
		termRendererList.setEditor(new RendererEditor(false));
		linkRendererList = new TableList(true, true);
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
		logger.debug("GlobalFilterManagerComponent.commit: new link filter = " + linkFilterComponent.getFilter() + 
		", new term filter = " + termFilterComponent.getFilter()); // DEL
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
