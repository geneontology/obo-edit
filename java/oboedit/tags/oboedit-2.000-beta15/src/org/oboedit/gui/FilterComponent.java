package org.oboedit.gui;

import java.awt.BorderLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.LinkedList;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.obo.filters.Filter;
import org.obo.filters.RenderSpec;
import org.obo.filters.SpecEditor;
import org.obo.util.FilterUtil;
import org.oboedit.controller.SelectionManager;
import org.oboedit.gui.event.GUIUpdateEvent;
import org.oboedit.gui.event.GUIUpdateListener;
import org.oboedit.gui.event.SelectionEvent;
import org.oboedit.gui.event.SelectionListener;

public class FilterComponent extends JPanel {

	protected SearchComponentFactory factory;
	protected SelectionListener listener = new SelectionListener() {

		public void selectionChanged(SelectionEvent e) {
			contentPanel.updateMatchLabels();
		}

	};
	protected SearchPanel contentPanel;
	protected JComponent rendererEditor;
	
	protected Collection<ActionListener> listeners = new LinkedList<ActionListener>();
	protected JButton searchButton = new JButton("Search");
	protected boolean showRendererControls = false;
	
	public void showRendererControls(boolean showRendererControls) {
		this.showRendererControls = showRendererControls;
		updateRendererControls();
	}
	
	protected void updateRendererControls() {
		if (showRendererControls)
			remove(rendererEditor);
		else
			add(rendererEditor, "West");
		validate();
		repaint();
	}
	
	public boolean rendererControlsShowing() {
		return showRendererControls;
	}

	public FilterComponent(SearchComponentFactory factory) {
		this.factory = factory;
		rendererEditor = factory.getSpecEditor();
		initGUI();
		installListeners();
	}

	public void setShowButton(boolean visible) {
		searchButton.setVisible(visible);
		validate();
	}

	public void cleanup() {
		uninstallListeners();
	}

	public Filter<?> getFilter() {
		return contentPanel.getFilter();
	}

	public void setFilter(Filter<?> filter) {
		contentPanel.setFilter(filter);
	}
	
	public void setRenderSpec(RenderSpec spec) {
		if (showRendererControls)
			factory.setRenderSpec(rendererEditor, spec);
	}
	
	public RenderSpec getRenderSpec() {
		if (showRendererControls)
			return factory.getRenderSpec(rendererEditor);
		else
			return null;
	}

	protected void installListeners() {
		SelectionManager.getManager().addSelectionListener(listener);
	}

	protected void uninstallListeners() {
		SelectionManager.getManager().removeSelectionListener(listener);
	}

	public void addActionListener(ActionListener listener) {
		listeners.add(listener);
	}

	public void removeActionListener(ActionListener listener) {
		listeners.remove(listener);
	}

	protected void fireActionEvent(ActionEvent e) {
		for (ActionListener listener : listeners) {
			listener.actionPerformed(e);
		}
	}

	public void setButtonLabel(String label) {
		searchButton.setText(label);
		validate();
	}

	public void setButtonVisible(boolean visible) {
		searchButton.setVisible(visible);
		validate();
	}

	protected void initGUI() {
		setLayout(new BorderLayout());
		final JLabel statusLabel = new JLabel();
		contentPanel = new SearchPanel(factory);
		contentPanel.addUpdateListener(new GUIUpdateListener() {

			public void guiupdated(GUIUpdateEvent e) {
				String s = FilterUtil.getOBOFilterExpression(contentPanel
						.getFilter());
				statusLabel.setText(s);
			}

		});
		SelectionManager.getManager().addSelectionListener(listener);
		ActionListener actionListener = new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				fireActionEvent(e);
			}
		}; 
		searchButton.addActionListener(actionListener);
		contentPanel.addActionListener(actionListener);
		JButton newButton = new JButton(Preferences.loadLibraryIcon("file.gif"));
		newButton.setToolTipText("Create new filter");
		JButton loadButton = new JButton(Preferences
				.loadLibraryIcon("folder.gif"));
		newButton.setToolTipText("Load a filter from disk");
		JButton saveButton = new JButton(Preferences
				.loadLibraryIcon("floppy.gif"));
		newButton.setToolTipText("Save this filter to disk");
		loadButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				contentPanel.load();
			}
		});
		saveButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				contentPanel.save();
			}
		});

		newButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				contentPanel.clear();
			}
		});

		JPanel buttonPanel = new JPanel();
		buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.X_AXIS));
		buttonPanel.add(newButton);
		buttonPanel.add(loadButton);
		buttonPanel.add(saveButton);

		JPanel panel = new JPanel();
		panel.setLayout(new BorderLayout());
		panel.add(buttonPanel, "West");
		panel.add(searchButton, "Center");
		contentPanel.setActionComponent(panel);
		contentPanel.setStatusComponent(statusLabel);
		contentPanel.setName("Outer search panel");
		add(contentPanel, "Center");
	}
}
