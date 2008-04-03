package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextArea;
import javax.swing.border.TitledBorder;

import org.bbop.framework.ConfigurationPanel;
import org.bbop.swing.widget.TableList;
import org.oboedit.gui.FilterComponent;
import org.oboedit.gui.LinkFilterEditorFactory;
import org.oboedit.gui.TermFilterEditorFactory;

public class OntologyEditorConfigEditor extends ConfigurationPanel {

	protected FilterComponent termFilterComponent;
	protected FilterComponent linkFilterComponent;
	protected TableList termRendererList;
	protected TableList linkRendererList;
	protected JTabbedPane configTabbedPane = new JTabbedPane();
	protected JComboBox showToolbarBox = new JComboBox();
	protected JCheckBox linkFilterButton = new JCheckBox("Filter links");
	protected JCheckBox termFilterButton = new JCheckBox("Filter terms");
	protected JTextArea htmlArea = new JTextArea();
	protected JPanel termFilterPanel = new JPanel();
	protected JPanel linkFilterPanel = new JPanel();
	protected JPanel htmlExpressionPanel = new JPanel();
	protected JCheckBox revertToDefaultAction = new JCheckBox(
			"Revert to default drag gesture after each edit");

	public OntologyEditorConfigEditor() {
		setLayout(new BorderLayout());
		removeAll();
		add(configTabbedPane, "Center");
		validate();

		termRendererList = new TableList(true, true);
		termRendererList.setRenderer(new RendererRenderer(false));
		termRendererList.setEditor(new RendererEditor(false));
		linkRendererList = new TableList(true, true);
		linkRendererList.setRenderer(new RendererRenderer(true));
		linkRendererList.setEditor(new RendererEditor(true));

		showToolbarBox.addItem("Never");
		showToolbarBox.addItem("After using a hotkey");
		showToolbarBox.addItem("Always");
		showToolbarBox.setMaximumSize(
		    new Dimension(Integer.MAX_VALUE, showToolbarBox.getPreferredSize().height));


		showToolbarBox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				if (showToolbarBox.getSelectedIndex() == OntologyEditorConfiguration.SHOW_TOOLBAR_ON_HOTKEY) {
					revertToDefaultAction.setSelected(true);
					revertToDefaultAction.setEnabled(false);
				} else
					revertToDefaultAction.setEnabled(true);
			}
		});

		termFilterPanel.setLayout(new BorderLayout());
		termFilterPanel.add(termFilterButton, "North");

		linkFilterPanel.setLayout(new BorderLayout());
		linkFilterPanel.add(linkFilterButton, "North");

		termFilterComponent = new FilterComponent(new TermFilterEditorFactory());
		termFilterComponent.setBorder(new TitledBorder("Term Filter"));
		linkFilterComponent = new FilterComponent(new LinkFilterEditorFactory());
		linkFilterComponent.setBorder(new TitledBorder("Link Filter"));

		termFilterComponent.setButtonVisible(false);
		linkFilterComponent.setButtonVisible(false);

		termFilterComponent.setShowButton(false);

		termFilterButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				updateConfigFilterPanels();
			}
		});
		linkFilterButton.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				updateConfigFilterPanels();
			}
		});

		JScrollPane pane = new JScrollPane(htmlArea,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		htmlExpressionPanel.setLayout(new BorderLayout());
		htmlExpressionPanel
				.add(
						new JLabel(
								"<html>Enter an expression below that will "
								+ "determine what information is shown for each term. "
								+ "To include specific term information, use search "
								+ "criteria ids enclosed in $ characters. For example, "
								+ "to display the term name on one line and the term "
								+ "id in italics on the line below, use the expression "
								+ "<b>&lt;center&gt;$name$&lt;br&gt;&lt;i&gt;&lt;font "
								+ "size=-1&gt;$id$&lt;/font&gt;&lt;/i&gt;&lt;/center&gt;</b></html>"),
						BorderLayout.NORTH);
		htmlExpressionPanel.add(pane, BorderLayout.CENTER);

		JComponent guiConfigPanel = createGUIConfigPanel();
		guiConfigPanel.add(Box.createVerticalGlue());
		configTabbedPane.add(guiConfigPanel, "GUI Settings");
		configTabbedPane.add(termFilterPanel, "Term filtering");
		configTabbedPane.add(linkFilterPanel, "Link filtering");
		configTabbedPane.add(htmlExpressionPanel, "HTML Expression");
		configTabbedPane.addTab("Term renderers", termRendererList);
		configTabbedPane.addTab("Link renderers", linkRendererList);
	}

	protected JComponent createGUIConfigPanel() {
		JPanel guiConfigPanel = new JPanel();
		guiConfigPanel
				.setLayout(new BoxLayout(guiConfigPanel, BoxLayout.Y_AXIS));
		Box toolbarPanel = new Box(BoxLayout.X_AXIS);
		toolbarPanel.add(new JLabel("Show toolbar"));
		toolbarPanel.add(showToolbarBox);
		guiConfigPanel.add(toolbarPanel);
		guiConfigPanel.add(revertToDefaultAction);
		return guiConfigPanel;
	}

	@Override
	public void commit() {
		OntologyEditorConfiguration config = (OntologyEditorConfiguration) getComponent()
				.getConfiguration();
		commitConfig(config);
		getComponent().setConfiguration(config);
	}

	protected void commitConfig(OntologyEditorConfiguration config) {
		if (termFilterButton.isSelected()) {
			config.setTermFilter(termFilterComponent.getFilter());
		} else
			config.setTermFilter(null);

		if (linkFilterButton.isSelected()) {
			config.setLinkFilter(linkFilterComponent.getFilter());
		} else
			config.setLinkFilter(null);
		config.setObjectRenderers(termRendererList.getData());
		config.setLinkRenderers(linkRendererList.getData());
		config.setShowToolbar(showToolbarBox.getSelectedIndex());
		config.setRevertToDefaultAction(revertToDefaultAction.isSelected());
		config.setBasicHTML(htmlArea.getText());
	}

	@Override
	public void init() {
		OntologyEditorConfiguration config = (OntologyEditorConfiguration) getComponent()
				.getConfiguration();
		initConfig(config);
		updateConfigFilterPanels();
	}

	protected void initConfig(OntologyEditorConfiguration config) {
		linkFilterButton.setSelected(config.getLinkFilter() != null);
		termFilterButton.setSelected(config.getTermFilter() != null);
		if (config.getLinkFilter() != null)
			linkFilterComponent.setFilter(config.getLinkFilter());
		if (config.getTermFilter() != null)
			termFilterComponent.setFilter(config.getTermFilter());

		showToolbarBox.setSelectedIndex(config.getShowToolbar());
		revertToDefaultAction.setSelected(config.isRevertToDefaultAction());
		htmlArea.setText(config.getBasicHTML());
		termRendererList.setData(config.getObjectRenderers());
		linkRendererList.setData(config.getLinkRenderers());
	}

	protected void updateConfigFilterPanels() {
		if (termFilterButton.isSelected())
			termFilterPanel.add(termFilterComponent, "Center");
		else
			termFilterPanel.remove(termFilterComponent);
		if (linkFilterButton.isSelected())
			linkFilterPanel.add(linkFilterComponent, "Center");
		else
			linkFilterPanel.remove(linkFilterComponent);
		validate();
		repaint();
	}
}
