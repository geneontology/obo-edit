package org.oboedit.gui.filter;

import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JPanel;

import org.oboedit.controller.FilterManager;

import info.clearthought.layout.TableLayout;

public class GeneralSpecEditor extends JPanel implements SpecEditor {

	protected Map<GeneralRendererSpecField<?>, JCheckBox> checkboxMap = new HashMap<GeneralRendererSpecField<?>, JCheckBox>();
	protected Map<GeneralRendererSpecField<?>, JComponent> panelMap = new HashMap<GeneralRendererSpecField<?>, JComponent>();
	protected Map<GeneralRendererSpecField<?>, GeneralRendererSpecFieldEditor<?>> editorMap = new HashMap<GeneralRendererSpecField<?>, GeneralRendererSpecFieldEditor<?>>();

	public GeneralSpecEditor(boolean links) {
		double[] rowSizes = new double[FilterManager.getManager()
				.getRenderSpecFields().size()];
		Arrays.fill(rowSizes, TableLayout.PREFERRED);
		double[][] sizes = { { TableLayout.PREFERRED, TableLayout.FILL },
				rowSizes };
		setLayout(new TableLayout(sizes));

		int row = 0;
		for (final GeneralRendererSpecField<?> field : FilterManager
				.getManager().getRenderSpecFields()) {
			if ((field.isLinkRenderer() && links)
					|| (field.isObjectRenderer() && !links)) {

				final JCheckBox selectBox = new JCheckBox(field.getName());
				JPanel holderPanel = new JPanel();
				holderPanel.setLayout(new GridLayout(1, 1));
				final GeneralRendererSpecFieldEditor<?> editor = field
						.getEditor();
				add(selectBox, "0, " + row);
				add(holderPanel, "1, " + row);
				row++;
				checkboxMap.put(field, selectBox);
				panelMap.put(field, holderPanel);
				editorMap.put(field, editor);
				selectBox.addActionListener(new ActionListener() {

					public void actionPerformed(ActionEvent e) {
						setVisible(field, selectBox.isSelected());
					}
				});
			}
		}
	}
	
	protected void setVisible(GeneralRendererSpecField<?> field, boolean visible) {
		JCheckBox checkBox = checkboxMap.get(field);
		checkBox.setSelected(visible);
		GeneralRendererSpecFieldEditor<?> editor = editorMap.get(field);
		JComponent holderPanel = panelMap.get(field);
		if (!visible)
			holderPanel.removeAll();
		else {
			editor.setValue(null);
			holderPanel.add((JComponent) editor);
		}
		validate();
		repaint();
	}

	public RenderSpec getSpec() {
		GeneralRendererSpec spec = new GeneralRendererSpec();
		for (final GeneralRendererSpecField field : FilterManager.getManager()
				.getRenderSpecFields()) {
			if (checkboxMap.containsKey(field) && checkboxMap.get(field).isSelected())
				spec.setValue(field, editorMap.get(field).getValue());
		}
		return spec;
	}

	public void setSpec(RenderSpec s) {
		if (s instanceof GeneralRendererSpec) {
			GeneralRendererSpec spec = (GeneralRendererSpec) s;
			for (GeneralRendererSpecField field : checkboxMap.keySet()) {
				setVisible(field, false);
			}
			for (final GeneralRendererSpecField field : spec.getFields()) {
				setVisible(field, true);
				GeneralRendererSpecFieldEditor editor = editorMap.get(field);
				editor.setValue(spec.getValue(field));
			}
		}
	}

}
