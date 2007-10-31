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

	public GeneralSpecEditor() {
		double[] rowSizes = new double[FilterManager.getManager()
				.getRenderSpecFields().size()];
		Arrays.fill(rowSizes, TableLayout.PREFERRED);
		double[][] sizes = { { TableLayout.PREFERRED, TableLayout.FILL },
				rowSizes };
		setLayout(new TableLayout(sizes));

		int row = 0;
		for (final GeneralRendererSpecField<?> field : FilterManager
				.getManager().getRenderSpecFields()) {
			final JCheckBox selectBox = new JCheckBox(field.getName());
			final JPanel holderPanel = new JPanel();
			holderPanel.setLayout(new GridLayout(1,1));
			final GeneralRendererSpecFieldEditor<?> editor = field.getEditor();
			add(selectBox, "0, "+row);
			add(holderPanel, "1, "+row);
			row++;
			checkboxMap.put(field, selectBox);
			panelMap.put(field, holderPanel);
			editorMap.put(field, editor);
			selectBox.addActionListener(new ActionListener() {

				public void actionPerformed(ActionEvent e) {
					if (!selectBox.isSelected())
						holderPanel.removeAll();
					else {
						editor.setValue(null);
						holderPanel.add((JComponent) editor);
					}
					validate();
					repaint();
				}
			});
		}
	}

	public RenderSpec getSpec() {
		GeneralRendererSpec spec = new GeneralRendererSpec();
		for (final GeneralRendererSpecField field : FilterManager.getManager()
				.getRenderSpecFields()) {
			if (checkboxMap.get(field).isSelected())
				spec.setValue(field, editorMap.get(field).getValue());
		}
		return spec;
	}

	public void setSpec(RenderSpec s) {
		if (s instanceof GeneralRendererSpec) {
			GeneralRendererSpec spec = (GeneralRendererSpec) s;
			for (JCheckBox box : checkboxMap.values()) {
				box.setSelected(false);
			}
			for (final GeneralRendererSpecField field : spec.getFields()) {
				checkboxMap.get(field).setSelected(true);
				GeneralRendererSpecFieldEditor editor = editorMap.get(field);
				editor.setValue(spec.getValue(field));
			}
		}
	}

}
