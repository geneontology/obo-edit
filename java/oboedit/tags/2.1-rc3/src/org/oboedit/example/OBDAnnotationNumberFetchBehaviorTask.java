package org.oboedit.example;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;

import org.bbop.swing.ColorUtil;
import org.obo.datamodel.IdentifiedObject;
import org.oboedit.connect.OBDConnect;
import org.oboedit.graph.AbstractFetchTask;
import org.oboedit.gui.FilteredRenderable;
import org.oboedit.gui.filter.BackgroundColorSpecField;
import org.oboedit.gui.filter.GeneralRendererSpec;
import org.oboedit.gui.filter.HTMLSpecField;
import org.oboedit.gui.filter.HeatmapColor;

import org.apache.log4j.*;

public class OBDAnnotationNumberFetchBehaviorTask extends AbstractFetchTask<Integer> {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(OBDAnnotationNumberFetchBehaviorTask.class);

	protected OBDConnect gdbc = new OBDConnect();
	
	public static class AnnotationNumberFetchTaskConfiguration extends
			FetchTaskConfiguration {
		protected Color minColor;
		protected Color maxColor;

		public AnnotationNumberFetchTaskConfiguration() {
		}

		public AnnotationNumberFetchTaskConfiguration(boolean enabled,
				Color minColor, Color maxColor) {
			setEnabled(enabled);
			setMinColor(minColor);
			setMaxColor(maxColor);
		}

		public Color getMinColor() {
			return minColor;
		}

		public void setMinColor(Color minColor) {
			this.minColor = minColor;
		}

		public Color getMaxColor() {
			return maxColor;
		}

		public void setMaxColor(Color maxColor) {
			this.maxColor = maxColor;
		}
	}

	protected Color minColor;
	protected Color maxColor;

	protected JCheckBox enabledCheckBox = new JCheckBox("Enabled", true);
	protected JButton minColorButton = new JButton();
	protected JButton maxColorButton = new JButton();

	public OBDAnnotationNumberFetchBehaviorTask() {
		super(Integer.class);
		setEnabled(false);
		gdbc.connect();
		minColorButton.setBorderPainted(false);
		maxColorButton.setBorderPainted(false);
		minColorButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				Color c = JColorChooser.showDialog(minColorButton,
						"Select min color", getMinColor());
				if (c != null)
					setMinColor(c);
			}

		});
		maxColorButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				Color c = JColorChooser.showDialog(maxColorButton,
						"Select max color", getMaxColor());
				if (c != null)
					setMaxColor(c);
			}

		});
		setMinColor(Color.white);
		setMaxColor(Color.red);
	}

	@Override
	protected String getBehaviorID() {
		return "annotation_number_fetch";
	}

	@Override
	protected Integer getValue(IdentifiedObject io) {
		int num = -1; 
		num = gdbc.getAssociationCount( io.getID() );
		//logger.info("\t___ SQL_COMPLETE");
		return num;
	}

	@Override
	protected GeneralRendererSpec getFetchedRenderer(FilteredRenderable canvas,
			String valueVar) {
		return new GeneralRendererSpec(HTMLSpecField.FIELD,
				"$term$<hr><center><font color=\"black\">count: $"
						+ valueVar + "$</font></center>",
				BackgroundColorSpecField.FIELD, new HeatmapColor(getMinColor(),
						getMaxColor(), valueVar));
	}

	@Override
	protected GeneralRendererSpec getPendingRenderer(FilteredRenderable canvas,
			String valueVar) {
		return new GeneralRendererSpec(HTMLSpecField.FIELD,
				"$term$<hr><center><font color=\"black\"><i>Loading...</i></font></center>");
	}

	@Override
	protected JComponent getConfigurationPanel() {
		JPanel configPanel = new JPanel();
		enabledCheckBox.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				setEnabled(enabledCheckBox.isSelected());
			}
		});
		configPanel.setLayout(new BoxLayout(configPanel, BoxLayout.Y_AXIS));
		configPanel.add(enabledCheckBox);
		Box minPanel = Box.createHorizontalBox();
		minPanel.add(new JLabel("Minimum value color"));
		minPanel.add(Box.createHorizontalStrut(10));
		minPanel.add(minColorButton);
		configPanel.add(minPanel);
		Box maxPanel = Box.createHorizontalBox();
		maxPanel.add(new JLabel("Maximum value color"));
		maxPanel.add(Box.createHorizontalStrut(10));
		maxPanel.add(maxColorButton);
		configPanel.add(maxPanel);
		return configPanel;
	}

	@Override
	public Object getConfiguration() {
		return new AnnotationNumberFetchTaskConfiguration(isEnabled(), getMinColor(),
				getMaxColor());
	}

	@Override
	public void setConfiguration(Object config) {
		super.setConfiguration(config);
		if (config instanceof AnnotationNumberFetchTaskConfiguration) {
			AnnotationNumberFetchTaskConfiguration myConfig = (AnnotationNumberFetchTaskConfiguration) config;
			setMinColor(myConfig.getMinColor());
			setMaxColor(myConfig.getMaxColor());
		}
	}
	
	@Override
	public void setEnabled(boolean enabled) {
		super.setEnabled(enabled);
		enabledCheckBox.setSelected(enabled);
	}


	@Override
	protected String getName() {
		return "OBD Annotation Number Renderer Plugin";
	}

	public Color getMinColor() {
		return minColor;
	}

	public void setMinColor(Color minColor) {
		this.minColor = minColor;
		minColorButton.setBackground(minColor);
		minColorButton.setText(ColorUtil.getName(minColor));
		resetRenderers();
	}

	public Color getMaxColor() {
		return maxColor;
	}

	public void setMaxColor(Color maxColor) {
		this.maxColor = maxColor;
		maxColorButton.setBackground(maxColor);
		maxColorButton.setText(ColorUtil.getName(maxColor));
		resetRenderers();
	}
}
