package org.oboedit.gui.components.graphvizViewer;

import java.awt.BorderLayout;

import javax.swing.JColorChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import org.bbop.framework.ConfigurationPanel;

public class GraphvizConfigurationPanelNew  extends ConfigurationPanel {

		protected JPanel backgroundColorPanel = new JPanel();
		protected JColorChooser backgroundColorChooser = new JColorChooser();
		protected GraphvizCanvas graphvizCanvasInstance;
		
		public GraphvizConfigurationPanelNew(GraphvizCanvas graphvizCanvas) {
			backgroundColorPanel.add(backgroundColorChooser);
			this.graphvizCanvasInstance = graphvizCanvas;
			
		}

		@Override
		public void commit() {
			graphvizCanvasInstance.setBackground(backgroundColorChooser.getColor());
		}

		@Override
		public void init() {
		backgroundColorChooser.setColor(graphvizCanvasInstance.getBackground());	
		}

	}

