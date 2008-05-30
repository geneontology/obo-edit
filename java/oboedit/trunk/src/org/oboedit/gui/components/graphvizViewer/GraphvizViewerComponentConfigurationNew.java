package org.oboedit.gui.components.graphvizViewer;

import java.awt.Color;

import org.bbop.framework.ComponentConfiguration;

public class GraphvizViewerComponentConfigurationNew 
	 implements ComponentConfiguration {
		protected Color backgroundColor;
		
		public GraphvizViewerComponentConfigurationNew() {
		}

		public Color getBackgroundColor() {
			return backgroundColor;
		}

		public void setBackgroundColor(Color backgroundColor) {
			this.backgroundColor = backgroundColor;
		}

		public GraphvizViewerComponentConfigurationNew(Color bColor) {
			this.backgroundColor = bColor;
		}
	}

