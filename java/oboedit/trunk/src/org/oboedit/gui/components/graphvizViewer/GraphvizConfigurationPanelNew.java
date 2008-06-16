package org.oboedit.gui.components.graphvizViewer;

import java.awt.BorderLayout;
import java.awt.Container;

import javax.swing.JColorChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextArea;

import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.ConfigurationPanel;
import org.bbop.framework.GUIComponent;

import com.sun.org.apache.bcel.internal.generic.RETURN;

//public class GraphvizConfigurationPanelNew  extends ConfigurationPanel {

//protected JPanel backgroundColorPanel = new JPanel();
//protected JColorChooser backgroundColorChooser = new JColorChooser();

public class GraphvizConfigurationPanelNew extends ConfigurationPanel {

	protected GraphvizCanvas graphvizCanvasInstance;

	private static final long serialVersionUID = 1L;

	private JTextArea htmlArea = new JTextArea();
//	private JColorChooser backgroundColorChooser = new JColorChooser();
//	private JPanel backgroundColorPanel = new JPanel();

	public GraphvizConfigurationPanelNew(GraphvizCanvas graphvizCanvasInstance) {
		htmlArea.setWrapStyleWord(true);
		htmlArea.setLineWrap(true);
		setLayout(new BorderLayout());
		add(new JLabel("HTML or plain text message"), BorderLayout.NORTH);

		//backgroundColorPanel.add(backgroundColorChooser);
		this.graphvizCanvasInstance = graphvizCanvasInstance;

	}

	@Override
	public void commit() {
		// TODO Auto-generated method stub
		System.out.println("GraphvizConfigurationPanelNew: init() run.");
		
	}

	@Override
	public void init() {
		// TODO Auto-generated method stub
		System.out.println("GraphvizConfigurationPanelNew: init() run.");
		htmlArea.setLineWrap(false);
//		backgroundColorChooser.setColor(graphvizCanvasInstance.getBackground());	

	}


	@Override
	public GUIComponent getComponent() {
		System.out.println("Config panel New : getComponent.");
		return graphvizCanvasInstance;
	}	
	
	@Override
	public void setComponent(GUIComponent comp) {
		if (comp instanceof GraphvizCanvas) {
			graphvizCanvasInstance = (GraphvizCanvas)comp;
			System.out.println("Config panel New : setComponent.");
		}
	}
}

