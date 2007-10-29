package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.awt.GridLayout;

import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.ConfigurationPanel;

public class ConfigurableTextComponent extends AbstractGUIComponent {

	public static class InfoConfig implements ComponentConfiguration {
		protected String html;

		public InfoConfig() {
		}

		public InfoConfig(String html) {
			this.html = html;
		}

		public String getHtml() {
			return html;
		}

		public void setHtml(String html) {
			this.html = html;
		}
	}

	public class CTConfigPanel extends ConfigurationPanel {

		protected JTextArea htmlArea = new JTextArea();

		public CTConfigPanel() {
			htmlArea.setWrapStyleWord(true);
			htmlArea.setLineWrap(true);
			setLayout(new BorderLayout());
			add(new JLabel("HTML or plain text message"), BorderLayout.NORTH);
			add(new JScrollPane(htmlArea, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
					JScrollPane.HORIZONTAL_SCROLLBAR_NEVER), BorderLayout.CENTER);
		}

		@Override
		public void commit() {
			setHTML(htmlArea.getText());
		}

		@Override
		public void init() {
			htmlArea.setText(getHTML());
		}

	}

	protected JLabel label = new JLabel();
	protected String html = "<html><font size='+1'>Press the configuration button to "
			+ "enter your own text. Your text will be saved when the application is "
			+ "restarted.</font></html>";

	public ConfigurableTextComponent(String id) {
		super(id);
		setHTML(html);
		setLayout(new GridLayout(1, 1));
		add(label);
	}

	@Override
	public ConfigurationPanel getConfigurationPanel() {
		return new CTConfigPanel();

	}
	
	public String getHTML() {
		return html;
	}

	public void setHTML(String html) {
		this.html = html;
		label.setText(html);
	}

	@Override
	public ComponentConfiguration getConfiguration() {
		return new InfoConfig(html);
	}

	public void setConfiguration(ComponentConfiguration config) {
		if (config instanceof InfoConfig) {
			InfoConfig ic = (InfoConfig) config;
			setHTML(ic.getHtml());
		}
	}
}
