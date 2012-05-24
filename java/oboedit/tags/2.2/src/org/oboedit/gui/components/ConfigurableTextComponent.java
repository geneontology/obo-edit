package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.awt.GridLayout;

import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.framework.ComponentConfiguration;
import org.bbop.framework.ConfigurationPanel;

import org.apache.log4j.*;

public class ConfigurableTextComponent extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ConfigurableTextComponent.class);

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
	protected String html = "<html><font size='+1'>To "
	+ "enter your own text, click the little wrench icon in the toolbar above. " +
	"You may use html markup in your text. " + 
	"When you're ready, click the check icon in the toolbar. Your text will be saved and displayed when OBO-Edit is "
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
