package org.oboedit.verify;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;

import org.obo.datamodel.*;
import org.obo.filters.*;
import org.oboedit.gui.*;
import org.oboedit.gui.widget.FilterPairEditor;

public class UserFilterCheck extends AbstractCheck implements ObjectCheck,
		UserCheck {

	public static class UserFilterConfiguration extends CheckConfiguration {
		protected FilterPair filter = new FilterPairImpl();

		protected String message = "";

		protected boolean isFatal = false;

		protected String desc = "<new filter>";

		public void setDescription(String desc) {
			this.desc = desc;
		}

		public String getDescription() {
			return desc;
		}

		public void setFilter(FilterPair filter) {
			this.filter = filter;
		}

		public FilterPair getFilter() {
			return filter;
		}

		public void setMessage(String message) {
			this.message = message;
		}

		public String getMessage() {
			return message;
		}

		public boolean isFatal() {
			return isFatal;
		}

		public void setIsFatal(boolean isFatal) {
			this.isFatal = isFatal;
		}
	}

	protected class ConfigurationPanel extends JPanel implements ActionListener {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		protected FilterPairEditor filterPairEditor = new FilterPairEditor();

		protected JTextField messageField = new JTextField("contains errors");

		protected JTextField descField = new JTextField(30);

		protected JCheckBox fatalCheckBox = new JCheckBox("Is fatal");

		protected JButton newButton = new JButton(Preferences
				.loadLibraryIcon("file.gif"));
		protected JButton loadButton = new JButton(Preferences
				.loadLibraryIcon("folder.gif"));
		protected JButton saveButton = new JButton(Preferences
				.loadLibraryIcon("floppy.gif"));
		
		public ConfigurationPanel() {
			JLabel messageLabel = new JLabel(
					"Message suffix (message will begin with \"Term <term name> \")");
			JLabel descriptionLabel = new JLabel("Check name");

			Box northPanel = new Box(BoxLayout.X_AXIS);
			northPanel.add(descriptionLabel);
			northPanel.add(Box.createHorizontalStrut(10));
			northPanel.add(descField);
			northPanel.add(Box.createHorizontalStrut(10));
			northPanel.add(filterPairEditor.getLoadSaveButtonPanel());

			fatalCheckBox.setOpaque(false);

			Box southPanel = new Box(BoxLayout.Y_AXIS);
			southPanel.add(messageLabel);
			southPanel.add(messageField);
			southPanel.add(fatalCheckBox);
			
			filterPairEditor.setRendererOptionAllowed(false);
			filterPairEditor.setKeywordOptionAllowed(false);

			setLayout(new BorderLayout());
			add(northPanel, "North");
			add(filterPairEditor, "Center");
			add(southPanel, "South");
			validate();
			repaint();
		}

		public void actionPerformed(ActionEvent e) {
			updateConfiguration();
		}

		protected void updateConfiguration() {
			((UserFilterConfiguration) configuration)
					.setFilter(filterPairEditor.getFilterPair());
			((UserFilterConfiguration) configuration).setDescription(descField
					.getText());
			((UserFilterConfiguration) configuration).setMessage(messageField
					.getText());
			((UserFilterConfiguration) configuration).setIsFatal(fatalCheckBox
					.isSelected());
		}
	}

	protected ConfigurationPanel configurationPanel = new ConfigurationPanel();

	public UserFilterCheck() {
	}

	@Override
	protected CheckConfiguration createConfiguration() {
		return new UserFilterConfiguration();
	}

	@Override
	public String getDescription() {
		return ((UserFilterConfiguration) configuration).getDescription();
	}

	public FilterPair getFilter() {
		return ((UserFilterConfiguration) configuration).getFilter();
	}

	public String getMessage() {
		return ((UserFilterConfiguration) configuration).getMessage();
	}

	public boolean isFatal() {
		return ((UserFilterConfiguration) configuration).isFatal();
	}

	public Collection check(OBOSession session, IdentifiedObject currentObject,
			byte condition, boolean checkObsoletes) {
		Collection out = new LinkedList();
		if (getFilter().getLinkFilter() != null) {
			if (currentObject instanceof LinkedObject) {
				Iterator it = ((LinkedObject) currentObject).getParents()
						.iterator();
				while (it.hasNext()) {
					Link link = (Link) it.next();
					if (getFilter().getLinkFilter().satisfies(link)) {
						out
								.add(new CheckWarning("Link " + link + " "
										+ getMessage(), isFatal(), this,
										currentObject));
						break;
					}
				}
			}
		}

		if (getFilter().getObjectFilter() != null) {
			if (getFilter().getObjectFilter().satisfies(currentObject))
				out.add(new CheckWarning("Term " + currentObject.getName()
						+ " (" + currentObject.getID() + ") " + getMessage(),
						isFatal(), this, currentObject));
		}
		return out;
	}

	@Override
	public JComponent getConfigurationPanel() {
		return configurationPanel;
	}

	public String getID() {
		return "USER_FILTER_CHECK";
	}
}
