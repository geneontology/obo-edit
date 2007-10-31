package org.oboedit.verify;

import java.awt.*;
import java.awt.event.*;
import java.util.*;
import javax.swing.*;

import org.obo.datamodel.*;
import org.obo.filters.*;
import org.oboedit.gui.*;

public class UserFilterCheck extends AbstractCheck implements ObjectCheck,
		UserCheck {

	public static class UserFilterConfiguration extends CheckConfiguration {
		protected Filter filter;
		protected boolean link;

		protected String message = "";

		protected boolean isFatal = false;

		protected String desc = "<new filter>";

		public void setDescription(String desc) {
			this.desc = desc;
		}

		public String getDescription() {
			return desc;
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

		public Filter getFilter() {
			return filter;
		}

		public void setFilter(Filter filter) {
			this.filter = filter;
		}

		public boolean isLink() {
			return link;
		}

		public void setLink(boolean link) {
			this.link = link;
		}
	}

	protected class ConfigurationPanel extends JPanel implements ActionListener {

		/**
		 * 
		 */
		private static final long serialVersionUID = 1L;

		protected FilterComponent editor;

		protected JTextField messageField = new JTextField("contains errors");

		protected JTextField descField = new JTextField(30);

		protected JCheckBox fatalCheckBox = new JCheckBox("Is fatal");

		public ConfigurationPanel() {
			JLabel messageLabel = new JLabel(
					"Message suffix (message will begin with \"Term <term name> \")");
			JLabel descriptionLabel = new JLabel("Check name");

			fatalCheckBox.setOpaque(false);

			Box southPanel = new Box(BoxLayout.Y_AXIS);
			southPanel.add(messageLabel);
			southPanel.add(messageField);
			southPanel.add(fatalCheckBox);

			setLayout(new BorderLayout());
			add(southPanel, "South");
			validate();
			repaint();
		}

		public void setIsLink(boolean link) {
			remove(editor);
			if (link)
				editor = new FilterComponent(new LinkFilterEditorFactory());
			else
				editor = new FilterComponent(new TermFilterEditorFactory());
			add(editor, "Center");
		}

		public void actionPerformed(ActionEvent e) {
			updateConfiguration();
		}

		protected void updateConfiguration() {
			((UserFilterConfiguration) configuration).setFilter(editor
					.getFilter());
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

	public Filter getFilter() {
		return ((UserFilterConfiguration) configuration).getFilter();
	}

	public String getMessage() {
		return ((UserFilterConfiguration) configuration).getMessage();
	}

	public boolean isFatal() {
		return ((UserFilterConfiguration) configuration).isFatal();
	}

	public boolean isLink() {
		return ((UserFilterConfiguration) configuration).isLink();
	}

	public Collection check(OBOSession session, IdentifiedObject currentObject,
			byte condition, boolean checkObsoletes) {
		Collection out = new LinkedList();
		if (isLink()) {
			if (currentObject instanceof LinkedObject) {
				Iterator it = ((LinkedObject) currentObject).getParents()
						.iterator();
				while (it.hasNext()) {
					Link link = (Link) it.next();
					if (getFilter().satisfies(link)) {
						out
								.add(new CheckWarning("Link " + link + " "
										+ getMessage(), isFatal(), this,
										currentObject));
						break;
					}
				}
			}
		} else {
			if (getFilter().satisfies(currentObject))
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
