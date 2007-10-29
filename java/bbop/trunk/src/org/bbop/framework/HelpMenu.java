package org.bbop.framework;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.List;

import javax.help.CSH;
import javax.help.HelpBroker;
import javax.help.HelpSet;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;

import org.bbop.swing.AbstractDynamicMenuItem;
import org.bbop.swing.DynamicMenu;
import org.bbop.util.CollectionUtil;

public class HelpMenu extends DynamicMenu {

	protected HelpBroker helpBroker;

	protected File helpSetFile;

	public HelpMenu(File helpSetFile) {
		super("Help");
		this.helpSetFile = helpSetFile;
		helpBroker = createHelpBroker(helpSetFile);
		AbstractDynamicMenuItem guideItem = new AbstractDynamicMenuItem(
				"User guide options", true, false, false) {

			public List<? extends Component> getItems() {
				JMenuItem helpItem = new JMenuItem("User Guide");

				if (helpBroker != null) {
					helpItem.addActionListener(new CSH.DisplayHelpFromSource(
							helpBroker));
				} else {
					helpItem.addActionListener(new ActionListener() {
						public void actionPerformed(ActionEvent e) {
							JOptionPane.showMessageDialog(GUIManager
									.getManager().getFrame(),
									"No help files found!");
						}
					});
				}
				return CollectionUtil.list(helpItem);
			}

		};
		add(guideItem);
	}

	protected static HelpBroker createHelpBroker(File docsDir) {
		HelpSet hs;

		try {
			hs = new HelpSet(null, docsDir.toURL());
		} catch (Exception ee) {
			System.out.println("HelpSet " + ee.getMessage());
			System.out.println("HelpSet " + docsDir + " not found");
			return null;
		}
		return hs.createHelpBroker();
	}
}
