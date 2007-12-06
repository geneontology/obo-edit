package org.bbop.framework;

import java.awt.Component;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.JMenuItem;

import org.bbop.swing.AbstractDynamicMenuItem;
import org.bbop.swing.DynamicMenu;
import org.bbop.util.CollectionUtil;

public class HelpMenu extends DynamicMenu {

	public HelpMenu() {
		super("Help");
		AbstractDynamicMenuItem guideItem = new AbstractDynamicMenuItem(
				"User guide options", true, false, false) {

			public List<? extends Component> getItems() {
				JMenuItem helpItem = new JMenuItem("User Guide");
				helpItem.addActionListener(new ActionListener() {

					public void actionPerformed(ActionEvent e) {
						HelpManager.getManager().displayHelp();
					}
				});
				return CollectionUtil.list(helpItem);
			}

		};
		add(guideItem);
	}
}
