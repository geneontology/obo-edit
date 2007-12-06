package org.oboedit.gui.menu;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;

import javax.swing.JDialog;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.WindowConstants;

import org.bbop.framework.GUIManager;
import org.bbop.framework.HelpMenu;
import org.bbop.swing.BackgroundImagePanel;
import org.bbop.swing.SwingUtil;
import org.oboedit.gui.Preferences;
import org.oboedit.gui.widget.SplashScreen;

public class OEHelpMenu extends HelpMenu {

	public OEHelpMenu() {
		super();
		JMenuItem aboutItem = new JMenuItem("About");
		aboutItem.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				showAboutFrame();
			}
		});

		add(aboutItem);
	}

	public static void showAboutFrame() {
		final JDialog aboutDialog = new JDialog(GUIManager.getManager()
				.getFrame(), "About OBO-Edit");
		BackgroundImagePanel bip = SplashScreen.getSplashPanel();
		bip.addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				aboutDialog.dispose();
			}
		});
		bip.setMaximumSize(new Dimension(400, 400));
		bip.setMinimumSize(new Dimension(400, 400));
		bip.setPreferredSize(new Dimension(400, 400));
		aboutDialog.setResizable(false);
		aboutDialog.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
		aboutDialog.setContentPane(bip);
		aboutDialog.pack();
		SwingUtil.center(aboutDialog);
		aboutDialog.setVisible(true);
	}
}
