package org.bbop.framework;

import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Toolkit;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import org.bbop.swing.EnhancedMenuBar;

public class MainFrame extends JFrame {
	private EnhancedMenuBar menubar;
	
	protected boolean confirmOnExit = true;

	protected JPanel mainPanel;
	
	private int[][] dimensionInfo = { { 620, 460, 160 }, { 760, 560, 300 },
			{ 960, 700, 400 } };
	
	private class WindowCloser extends WindowAdapter {
		@Override
		public void windowClosing(WindowEvent e) {
			if (!isConfirmOnExit()
					|| JOptionPane.showConfirmDialog(MainFrame.this,
							"Really quit?", "Exit?", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION)
				GUIManager.exit(0);

		}
	}

	public MainFrame(String title) {
		super(title);

		setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
		addWindowListener(new WindowCloser());

		try {

			menubar = new EnhancedMenuBar();
			setJMenuBar(menubar);
			addMenus();
			createListeners();
			initGUI();
			// setXMLLayout(Preferences.getPreferences().getLayout());
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Override
	public void setVisible(boolean b) {
		setupFrame();
		super.setVisible(b);
	}

	protected void setupFrame() {
		GUIComponent c = DockPanelFactory.getInstance().createComponent(
				DockPanelFactory.getInstance().getDefaultID());
		c.init();
		mainPanel.add((Component) c);
	}
	
	public void addMenu(JMenu menu) {
		menubar.add(menu);
	}
	
	public void setHelpMenu(JMenu menu) {
		menubar.add(menu);
	}
	
	protected void addMenus() {
		
	}
	
	protected void createListeners() {
	}
	
	protected void initGUI() {
		mainPanel = new JPanel();
		mainPanel.setLayout(new GridLayout(1, 1));
		setContentPane(mainPanel);

		int i;
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		for (i = 1; i < dimensionInfo.length; i++) {
			if (screenSize.width < dimensionInfo[i][0]
					|| screenSize.height < dimensionInfo[i][1])
				break;

		}
		setSize(dimensionInfo[i - 1][0], dimensionInfo[i - 1][1]);
	}

	public boolean isConfirmOnExit() {
		return confirmOnExit;
	}

	public void setConfirmOnExit(boolean confirmOnExit) {
		this.confirmOnExit = confirmOnExit;
	}
}
