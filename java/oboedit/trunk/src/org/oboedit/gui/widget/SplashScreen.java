package org.oboedit.gui.widget;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;

import org.bbop.swing.*;
import org.oboedit.gui.Preferences;

import java.net.URL;
import java.util.*;

public class SplashScreen extends JWindow {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private BackgroundImagePanel bip;
	private final static String splashscreen = "org/geneontology/oboedit/gui/resources/splash.gif";

	protected Thread thread;
	protected long waitTime = 5000;

	public SplashScreen() {
		super();
		bip = getSplashPanel();

		setContentPane(bip);
		bip.repaint();

		thread = new Thread(new Runnable() {
			public void run() {
				try {
					SwingUtilities.invokeAndWait(new Runnable() {
						public void run() {
							setVisible(true);
						}
					});
					thread.sleep(waitTime);
				} catch (Exception ex) {
				}
			}
		});

		addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
				kill();
			}
		});
		bip.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				kill();
			}
		});
		bip.requestFocus();
	}

	public static BackgroundImagePanel getSplashPanel() {
		JTextArea textArea = new JTextArea() {
			/**
			 * 
			 */
			private static final long serialVersionUID = -4472416435803158030L;

			@Override
			public void paint(Graphics g) {
				Graphics2D g2 = (Graphics2D) g;
				g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,
						RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
				super.paint(g);
			}
		};

		URL url = ClassLoader.getSystemResource(splashscreen);

		BackgroundImagePanel bip = new BackgroundImagePanel(url, false);
		bip.setLayout(null);

		bip.add(textArea);

		int year;

		GregorianCalendar calendar = new GregorianCalendar();
		year = calendar.get(Calendar.YEAR);
		if (year < 2006)
			year = 2006;

		textArea.setLineWrap(true);
		textArea.setBounds(23, 275, 337, 100);
		textArea.setFont(new Font("Courier", Font.BOLD, 16));
		textArea.setForeground(Color.black);
		textArea.setOpaque(false);
		textArea.setText("! An open source ontology editor\n"
				+ "! Distributed under the terms of\n"
				+ "! the Artistic License\n" + "written-by: John Day-Richter\n"
				+ "copyright:  2001-" + year + "\n" + "version:    "
				+ Preferences.getVersion());

		/*
		 * JLabel versionLabel = new JLabel("Version "+ OBOEdit.getVersion());
		 * versionLabel.setFont(new Font("Dialog", 0, 12));
		 * versionLabel.setForeground(Color.black);
		 * 
		 * bip.setLayout(new BoxLayout(bip, BoxLayout.Y_AXIS));
		 * 
		 * Box hBox = new Box(BoxLayout.X_AXIS);
		 * hBox.add(Box.createHorizontalGlue()); hBox.add(versionLabel);
		 * hBox.add(Box.createHorizontalGlue());
		 * bip.add(Box.createVerticalGlue()); bip.add(hBox);
		 * bip.add(Box.createVerticalStrut(15));
		 */

		bip.setBorder(null);
		return bip;
	}

	protected void kill() {
		setVisible(false);
		if (thread.isAlive())
			thread.interrupt();
	}

	public void start() {
		thread.start();
	}

	public void join() {
		try {
			thread.join();
		} catch (InterruptedException ex) {
		}
	}
}
