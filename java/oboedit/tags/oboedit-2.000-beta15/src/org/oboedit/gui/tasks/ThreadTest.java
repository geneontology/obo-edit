package org.oboedit.gui.tasks;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;

public class ThreadTest {

	public static void main(String[] args) {
		final Thread myThread = new Thread() {
			@Override
			public void run() {
				while (true) {
					long time = System.currentTimeMillis();
					try {
						sleep(5000);
					} catch (InterruptedException ex) {
						System.err.println(" Interrupted!");
					}
					System.err.println("woke after "+(System.currentTimeMillis() - time));
				}
			}
		};
		myThread.start();
		JDialog d = new JDialog();
		JButton button = new JButton("Click");
		button.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				myThread.interrupt();
			}
			
		});
		d.getContentPane().add(button);
		d.pack();
		d.setVisible(true);
	}
}
