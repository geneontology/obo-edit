package org.bbop.swing;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.Label;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.border.EmptyBorder;

import org.bbop.util.TaskDelegate;

import org.apache.log4j.*;

public class ScreenLockRunnable extends AbstractPeriodicUpdateRunnable {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ScreenLockRunnable.class);

	protected JButton cancelButton = new JButton("Cancel");
	protected JProgressBar progressBar = new JProgressBar();
	protected JLabel messageLabel = new JLabel();
	protected JDialog dialog;
	protected Frame frame;
	protected boolean modal;
	protected boolean cancelled;
	protected Dimension lastSize;

	public ScreenLockRunnable(BackgroundEventQueue queue, Frame frame,
			boolean modal) {
		super(queue, true);
		this.frame = frame;
		this.modal = modal;
		cancelButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				cancelled = true;
			}

		});
	}

	@Override
	protected void setupUpdate() {
		if (dialog == null)
			dialog = createDialog();
		cancelled = false;
		progressBar.setString("0 %");
		progressBar.setIndeterminate(true);
		progressBar.setValue(0);
		messageLabel.setText("");
		//dialog.pack();
		dialog.setSize(400,200);
		SwingUtil.center(frame, dialog);
		dialog.toFront();
		dialog.setVisible(true);  // ?
	}

	protected JDialog createDialog() {
		JDialog dialog = new JDialog(frame, "Progress...", modal);
		JPanel contentPane = new JPanel();
		contentPane.setBorder(new EmptyBorder(20, 20, 20, 20));
		contentPane.setLayout(new BorderLayout());
		JPanel southPanel = new JPanel();
		southPanel.setLayout(new BorderLayout());
		southPanel.add(progressBar, "Center");
		southPanel.add(cancelButton, "South");
		contentPane.add(messageLabel, "Center");
		contentPane.add(southPanel, "South");
		dialog.setContentPane(contentPane);
		return dialog;
	}

	@Override
	protected void cleanupUpdate() {
		if (dialog != null) {
		    dialog.setVisible(false);  // not good enough!
		    dialog.dispose();  // We're done!  Make it go away!
		}
	}

	@Override
	protected boolean isCancelled() {
		return cancelled;
	}

    // For some reason, this method seems to take more time than you'd think.
	@Override
	protected void doUpdate(TaskDelegate<?> currentTask) {
		if (lastSize == null)
			lastSize = dialog.getContentPane().getSize();
		Number n = currentTask.getProgressValue();
		if (n != null) {
			int val = n.intValue();
			if (val < 0) {
				progressBar.setIndeterminate(true);
				progressBar.setStringPainted(false);
			} else {
				progressBar.setValue(val);
				progressBar.setIndeterminate(false);
				progressBar.setString(val + " %");
				progressBar.setStringPainted(true);
			}
		}
		String s = currentTask.getProgressString();
		if (s != null)
			messageLabel.setText(s);
		if (!dialog.isVisible()) {
//		    System.out.println("doUpdate: dialog was not visible");  // DEL
		    // Not needed
//  			dialog.setVisible(true);
//  			dialog
//  					.setSize(
//  							(int) (lastSize.getWidth()
//  									+ dialog.getInsets().left + dialog
//  									.getInsets().right), (int) (lastSize
//  									.getHeight()
//  									+ dialog.getInsets().bottom + dialog
//								.getInsets().top));
		} else {
			Dimension newSize = dialog.getContentPane().getPreferredSize();
			if (newSize.width > lastSize.width
					|| newSize.height > lastSize.height)
				dialog
						.setSize(
								(int) (newSize.getWidth()
										+ dialog.getInsets().left + dialog
										.getInsets().right), (int) (newSize
										.getHeight()
										+ dialog.getInsets().bottom + dialog
										.getInsets().top));
		}
		lastSize = dialog.getContentPane().getSize();
//		SwingUtil.center(frame, dialog);
	}
}
