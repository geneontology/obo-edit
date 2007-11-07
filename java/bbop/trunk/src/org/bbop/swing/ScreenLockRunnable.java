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

public class ScreenLockRunnable extends AbstractPeriodicUpdateRunnable {

	protected JButton cancelButton = new JButton("Cancel");
	protected JProgressBar progressBar = new JProgressBar();
	protected JLabel messageLabel = new JLabel();
	protected JDialog dialog;
	protected Frame frame;
	protected boolean modal;
	protected boolean cancelled;

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
		dialog.pack();
		SwingUtil.center(frame, dialog);
		dialog.toFront();
	}

	protected JDialog createDialog() {
		JDialog dialog = new JDialog(frame, "Progress...", modal);
		JPanel contentPane = new JPanel();
		contentPane.setBorder(new EmptyBorder(20,20,20,20));
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
			dialog.setVisible(false);
		}
	}

	@Override
	protected boolean isCancelled() {
		return cancelled;
	}

	@Override
	protected void doUpdate(TaskDelegate<?> currentTask) {
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
			dialog.setVisible(true);
			dialog.pack();
		} else {
			dialog.pack();
		}
		SwingUtil.center(frame, dialog);
	}
}
