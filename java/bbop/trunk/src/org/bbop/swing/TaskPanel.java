package org.bbop.swing;

import java.awt.CardLayout;
import java.awt.Component;
import java.awt.LayoutManager;
import java.lang.reflect.InvocationTargetException;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;

import org.bbop.util.TaskDelegate;

public class TaskPanel extends JPanel {

	protected static final String CONTENT_KEY = "CONTENT";
	protected static final String PROGRESS_KEY = "PROGRESS";

	protected JPanel contentPanel = new JPanel();
	protected JPanel progressPanel = new JPanel();
	protected JLabel progressLabel = new JLabel();
	protected JProgressBar progressBar = new JProgressBar();
	protected BackgroundEventQueue queue = new BackgroundEventQueue();
	protected CardLayout layout = new CardLayout();

	public TaskPanel() {
		super.setLayout(layout);
		super.add(contentPanel, CONTENT_KEY);
		super.add(progressPanel, PROGRESS_KEY);
		progressBar.setStringPainted(true);
		progressPanel.setLayout(new BoxLayout(progressPanel, BoxLayout.Y_AXIS));
		progressPanel.add(Box.createVerticalGlue());
		progressPanel.add(progressLabel);
		progressPanel.add(progressBar);
		progressPanel.add(Box.createVerticalGlue());
		queue.addStartupNotifier(new ProgressBarUpdateRunnable(queue,
				progressBar) {
			@Override
			protected void cleanupUpdate() {
				setProgressVisible(false);
			}
		});
	}

	public void schedule(TaskDelegate<?> task, boolean exclusive) {
		setProgressVisible(true);
		if (exclusive)
			queue.cancelAll();
		queue.scheduleTask(task);
	}

	protected void setProgressVisible(final boolean visible) {

		if (visible) {
			progressLabel.setText("");
			progressBar.setValue(0);
			progressBar.setString("");
			layout.show(TaskPanel.this, PROGRESS_KEY);
		} else
			layout.show(TaskPanel.this, CONTENT_KEY);
		repaint();
		validate();
		System.err.println("done updating");

	}

	@Override
	public void setOpaque(boolean isOpaque) {
		super.setOpaque(isOpaque);
		if (contentPanel != null)
			contentPanel.setOpaque(isOpaque);
		if (progressPanel != null)
			progressPanel.setOpaque(isOpaque);
	}

	@Override
	public Component add(Component comp) {
		return contentPanel.add(comp);
	}

	@Override
	public Component add(Component comp, int index) {
		return contentPanel.add(comp, index);
	}

	@Override
	public void add(Component comp, Object constraints) {
		contentPanel.add(comp, constraints);
	}

	@Override
	public void add(Component comp, Object constraints, int index) {
		contentPanel.add(comp, constraints, index);
	}

	@Override
	public Component add(String name, Component comp) {
		return contentPanel.add(name, comp);
	}

	@Override
	public void setLayout(LayoutManager mgr) {
		if (contentPanel != null)
			contentPanel.setLayout(mgr);
	}

	@Override
	public void remove(Component comp) {
		contentPanel.remove(comp);
	}

	@Override
	public void remove(int index) {
		contentPanel.remove(index);
	}

	@Override
	public void removeAll() {
		contentPanel.removeAll();
	}
}
