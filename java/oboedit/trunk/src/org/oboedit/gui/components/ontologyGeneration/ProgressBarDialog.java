package org.oboedit.gui.components.ontologyGeneration;

import java.awt.BorderLayout;
import java.awt.HeadlessException;
import java.awt.Point;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.SwingUtilities;

/**
 * JDialog is invoked very time web service is queried.
 * 
 * @author Thomas Waechter (<href>waechter@biotec.tu-dresden.de</href>), 2008
 */
public class ProgressBarDialog extends JDialog
{
	private static final long serialVersionUID = 8003810001805242066L;

	private static final String defaultWaitLabelText = "Please wait...                                                  ";

	public ProgressBarDialog(JComponent parent) throws HeadlessException
	{
		this(parent, defaultWaitLabelText);
	}

	public ProgressBarDialog(JComponent parent, String waitLabelText) throws HeadlessException
	{
		super();

		setModal(true);
		setAlwaysOnTop(true);
		setTitle("Working");
		JPanel contentPane = new JPanel(new BorderLayout(7, 7));
		setContentPane(contentPane);
		JProgressBar progressBar = new JProgressBar();
		progressBar.setIndeterminate(true);
		contentPane.setBorder(BorderFactory.createEmptyBorder(10, 10, 10, 10));
		if (waitLabelText != null) {
			contentPane.add(new JLabel(waitLabelText));
		}
		else {
			contentPane.add(new JLabel(defaultWaitLabelText));
		}
		contentPane.add(progressBar, BorderLayout.SOUTH);
		setSize(100, 100);

		// center
		Point parentAnchor = parent.getLocation();
		SwingUtilities.convertPointToScreen(parentAnchor, parent);
		int x = parentAnchor.x + (parent.getWidth() - this.getWidth()) / 2;
		int y = parentAnchor.y + (parent.getHeight() - this.getHeight()) / 2;
		setLocation(x, y);

		pack();
	}
}
