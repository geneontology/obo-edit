package org.oboedit.gui.components;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.Collection;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.plaf.basic.BasicLabelUI;
import javax.swing.text.AttributeSet;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.Position;
import javax.swing.text.View;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLDocument;

import org.bbop.framework.AbstractGUIComponent;
import org.bbop.swing.HyperlinkLabel;
import org.bbop.swing.StringLinkListener;
import org.obo.datamodel.IdentifiedObject;
import org.obo.identifier.DefaultLinkIDResolution;
import org.obo.identifier.IDResolution;
import org.obo.identifier.IDWarning;
import org.obo.identifier.LinkIDResolution;
import org.obo.identifier.LinkIDWarning;
import org.obo.identifier.UnresolvedIDsException;
import org.obo.util.HTMLUtil;
import org.obo.util.IDUtil;
import org.oboedit.controller.SessionManager;
import org.oboedit.gui.event.DefaultTermHyperlinkListener;

import org.apache.log4j.*;

public class IDResolutionComponent extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(IDResolutionComponent.class);

	protected boolean checkImmediately;
	protected JPanel resultsPanel = new JPanel();
	protected JPanel buttonPanel = new JPanel();
	protected Collection<LinkIDResolution> resolutions = new ArrayList<LinkIDResolution>();

	public IDResolutionComponent(String id) {
		super(id);
		this.checkImmediately = true;
		setLayout(new BorderLayout());
		add(new JScrollPane(resultsPanel,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_NEVER), BorderLayout.CENTER);
		add(buttonPanel, BorderLayout.SOUTH);
		buttonPanel.setLayout(new GridLayout(1, 2));
		resultsPanel.setLayout(new BoxLayout(resultsPanel, BoxLayout.Y_AXIS));
		JButton checkButton = new JButton("Check IDs");
		checkButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				check(false);
			}
		});

		JButton updateButton = new JButton("Update ids");
		updateButton.addActionListener(new ActionListener() {

			public void actionPerformed(ActionEvent e) {
				update();
			}

		});
		buttonPanel.add(checkButton);
		buttonPanel.add(updateButton);
	}

	@Override
	public void init() {
		if (checkImmediately)
			check(true);
	}

	protected static class ResolutionCheckbox extends JCheckBox {
		protected LinkIDResolution res;

		public ResolutionCheckbox(LinkIDResolution res) {
			this.res = res;
		}

		public LinkIDResolution getResolution() {
			return res;
		}
	}

	public void check(boolean clear) {
		if (clear)
			resolutions.clear();
		resultsPanel.removeAll();
		try {
			IDUtil.updateIDs(SessionManager.getManager().getSession(),
					resolutions, true);
			resultsPanel.add(new JLabel("<html><i>No ID issues</i></html>"));
		} catch (UnresolvedIDsException e) {

			for (LinkIDWarning warning : e.getWarnings()) {
				JComponent linePanel = getLine(warning, resolutions);
				resultsPanel.add(linePanel);
			}
		}
		validate();
		repaint();
	}

	public static JComponent getLine(LinkIDWarning warning,
			final Collection<LinkIDResolution> resolutions) {
		JPanel linePanel = new JPanel();
		linePanel.setLayout(new BorderLayout());
		HyperlinkLabel label = new HyperlinkLabel(IDUtil.getDescription(
				warning, true));
		label.addHyperlinkListener(new DefaultTermHyperlinkListener(label,
				SessionManager.getManager().getCurrentLinkDatabase()));
		linePanel.add(label, BorderLayout.NORTH);
		linePanel.add(Box.createHorizontalStrut(20), BorderLayout.WEST);
		JPanel optionsPanel = new JPanel();
		optionsPanel.setLayout(new BoxLayout(optionsPanel, BoxLayout.Y_AXIS));
		linePanel.add(optionsPanel, BorderLayout.CENTER);
		final ResolutionCheckbox ignoreCheckbox = new ResolutionCheckbox(
				DefaultLinkIDResolution.getIgnoreResolution(warning.getLink()));
		final Collection<ResolutionCheckbox> otherOptions = new ArrayList<ResolutionCheckbox>();
		boolean localInterventions = false;
		boolean automatedOptions = false;
		for (final LinkIDResolution res : warning.getResolutions()) {
			if (res.requiresUserIntervention()) {
				localInterventions = true;
				final ResolutionCheckbox resolutionBox = new ResolutionCheckbox(
						res);
				resolutionBox.addActionListener(new ActionListener() {

					public void actionPerformed(ActionEvent e) {
						if (resolutionBox.isSelected()) {
							ignoreCheckbox.setSelected(false);
							resolutions.add(resolutionBox.getResolution());
						} else
							resolutions.remove(resolutionBox.getResolution());
					}

				});
				otherOptions.add(resolutionBox);
				Box resolutionPanel = Box.createHorizontalBox();
				resolutionPanel.add(resolutionBox);
				resolutionPanel.add(getLabelComponent(res));
				optionsPanel.add(resolutionPanel);
			} else {
				JCheckBox autoBox = new JCheckBox();
				autoBox.setEnabled(false);
				autoBox.setSelected(true);
				Box resolutionPanel = Box.createHorizontalBox();
				resolutionPanel.add(autoBox);
				resolutionPanel.add(getLabelComponent(res));
				optionsPanel.add(resolutionPanel, 0);
				resolutions.add(res);
				automatedOptions = true;
			}
		}
		if (!automatedOptions) {
			Box resolutionPanel = Box.createHorizontalBox();
			resolutionPanel.add(ignoreCheckbox);
			resolutionPanel.add(getLabelComponent(ignoreCheckbox
					.getResolution()));
			optionsPanel.add(resolutionPanel);
		}
		ignoreCheckbox.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				for (ResolutionCheckbox box : otherOptions) {
					box.setSelected(false);
					resolutions.remove(box.getResolution());
					if (ignoreCheckbox.isSelected())
						resolutions.add(ignoreCheckbox.getResolution());
					else
						resolutions.remove(ignoreCheckbox.getResolution());
				}
			}
		});
		return linePanel;

	}

	protected static Component getLabelComponent(LinkIDResolution res) {
		Box out = Box.createHorizontalBox();
		if (res.getParentResolution() == null
				&& res.getTypeResolution() == null)
			out.add(new JLabel("Ignore this link"));
		else {
			StringBuffer buffer = new StringBuffer();
			buffer.append("<html>");
			if (res.getParentResolution() != null) {
				IDResolution p = res.getParentResolution();
				IdentifiedObject original = SessionManager.getManager()
						.getSession().getObject(p.getOriginalID());
				if (original == null) {
					buffer.append("Replace dangling ID " + p.getOriginalID());
				} else
					buffer.append("Replace obsolete term "
							+ HTMLUtil.getHTMLLink(original, false));
				buffer.append(" with term ");
				IdentifiedObject replacement = SessionManager.getManager()
						.getSession().getObject(p.getReplacementID());
				buffer.append(HTMLUtil.getHTMLLink(replacement, false));				
			}
			if (res.getTypeResolution() != null) {
				if (res.getParentResolution() == null) {
					buffer.append("Replace ");
				} else {
					buffer.append(" and replace ");
				}
				IDResolution p = res.getTypeResolution();
				IdentifiedObject original = SessionManager.getManager()
						.getSession().getObject(p.getOriginalID());
				if (original == null) {
					buffer.append("dangling ID " + p.getOriginalID());
				} else
					buffer.append("obsolete type "
							+ HTMLUtil.getHTMLLink(original, false));
				buffer.append(" with type ");
				IdentifiedObject replacement = SessionManager.getManager()
						.getSession().getObject(p.getReplacementID());
				buffer.append(HTMLUtil.getHTMLLink(replacement, false));	
			}
			buffer.append("</html>");
			out.add(new JLabel(buffer.toString()));
		}
		out.add(Box.createHorizontalGlue());
		return out;
	}

	public void update() {
		try {
			IDUtil.updateIDs(SessionManager.getManager().getSession(),
					resolutions, true);
		} catch (UnresolvedIDsException e) {
		}
		SessionManager.getManager().reload();
		check(false);
	}

}
