package org.oboedit.gui;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;

import org.bbop.framework.GUIManager;

import jwf.Wizard;
import jwf.WizardListener;
import jwf.WizardPanel;

import org.apache.log4j.*;

public class SimpleWizard extends Wizard {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(SimpleWizard.class);

	protected String currentPath;
	protected Map<String, LinkableWizardPanel> stepMap =
		new LinkedHashMap<String, LinkableWizardPanel>();
	protected boolean modal;
	protected int stepIndex = -1;
	protected String introText;
	protected boolean cancelled = true;
	
	protected class FirstPanel extends WizardPanel {

		@Override
		public boolean canFinish() {
			return false;
		}

		@Override
		public void display() {
			currentPath = null;
			removeAll();
			setLayout(new BorderLayout());
			if (introText != null) {
				JLabel label = new JLabel("<html>"+introText+"</html>");
				add(label, "North");
			}
			if (stepMap.size() > 1) {
				add(Box.createHorizontalStrut(10), "West");
				ButtonGroup group = new ButtonGroup();
				JPanel buttonPanel = new JPanel();
				buttonPanel.setLayout(new BoxLayout(buttonPanel, BoxLayout.Y_AXIS));
				boolean first = true;
				for(final String label : stepMap.keySet()) {
					JRadioButton radioButton = new JRadioButton(label, first);
					radioButton.addActionListener(new ActionListener() {
						public void actionPerformed(ActionEvent e) {
							currentPath = label;
						}					
					});
					group.add(radioButton);
					first = false;
					buttonPanel.add(radioButton);
				}
				buttonPanel.add(Box.createVerticalGlue());
				add(buttonPanel, "Center");
			}
		}

		@Override
		public void finish() {
		}

		@Override
		public boolean hasNext() {
			return true;
		}

		@Override
		public WizardPanel next() {	
			return stepMap.get(currentPath);
		}

		@Override
		public boolean validateFinish(List arg0) {
			return false;
		}

		@Override
		public boolean validateNext(List arg0) {
			return currentPath != null;
		}
	}
	
	public SimpleWizard(JComponent... panels) {
		this(null, true, true, panels);
	}
	
	public SimpleWizard(String intro, boolean modal, boolean autostart, JComponent... panels) {
		List<JComponent> steps = new LinkedList<JComponent>();
		for(JComponent panel : panels) {
			steps.add(panel);
		}
		setIntroText(intro);
		setModal(modal);
		setPanelList(steps);
		if (autostart)
			start();
	}

	public SimpleWizard(List<JComponent> steps, String intro, boolean modal,
			boolean autostart) {
		setIntroText(intro);
		setModal(modal);
		setPanelList(steps);
		if (autostart)
			start();
	}

	public SimpleWizard(boolean modal) {
		setModal(modal);
	}
	
	public SimpleWizard(String intro) {
		setIntroText(intro);
		setModal(true);
	}
	
	public void setPanelList(List steps) {
		stepMap.clear();
		addSteps("DEFAULT", steps);
	}
	
	public void setIntroText(String intro) {
		this.introText = intro;
	}
	
	public boolean isCancelled() {
		return cancelled;
	}
	
	public void addSteps(String name, List steps) {
		List<LinkableWizardPanel> panels = new LinkedList<LinkableWizardPanel>();
		for(Object o : steps) {
			if (o instanceof LinkableWizardPanel) {
				panels.add((LinkableWizardPanel) o);
			} else if (o instanceof Component) {		
				panels.add(new LinkableWizardPanel((Component) o));				
			}
		}
		stepMap.put(name, LinkableWizardPanel.link(panels));
	}

	public void setModal(boolean modal) {
		this.modal = modal;
	}
	
	protected boolean showFirstPanel() {
		return !(stepMap.size() < 2 && introText == null);
	}

	public void start() {
		currentPath = stepMap.keySet().iterator().next();
		stepIndex = -1;
		Map<String, String> names = new HashMap<String, String>();
		names.put(Wizard.BACK_I18N, "Back");
		names.put(Wizard.NEXT_I18N, "Next");
		names.put(Wizard.FINISH_I18N, "Finish");
		names.put(Wizard.HELP_I18N, "Help");
		names.put(Wizard.CANCEL_I18N, "Cancel");
		final JDialog frame = new JDialog(GUIManager.getManager().getFrame(),
				true);
		setI18NMap(names);

		addWizardListener(new WizardListener() {
			/**
			 * Called when the wizard finishes.
			 * 
			 * @param wizard
			 *            the wizard that finished.
			 */
			public void wizardFinished(Wizard wizard) {
				cancelled = false;
				frame.dispose();
			}

			/**
			 * Called when the wizard is cancelled.
			 * 
			 * @param wizard
			 *            the wizard that was cancelled.
			 */
			public void wizardCancelled(Wizard wizard) {
				frame.dispose();
			}

			/**
			 * Called when a new panel has been displayed in the wizard.
			 * 
			 * @param wizard
			 *            the wizard that was updated
			 */
			public void wizardPanelChanged(Wizard wizard) {
			}
		});
		frame.setContentPane(this);
		frame.pack();
		if (showFirstPanel())
			start(new FirstPanel());
		else
			start(stepMap.get("DEFAULT"));
		frame.setVisible(true);
	}
}
