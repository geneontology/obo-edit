package org.oboedit.gui;

import java.awt.Component;
import java.awt.GridLayout;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import javax.swing.JPanel;

import jwf.WizardPanel;

import org.apache.log4j.*;

public class LinkableWizardPanel extends WizardPanel {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(LinkableWizardPanel.class);
	
	protected Component displayMe;
	protected LinkableWizardPanel nextPanel;
	
	public LinkableWizardPanel() {
		this(new JPanel());
	}

	public LinkableWizardPanel(Component displayMe) {
		this.displayMe = displayMe;
	}
	
	public static LinkableWizardPanel link(LinkableWizardPanel... panels) {
		return link(Arrays.asList(panels));
	}
	
	public static LinkableWizardPanel link(Collection<LinkableWizardPanel> panels) {
		if (panels.isEmpty())
			throw new IllegalArgumentException("Panel list may not be empty");
		Iterator<LinkableWizardPanel> it = panels.iterator();
		LinkableWizardPanel last = it.next();
		while(it.hasNext()) {
			LinkableWizardPanel panel = it.next();
			last.setNextPanel(panel);
			last = panel;
		}
		return panels.iterator().next();
	}
	
	public void setNextPanel(LinkableWizardPanel nextPanel) {
		this.nextPanel = nextPanel;
	}
	
	@Override
	public boolean canFinish() {
		return nextPanel == null;
	}
	
	/**
	 * Do any special pre-display initialization
	 */
	protected void init() {
		
	}

	@Override
	public void display() {
		init();
		removeAll();
		setLayout(new GridLayout(1,1));
		add(displayMe);
	}

	@Override
	public void finish() {
	}

	@Override
	public boolean hasNext() {
		return nextPanel != null;
	}

	@Override
	public WizardPanel next() {
		return nextPanel;
	}

	@Override
	public boolean validateFinish(List arg0) {
		return true;
	}

	@Override
	public boolean validateNext(List arg0) {
		return true;
	}

}
