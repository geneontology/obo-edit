package org.oboedit.gui.components;

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JEditorPane;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

import org.bbop.framework.AbstractGUIComponent;
import org.obo.reasoner.ReasonerListener;
import org.obo.reasoner.ReasonerRegistry;
import org.oboedit.controller.SessionManager;

import org.apache.log4j.*;

public class ReasonerManagerComponent extends AbstractGUIComponent {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(ReasonerManagerComponent.class);

	private static final long serialVersionUID = 1L;

	private static SessionManager sessionManager = SessionManager.getManager();

	protected JComboBox reasonerChoice = new JComboBox();

	protected JEditorPane summaryField = new JEditorPane();

	protected JScrollPane summaryScroller = new JScrollPane(summaryField,
			JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
			JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

	protected ReasonerRegistry registry = ReasonerRegistry.getInstance();

	protected ReasonerListener reasonerActionListener = new ReasonerListener() {

		public void reasoningFinished() {
			updateProgressPanel(sessionManager.getUseReasoner());
		}

		public void reasoningStarted() {
			logger.debug("ReasonerManagerComponent: reasoningStarted");
		}

		public void reasoningCancelled() {
			logger.debug("ReasonerManagerComponent: reasoningCancelled");
			updateProgressPanel(sessionManager.getUseReasoner()); 
		}
	};

	protected ActionListener reasonerListener = new ActionListener() {
		public void actionPerformed(ActionEvent e) {
			logger.debug("calling enableReasoner " + reasonerChoice.getSelectedItem());
			enableReasoner((String)reasonerChoice.getSelectedItem());
		}
	};

	public ReasonerManagerComponent(String id) {		
		super(id);
		JPanel panel = new JPanel();
		panel.setLayout(new FlowLayout());	
		setPreferredSize(new Dimension(450,200));
		summaryField.setPreferredSize(new Dimension(400, 20));
		summaryField.setContentType("text/html");
		summaryField.setEditable(false);

		add(new JLabel("Reasoner: "));
		add(reasonerChoice);
		
		// ======================
		//partial reasoning in steps
		// for example while asserting implied links.. there will be mass deletion of links, mass addition and then reasoner runs again..
		// instead of reasoning after each link addition
		JCheckBox partialCheck = new JCheckBox("Partial/Step Reasoning"); 
		JButton topupButton = new JButton("Top-Up Results");
		//commenting out partial/step reasoning for 2.1 release
//		add(partialCheck);
//		add(topupButton);

//		// continuous incremental reasoning option
		JCheckBox incrementalCheck =  new JCheckBox("Activate Incremental Reasoning");
//
//		// basically what this reasones over the complete linkdatabase again - same effect as switching the reasoner off and then on again
//		// this will be used when the continuous incremental reasoning is off
		JButton refreshButton = new JButton("Reset/Refresh Results");
		//add(incrementalCheck);
		//add(refreshButton);
		// ======================
		
		add(summaryField);

		reasonerChoice.addItem("OFF");
		// Get reasoner names from registry
		ReasonerRegistry registry = ReasonerRegistry.getInstance();
//		logger.debug("Registered reasoners: " + registry.getRegisteredNames()); 
		for (String registryName : registry.getRegisteredNames()) 
			reasonerChoice.addItem(registryName);

		reasonerChoice.setMaximumSize(new Dimension(Integer.MAX_VALUE, reasonerChoice.getPreferredSize().height));
		reasonerChoice.setSelectedItem(sessionManager.getReasonerName());

	}

	protected void enableReasoner(String reasonerChoice) {
		logger.debug("ReasonerManagerComponent.enableReasoner");
		summaryField.setText(" ");
		sessionManager.setReasonerName(reasonerChoice);
	}

	protected void cancelReasoner(String reasonerChoice) {
		logger.debug("reasonerChoice: " + reasonerChoice);
		logger.debug("ReasonerManagerComponent.cancelReasoner");
		summaryField.setText("Reasoning cancelled.. ");
		sessionManager.setReasonerName(reasonerChoice);
	}


	protected void updateProgressPanel(final boolean enableReasoner) {
		String text = "";
		//		logger.debug("ReasonerManagerComponent -- updateProgressPanel: " + enableReasoner);
		if (enableReasoner) {
			text += "<html><body>\n";
			text += "Reasoning completed.";
			text += "</body></html>";
		} else{
			text += "<html><body>\n";
			text += "Reasoner off";
			text += "</body></html>";
		}
		final String summaryText = text;

		Runnable screenUpdate = new Runnable() {
			public void run() {
				summaryField.setText("");
				if (enableReasoner)
					add(summaryScroller, "South");
				else
					remove(summaryScroller);

				reasonerChoice.removeActionListener(reasonerListener);
				reasonerChoice.setSelectedItem(sessionManager.getReasonerName());
				reasonerChoice.addActionListener(reasonerListener);

				validate();
				repaint();
				summaryField.setText(summaryText);
			}
		};
		SwingUtilities.invokeLater(screenUpdate);
	}

	@Override
	public void init() {
		sessionManager.addReasonerListener(reasonerActionListener, true);
		updateProgressPanel(sessionManager.getUseReasoner());
	}

	@Override
	public String getName() {
		return "Reasoner Manager";
	}

	@Override
	public void cleanup() {
		sessionManager.removeReasonerListener(
				reasonerActionListener);
	}
}
