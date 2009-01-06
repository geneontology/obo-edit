package org.blipkit.gui.components;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Hashtable;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;

import jpl.Query;
import jpl.Term;

import org.bbop.framework.AbstractGUIComponent;
import org.blipkit.reasoner.impl.DatalogReasoner;
import org.oboedit.controller.SessionManager;

public class DatalogQueryComponent extends AbstractGUIComponent {

	public DatalogQueryComponent(String id) {
		super(id);
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
	}
	


	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;


	@Override
	public void init() {
		removeAll();
		
		update();
	}
	public JTextArea queryTextArea;
	public JTextArea resultsTextArea;
	
	DatalogReasoner datalog;
	
	protected void update() {
		removeAll();
		
		queryTextArea = new JTextArea();
		resultsTextArea = new JTextArea();
		JPanel buttonPanel = new JPanel();
		
		ActionListener al = new QueryButtonActionListener();
		addButton("Query",al,buttonPanel);
		JScrollPane querySP = new JScrollPane(queryTextArea,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		JScrollPane resultsSP = new JScrollPane(resultsTextArea,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		//add(textArea,"NORTH");
		add(buttonPanel,"SOUTH");
		add(querySP,"NORTH");
		add(resultsSP,"CENTER");
		
		validate();
		repaint();
	}
	
	private JButton addButton(String name,ActionListener al,JPanel parent) {
		JButton button = new JButton(name);
		button.setActionCommand(name);
		button.addActionListener(al);
		parent.add(button);
		return button;
	}
	
	private class QueryButtonActionListener implements ActionListener {

		
		public void actionPerformed(ActionEvent e) {
			// TODO: recache on changes..
			if (datalog == null) {
				datalog = new DatalogReasoner();
				datalog.setLinkDatabase(SessionManager.getManager().getCurrentLinkDatabase());
				datalog.recache();
			}
			String queryText = queryTextArea.getText();
			System.err.println("event: "+e);
			System.err.println("qtext: "+queryText);
			Query q = new jpl.Query(queryText);
			System.err.println("q: "+q);
			int n=0;
			StringBuffer sb = new StringBuffer();
			for (Hashtable h : q.allSolutions()) {
				n++;
				for (Object k : h.keySet()) {
					Term val = (Term)h.get(k);
					System.out.println(k+" = "+val);
					sb.append(k+" = "+val+" // ");
				}
				sb.append("\n");
			}
			System.out.println("num sols="+n);
			sb.append("---\nnum sols="+n+"\n");
			resultsTextArea.setText(sb.toString());

		}
	}

	@Override
	public void cleanup() {
		
		super.cleanup();
	}

	protected void search() {
	}

	@Override
	public String getName() {
		return "Datalog Query Component";
	}

}
