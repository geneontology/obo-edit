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
	public JTextArea textArea;
	
	protected void update() {
		removeAll();
		textArea = new JTextArea();
		JPanel buttonPanel = new JPanel();
		
		ActionListener al = new QueryButtonActionListener();
		addButton("Query",al,buttonPanel);
		add(buttonPanel,"SOUTH");
		JScrollPane sp = new JScrollPane(textArea,
				JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
				JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		//add(textArea,"NORTH");
		add(sp,"CENTER");
		
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
			
			String queryText = textArea.getText();
			System.err.println("event: "+e);
			System.err.println("qtext: "+queryText);
			Query q = new jpl.Query(queryText);
			System.err.println("q: "+q);
			for (Hashtable h : q.allSolutions()) {
				for (Object k : h.keySet()) {
					Term val = (Term)h.get(k);
					System.out.println(k+" = "+val);
				}
			}
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
