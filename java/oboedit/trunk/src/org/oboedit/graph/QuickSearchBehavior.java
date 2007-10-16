package org.oboedit.graph;

import java.awt.Color;
import java.awt.Paint;
import java.awt.Rectangle;
import java.awt.event.FocusAdapter;
import java.awt.event.FocusEvent;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.StringTokenizer;

import javax.swing.JTextField;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;

import org.bbop.swing.ShapeUtil;
import org.obo.datamodel.IdentifiedObject;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.Synonym;
import org.obo.datamodel.SynonymedObject;
import org.obo.util.TermUtil;
import org.oboedit.gui.components.LinkDatabaseCanvas;

import edu.umd.cs.piccolo.PNode;
import edu.umd.cs.piccolo.activities.PActivity;
import edu.umd.cs.piccolo.nodes.PPath;

public class QuickSearchBehavior implements ViewBehavior, NodeDecorator {

	protected static final int MAX_SEARCH_RESULTS = 10;

	protected static final double SEARCH_PANEL_HEIGHT = 200;

	protected LinkDatabaseCanvas canvas;

	protected JTextField searchField = new JTextField(30);

	protected Paint resultsPaint = Color.yellow;

	protected KeyListener keyListener = new KeyAdapter() {
		@Override
		public void keyPressed(KeyEvent e) {
			if (e.getKeyCode() == KeyEvent.VK_F && e.isControlDown()) {
				startSearch();
			}
		}
	};

	public Collection<LinkedObject> getVisResults() {
		return visResults;
	}

	protected static class SearchResult implements Comparable<SearchResult> {
		protected LinkedObject result;

		protected int score;

		public SearchResult(LinkedObject result, int score) {
			this.result = result;
			this.score = score;
		}

		public int compareTo(SearchResult o) {
			return score - o.getScore();
		}

		public LinkedObject getResult() {
			return result;
		}

		public int getScore() {
			return score;
		}

		public String toString() {
			return "[" + result + ", score=" + score + "]";
		}
	}

	protected Collection<LinkedObject> visResults = new LinkedList<LinkedObject>();

	protected List<SearchResult> results = new LinkedList<SearchResult>();

	protected PPath resultPanel = new PPath();

	protected void initiateSubSearch() {
		visResults.clear();
		results = new ArrayList<SearchResult>();
		Collection<String> searchWords = new LinkedList<String>();
		StringTokenizer tokenizer = new StringTokenizer(searchField.getText());
		while (tokenizer.hasMoreTokens())
			searchWords.add(tokenizer.nextToken().toLowerCase());
		for (IdentifiedObject io : canvas.getLinkProviderDatabase()
				.getObjects()) {
			if (io instanceof LinkedObject && !TermUtil.isProperty(io)
					&& !TermUtil.isObsolete(io)) {
				LinkedObject lo = (LinkedObject) io;
				boolean isVisible = canvas.getCollapsibleLinkDatabase()
						.isVisible(lo);
				int score = getScore(lo, searchWords, isVisible);
				boolean resultsChanged = false;
				if (isVisible && score > 0) {
					visResults.add(lo);
				} else if (score > 0) {
					if (results.size() == 0
							|| results.size() < MAX_SEARCH_RESULTS) {
						results.add(new SearchResult(lo, score));
						resultsChanged = true;
					} else {
						SearchResult last = results.get(0);
						if (score > last.getScore()) {
							results.set(0, new SearchResult(lo, score));
							resultsChanged = true;
						}
					}
					if (resultsChanged)
						Collections.sort(results);
				}
			}
		}
		if (results.size() > 0) {
			Collections.reverse(results);
			resultPanel.removeAllChildren();
			if (!canvas.getCamera().getChildrenReference()
					.contains(resultPanel))
				canvas.getCamera().addChild(resultPanel);
			double cameraHeight = canvas.getCamera().getBoundsReference()
					.getHeight();
			double cameraWidth = canvas.getCamera().getBoundsReference()
					.getWidth();
			resultPanel.setPaint(Color.red);
			resultPanel.setWidth(cameraWidth);
			resultPanel.setHeight(SEARCH_PANEL_HEIGHT);
			resultPanel.setOffset(0, cameraHeight - SEARCH_PANEL_HEIGHT);
			double xoff = 10;
			for (SearchResult r : results) {
				OENode path = new OENode(r.getResult(), null, new Rectangle(
						200, 50));
				resultPanel.addChild(path);
				path.setOffset(xoff, 20);
				xoff += 210;
			}
		}
		canvas.decorate();
	}

	protected int getScore(LinkedObject lo, Collection<String> searchWords,
			boolean stopOnFind) {
		int out = 0;
		out += 10 * getStringScore(lo.getName().toLowerCase(), searchWords,
				stopOnFind);
		if (stopOnFind && out > 0)
			return out;
		if (lo instanceof SynonymedObject) {
			for (Synonym s : ((SynonymedObject) lo).getSynonyms()) {
				int score = getStringScore(s.getText().toLowerCase(), searchWords,
						stopOnFind); 
				out += score;
				if (stopOnFind && out > 0)
					return out;
			}
		}
		return out;
	}

	protected int getStringScore(String str, Collection<String> searchWords,
			boolean stopOnFind) {
		int out = 0;
		for (String s : searchWords) {
			int index = str.indexOf(s);
			if (index >= 0) {
				if (index == 0)
					out += 50;
				else if (Character.isWhitespace(str.charAt(index - 1)))
					out += 5;
				else
					out += 1;
			} else
				return 0;
			if (stopOnFind && out > 0)
				return out;
		}
		return out;
	}

	public void install(LinkDatabaseCanvas canvas) {
		this.canvas = canvas;
		canvas.addKeyListener(keyListener);
		canvas.addDecorator(this);
	}

	public void uninstall(LinkDatabaseCanvas canvas2) {
		canvas.removeKeyListener(keyListener);
		canvas.removeDecorator(this);
		this.canvas = null;
	}

	boolean searching = false;
	
	protected void startSearch() {
		searching = true;
		resultPanel.removeAllChildren();
		searchField.setText("");
		searchField.setBounds(10, 10, 300, 20);
		canvas.getPlacementPanel().removeAll();
		canvas.getPlacementPanel().setLayout(null);
		canvas.getPlacementPanel().add(searchField);
		canvas.setPlacementPanelVisible(true);
		searchField.requestFocus();
		canvas.validate();
		canvas.repaint();
	}

	protected void completeSearch() {
		searching = false;
		if (canvas.getCamera().getChildrenReference().contains(resultPanel))
			canvas.getCamera().removeChild(resultPanel);
		canvas.setPlacementPanelVisible(true);
		canvas.getPlacementPanel().removeAll();
		canvas.validate();
		Collection<IdentifiedObject> vis = new LinkedList<IdentifiedObject>();
		for(IdentifiedObject io : canvas.getCollapsibleLinkDatabase().getObjects()) {
			if (io instanceof LinkedObject)
				vis.add((LinkedObject) io);
		}
		for (SearchResult result : results) {
			vis.add(result.getResult());
			visResults.add(result.getResult());
		}
		canvas.getCollapsibleLinkDatabase().setVisibleObjects(vis, false);
		canvas.decorate();
		canvas.repaint();
	}

	protected void cancelSearch() {
		searching = false;
		if (canvas.getCamera().getChildrenReference().contains(resultPanel))
			canvas.getCamera().removeChild(resultPanel);
		visResults.clear();
		canvas.setPlacementPanelVisible(true);
		canvas.getPlacementPanel().removeAll();
		canvas.validate();
		canvas.decorate();
		canvas.repaint();
	}

	public PActivity decorate(PNode p, boolean noAnimation) {
		if (p instanceof OENode) {
			OENode node = (OENode) p;
			if (visResults.contains(node.getObject()))
				node.setPaint(resultsPaint);
		}
		return null;
	}

	public QuickSearchBehavior() {
		searchField.addFocusListener(new FocusAdapter() {
			@Override
			public void focusLost(FocusEvent e) {
				if (searching)
					cancelSearch();
			}
		});
		searchField.getDocument().addDocumentListener(new DocumentListener() {

			public void changedUpdate(DocumentEvent e) {
				update(e);
			}

			public void insertUpdate(DocumentEvent e) {
				update(e);
			}

			public void removeUpdate(DocumentEvent e) {
				update(e);
			}

			protected void update(DocumentEvent e) {
				initiateSubSearch();

			}
		});
		searchField.addKeyListener(new KeyAdapter() {
			@Override
			public void keyPressed(KeyEvent e) {
				if (e.getKeyCode() == KeyEvent.VK_ESCAPE) {
					cancelSearch();
				} else if (e.getKeyCode() == KeyEvent.VK_ENTER) {
					completeSearch();
				}
			}
		});

	}

	public boolean onlyDecorateAfterLayout() {
		return false;
	}

}
