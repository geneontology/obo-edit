package org.oboedit.gui.components.ontologyGeneration;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.log4j.Logger;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.StopAnalyzer;
import org.apache.lucene.analysis.snowball.SnowballAnalyzer;
import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field;
import org.apache.lucene.index.IndexWriter;
import org.apache.lucene.index.IndexWriter.MaxFieldLength;
import org.apache.lucene.queryParser.ParseException;
import org.apache.lucene.queryParser.QueryParser;
import org.apache.lucene.search.BooleanQuery;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.search.BooleanClause.Occur;
import org.apache.lucene.store.RAMDirectory;
import org.obo.datamodel.LinkedObject;
import org.obo.datamodel.OBOClass;
import org.obo.datamodel.Synonym;
import org.oboedit.controller.SessionManager;

/**
 * Manager for the index to match against the ontology loaded in OBO-Edit
 */
public class OBOOntologyIndexManager
{
	private static final Logger logger = Logger.getLogger(OBOOntologyIndexManager.class);
	private static final String ID_FIELD = "id";
	private static final String LABEL_FIELD = "label";
	private IndexSearcher searcher;
	private RAMDirectory ramDirectory;
	private static Analyzer analyzer;
	private QueryParser queryParser;
	private int docCount = 0;
	private static OBOOntologyIndexManager theInstance;
	private static boolean isIndexInitializedOnce = false;

	/**
	 * Private constructor of {@link OBOOntologyIndexManager} Singleton
	 */
	private OBOOntologyIndexManager()
	{
		ramDirectory = new RAMDirectory();
		analyzer = new SnowballAnalyzer("English", StopAnalyzer.ENGLISH_STOP_WORDS);
		queryParser = new QueryParser(LABEL_FIELD, analyzer);
	}

	/**
	 * @return the instance of the singleton {@link OBOOntologyIndexManager}
	 */
	public static synchronized OBOOntologyIndexManager getInstance()
	{
		if (theInstance == null) {
			theInstance = new OBOOntologyIndexManager();
		}
		return theInstance;
	}

	/**
	 * Recreating the Lucene in memory index for the specified {@link Collection} of {@link OBOClass} instances
	 * 
	 * @param presentTerms
	 */
	public void recreateIndexWith(Collection<OBOClass> presentTerms)
	{
		synchronized (theInstance) {
			this.docCount = presentTerms.size();
			IndexWriter writer = null;
			try {
				ramDirectory.close();
				ramDirectory = new RAMDirectory();
				writer = new IndexWriter(ramDirectory, analyzer, MaxFieldLength.LIMITED);

				for (OBOClass term : presentTerms) {
					// add ontology labels
					Document document = new Document();
					document.add(new Field(ID_FIELD, term.getID(), Field.Store.YES, Field.Index.NO));
					document.add(new Field(LABEL_FIELD, term.getName(), Field.Store.NO, Field.Index.ANALYZED));

					// add ontology synonyms
					for (Synonym synonym : term.getSynonyms()) {
						document.add(new Field(LABEL_FIELD, synonym.getText(), Field.Store.NO, Field.Index.ANALYZED));
					}
					writer.addDocument(document);
				}
				writer.optimize();
				writer.close();
				if (this.searcher != null) {
					this.searcher.close();
				}
				this.searcher = null;
				isIndexInitializedOnce = true;
			}
			catch (FileNotFoundException exception) {
				logger.error("Indexing local ontology failed!", exception);
			}
			catch (IOException exception) {
				logger.error("Indexing local ontology failed!", exception);
			}
		}
	}

	/**
	 * Find all ids for {@link LinkedObject} loaded in OBO-Edit which overlap with the specified strings
	 * 
	 * @param queryList
	 * @return List of ids for {@link LinkedObject}
	 */
	private List<String> lookup(List<Query> queryList)
	{
		if (isIndexInitializedOnce && this.searcher == null) {
			initSearcher();
		}

		synchronized (theInstance) {
			List<String> list = new ArrayList<String>(docCount);
			if (searcher != null) {
				TopDocs topDocs = null;
				for (Query query : queryList) {
					try {
						topDocs = searcher.search(query, docCount);
						if (topDocs != null) {
							for (ScoreDoc scoreDoc : topDocs.scoreDocs) {
								Document doc = searcher.doc(scoreDoc.doc);
								Field field = doc.getField(ID_FIELD);
								list.add(field.stringValue());
							}
						}
					}
					catch (IOException exception) {
						logger.error("Index lookup failed!", exception);
					}
				}
			}
			return list;
		}
	}

	/**
	 * Returns the ontology terms which match the full query
	 * 
	 * @param list of query strings
	 * @return list of matching ontology term ids
	 */
	public List<String> lookupExact(List<String> list)
	{
		List<Query> queries = new ArrayList<Query>();
		Set<String> testSet = new HashSet<String>();
		for (String string : list) {
			try {
				string = QueryParser.escape(string);
				Query parsedQuery = queryParser.parse("\"" + string + "\"");
				queries.add(parsedQuery);
			}
			catch (ParseException exception) {
				logger.error("Error parsing query: "+ exception.getMessage());
			}
			String testString = string;
			testSet.add(testString.toLowerCase());
			if (testString.charAt(testString.length() - 1) == 's') {
				testSet.add(testString.substring(0, testString.length() - 1).toLowerCase());
			}
		}

		List<String> idList = lookup(queries);
		List<String> filteredList = new ArrayList<String>(idList.size());
		Set<String> idSet = new HashSet<String>();

		OUTERLOOP: for (String id : idList) {
			OBOClass ontologyClass = (OBOClass) SessionManager.getManager().getSession().getLinkDatabase().getObject(id);
			if (ontologyClass != null) {
				String name = ontologyClass.getName();
				if (testSet.contains(name)) {
					if (!idSet.contains(id)) {
						filteredList.add(id);
						idSet.add(id);
						continue OUTERLOOP;
					}
				}
				Set<Synonym> synonyms = ontologyClass.getSynonyms();
				for (Synonym synonym : synonyms) {
					if (testSet.contains(synonym.getText())) {
						if (!idSet.contains(id)) {
							filteredList.add(id);
							idSet.add(id);
							continue OUTERLOOP;
						}
					}
				}
			}
			else {
				logger.warn(String.format("Term with id '%s' not found!", id));
			}
		}
		return filteredList;
	}

	/**
	 * Returns ontology terms, where all parts of the term (document) are contained in the query
	 * 
	 * @param list
	 * @return
	 */
	public List<String> lookupStrict(List<String> list)
	{
		List<Query> queries = new ArrayList<Query>();
		Set<Query> querySet = new HashSet<Query>();
		for (String string : list) {
			BooleanQuery query = new BooleanQuery();
			try {
				string = QueryParser.escape(string);
				if (string.length() != 0) {
					Query parseQuery = queryParser.parse(string);
					if (!querySet.contains(parseQuery)) {
						query.add(parseQuery, Occur.SHOULD);
						querySet.add(parseQuery);
					}
				}
			}
			catch (ParseException exception) {
				logger.error("Error parsing query: "+ exception.getMessage());
			}
			queries.add(query);
		}
		List<String> idList = lookup(queries);
		List<String> filteredIdList = new ArrayList<String>(idList.size());
		Set<String> set = new HashSet<String>();
		for (String id : idList) {
			OBOClass ontologyClass = (OBOClass) SessionManager.getManager().getSession().getLinkDatabase().getObject(id);
			if (ontologyClass != null) {
				String name = ontologyClass.getName();
				for (String string : list) {
					if (string.contains(name)) {
						filteredIdList.add(id);
						set.add(id);
						continue;
					}
				}
			}
			else {
				logger.warn(String.format("Term with id '%s' not found!", id));
			}

		}
		return filteredIdList;
	}

	/**
	 * Initializing the index searcher
	 */
	private void initSearcher()
	{
		synchronized (theInstance) {
			try {
				searcher = new IndexSearcher(ramDirectory);
			}
			catch (IOException exception) {
				logger.error("Lucene index could not be initialized", exception);
			}
		}
	}
}
