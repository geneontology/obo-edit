package org.oboedit.gui.components.ontologyGeneration.extraction;

import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.StringTokenizer;
import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import javax.swing.text.BadLocationException;
import javax.swing.text.Element;
import javax.swing.text.ElementIterator;
import javax.swing.text.html.HTMLDocument;
import javax.swing.text.html.HTMLEditorKit;

import org.apache.log4j.Logger;
import org.oboedit.gui.components.ontologyGeneration.CandidateDefinition;

/**
 * Extraction of text from HTML to extend truncated {@link CandidateDefinition}
 * 
 * @author Marcel Hanke
 * @author Goetz Fabian
 * 
 */
public class HtmlToTextExtraction extends DataExtraction
{
	private HTMLDocument doc;

	private final static Logger logger = Logger.getLogger(HtmlToTextExtraction.class);

	/**
	 * Tries to extend an existing definition by looking up its source html
	 * content. If no extension could be found, the definition is left as it is.
	 * 
	 * @return true if extended, false otherwise
	 */
	@Override
	public boolean definitionExtraction(CandidateDefinition definition)
	{
		if (definition == null || definition.getCachedURLs() == null || definition.getCachedURLs().isEmpty()) {
			return false;
		}

		if (!load(definition)) {
			return false;
		}

		if (!parse(definition)) {
			return false;
		}

		return true;
	}

	private boolean load(CandidateDefinition definition)
	{
		if (definition == null || definition.getCachedURLs() == null || definition.getCachedURLs().isEmpty()) {
			return false;
		}

		for (int i = 0; i < definition.getCachedURLs().size(); i++) {
			String urlString = definition.getCachedURLs().get(i);
			if (urlString.endsWith("pdf") || urlString.endsWith("ppt") || urlString.endsWith("doc")) {
				logger.error("Definitions from files not in HTML are not extended.");
				return false;
			}
			if (i > 0) {
				logger.error("Try " + (i + 1));
			}
			try {
				ScheduledExecutorService service = Executors.newScheduledThreadPool(2);
				Future<String> taskFuture = service.submit(new HtmlExtractionCallable(urlString));
				service.schedule(new Timeout(taskFuture), 10000, TimeUnit.MILLISECONDS);
				service.shutdown();
				taskFuture.get();
				return true;
			}
			catch (CancellationException e) {
				logger.error("HTMLConnection call was interrupted");
			}
			catch (InterruptedException e) {
				logger.error("shouldn't happen -- service was shutdown");
			}
			catch (ExecutionException e) {
				logger.error("HTML parsing failed unexpectedly");
			}
		}
		return false; // return false if no cache url could be resolved
	}

	private HTMLDocument fetchDocument(String urlString) throws MalformedURLException, IOException
	{
		try {
			URL url = new URL(urlString);

			HTMLEditorKit kit = new HTMLEditorKit();
			doc = (HTMLDocument) kit.createDefaultDocument();

			doc.putProperty("IgnoreCharsetDirective", Boolean.TRUE);
			URLConnection con = url.openConnection();
			con.setConnectTimeout(5000);
			con.connect();
			Reader reader = new InputStreamReader(con.getInputStream());
			kit.read(reader, doc, 0);
		}
		catch (BadLocationException e) {
			logger.error(e.getLocalizedMessage());
		}
		return doc;
	}

	class HtmlExtractionCallable implements Callable<String>
	{
		private final String urlString;

		public HtmlExtractionCallable(String urlString)
		{
			this.urlString = urlString;
		}

		public String call()
		{
			try {
				fetchDocument(urlString);
				return "done";
			}
			catch (IOException e) {
				return "interrupted";
			}
		}
	}

	class Timeout implements Runnable
	{
		public Timeout(Future<?> target)
		{
			this.target = target;
		}

		public void run()
		{
			target.cancel(true);
		}

		private Future<?> target;
	}

	private boolean parse(CandidateDefinition definition)
	{
		String def = definition.getDefinition();
		if (def.endsWith("...")) {
			def = def.substring(0, def.length() - 3);
		}
		// filter out some special characters
		if (def.contains("[") || def.contains("]")) {
			StringTokenizer tokenizer = new StringTokenizer(def);
			String token;
			def = "";
			while (tokenizer.hasMoreTokens()) {
				token = tokenizer.nextToken();
				if (token.contains("[")) {
					token = token.replace('[', ' ').trim();
				}
				else if (token.contains("]")) {
					token = token.replace(']', ' ').trim();
				}
				def = def + " " + token;
			}
		}
		ElementIterator iter = new ElementIterator(doc);
		Element elem;
		try {
			boolean run = true;
			while (run) {
				elem = iter.next();
				if (elem == null) {
					run = false;
					continue;
				}

				// we found the main content of the HTML document
				if (elem.getName().equals("content")) {
					String element;

					final int length = elem.getDocument().getLength();
					if (length <= 0) {
						return false;
					}
					element = elem.getDocument().getText(0, length - 1);
					if (element.length() > 500000) { // dont parse long
						// documents
						System.err.println("Document to long to be parsed: len=" + element.length());
						return false;
					}
					int defLength = def.length();

					// extending definition if it contains def
					if (defLength > 2 && element.contains(def.subSequence(0, defLength - 1))) {
						int begin = element.indexOf(def.substring(0, defLength - 1));

						boolean extending = true;
						int i = begin + defLength - 1;
						while (extending) {
							if (element.length() > i && i >= 0) {
								if (element.charAt(i) == '.' || element.charAt(i) == '!') {
									extending = false;
									String newDef = element.subSequence(begin, i) + ".";
									newDef = newDef.replace('\n', ' ');

									if (def.equals(newDef)) {
										return false;
									}
									else {
										definition.setDefinition(newDef);

										// write html formatted definition
										super.generateHTMLFormattedDefinition(definition);

										return true;
									}
								}
							}
							else {
								logger.error("definition in HTML not found.");
								break;
							}
							i++;
						}
						break;
					}
				}
			}
		}
		catch (BadLocationException e) {
			logger.error(e.getLocalizedMessage());
			return false;
		}
		catch (Exception e) {
			logger.error(e.getLocalizedMessage());
			return false;

		}
		return false;
	}

	@Override
	public String fileExtraction(File file)
	{
		// TODO Auto-generated method stub

		return null;
	}
}
