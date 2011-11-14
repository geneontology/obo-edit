package org.oboedit.gui.components.ontologyGeneration;

import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.ImageIcon;

import net.sf.image4j.codec.ico.ICODecoder;

import org.apache.log4j.Logger;

public class FaviconRetriever
{
	private static final Logger logger = Logger.getLogger(FaviconRetriever.class);
	
	// CONSTANTS
	private static final String faviconPath = "/favicon.ico";
	private static final String shortcutIconString = "<link\\s*rel\\s*=\\s*\"(?:shortcut )?icon\"[^>]*href\\s*=\\s*\"(.+?)\"|<link\\s*href\\s*=\\s*\"(.+?)\"[^>]*rel\\s*=\\s*\"(?:shortcut )?icon\"|</head>";
	private static final int maxHeight = 20;
	
	private static FaviconRetriever sharedInstance;
	
	private Map<String, ImageIcon> faviconCache;
	private static Pattern shortcutIconPattern;
	
	private FaviconRetriever() {
		faviconCache = Collections.synchronizedMap(new HashMap<String, ImageIcon>());
		shortcutIconPattern = Pattern.compile(shortcutIconString, Pattern.CASE_INSENSITIVE);
	}
	
	public static FaviconRetriever sharedInstance() {
		if (sharedInstance == null)
			sharedInstance = new FaviconRetriever();
		
		return sharedInstance;
	}
	
	public boolean hasFaviconForBaseUrl(String baseUrl) {
		return faviconCache.containsKey(baseUrl);
	}
	
	public ImageIcon getFavicon(String url) {
		String baseURL;
		baseURL = baseURLFromURL(url);
		
		if (baseURL == null)
			return null;
		
		// Cache hit -- return favicon right away.
		if (faviconCache.containsKey(baseURL)) {
			return faviconCache.get(baseURL);
		}
		
		// Cache miss -- we need to fetch the favicon.
		ImageIcon image = null;
		try {
			image = fetchFavicon(baseURL);
		} catch (IOException e) {
			logger.trace("Error retrieving favicon for " + url);
		}

		faviconCache.put(baseURL, image);
		
		return image;
	}
	
	private ImageIcon fetchFavicon(String url) throws IOException {
		List<BufferedImage> images;
		
		images = getFaviconsFromURL(extractedFaviconPath(url));
		
		if (images == null) {
			images = getFaviconsFromURL(defaultFaviconPath(url));
		}

		if (images == null || images.isEmpty()) {
			logger.trace(url + ": images empty");
			return null;
		}

		// Select best-fitting image (large, but not larger than 20 px, since the rowHeight is 20 px)
		BufferedImage bestImage = null;
		for (BufferedImage image : images) {
			if (bestImage == null || (image.getHeight() > bestImage.getHeight() && image.getHeight() <= maxHeight)) {
				bestImage = image;
			}
		}
		// Resize if needed
		if (bestImage.getHeight() > 20) {
			int newWidth = (bestImage.getWidth()*maxHeight)/(bestImage.getHeight());
			bestImage = createResizedCopy(bestImage, newWidth, maxHeight);
		}
		
		return new ImageIcon(bestImage);
	}
	
	public static String baseURLFromURL(String urlString) {
		URL url;
		try {
	        url = new URL(urlString);
        }
        catch (MalformedURLException e) {
        	return null;
        }
		
		StringBuilder builder;
		builder = new StringBuilder();
		
		builder.append(url.getProtocol());
		builder.append("://");
		builder.append(url.getHost());
		if (url.getPort() != -1) {
			builder.append(":");
			builder.append(url.getPort());
		}

		return builder.toString();
	}
	
	private static List<BufferedImage> getFaviconsFromURL(URL url) {
		List<BufferedImage> images = null;
		InputStream inputStream = null;
		try {
			inputStream = url.openStream();
			images = ICODecoder.read(inputStream);
		} catch (Exception e) {
		} finally {
			try {
				if (inputStream != null) {
					inputStream.close();
				}
            } catch (IOException e) {
            	logger.trace("Couldn't close inputStream");
            }
		}
		return images;
	}
	
	private static URL defaultFaviconPath(String baseURL) {
		String faviconURL = baseURL + faviconPath;
		try {
	        return new URL(faviconURL);
        }
        catch (MalformedURLException e) {
        	return null;
        }
	}
	
	private static URL extractedFaviconPath(String baseURL) {
		InputStream inputStream;
		try {
	        inputStream = new URL(baseURL).openStream();
	        
			Reader reader = new InputStreamReader(inputStream);
			Scanner streamScanner = new Scanner(reader);
			String result = streamScanner.findWithinHorizon(shortcutIconPattern, 0);
			
			if (result == null)
				return null;
			
			Matcher matcher = shortcutIconPattern.matcher(result);
			matcher.find();
			if (matcher.group(1) == null && matcher.group(2) == null) {
				logger.trace(baseURL + ": no match found");
				return null;
			}
			
			String path = matcher.group(1);
			if (path == null) {
				path = matcher.group(2);
			}
			
			if (path.startsWith("http")) { // absolute path
				return new URL(path);
			} else { // relative path
				if (!baseURL.endsWith("/") && !path.startsWith("/")) {
					path = "/" + path; 
				}
				return new URL(baseURL + path);
			}
        }
        catch (MalformedURLException e) {
	       return null;
        }
        catch (IOException e) {
        	return null;
        }
	}
	
	// see: http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6196792
	private static BufferedImage createResizedCopy(BufferedImage originalImage, int scaledWidth, int scaledHeight)
	{
		BufferedImage scaledImage;
		scaledImage = new BufferedImage(scaledWidth, scaledHeight, BufferedImage.TYPE_INT_ARGB);
		Graphics2D g = scaledImage.createGraphics();
		g.drawImage(originalImage, 0, 0, scaledWidth, scaledHeight, null);
		g.dispose();
		return scaledImage;
	}

}
