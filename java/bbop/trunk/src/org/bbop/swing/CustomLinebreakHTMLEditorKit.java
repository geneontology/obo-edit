package org.bbop.swing;

import java.text.CharacterIterator;
import java.util.ArrayList;
import java.util.List;

import javax.swing.text.Element;
import javax.swing.text.GlyphView;
import javax.swing.text.Segment;
import javax.swing.text.StyleConstants;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import javax.swing.text.html.HTML;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.InlineView;
import org.apache.log4j.*;

public class CustomLinebreakHTMLEditorKit extends HTMLEditorKit {

	//initialize logger
	protected final static Logger logger = Logger.getLogger(CustomLinebreakHTMLEditorKit.class);

	protected static class SegmentCache {
		/**
		 * A global cache.
		 */
		private static SegmentCache sharedCache = new SegmentCache();

		/**
		 * A list of the currently unused Segments.
		 */
		private List segments;

		/**
		 * Returns the shared SegmentCache.
		 */
		public static SegmentCache getSharedInstance() {
			return sharedCache;
		}

		/**
		 * A convenience method to get a Segment from the shared
		 * <code>SegmentCache</code>.
		 */
		public static Segment getSharedSegment() {
			return getSharedInstance().getSegment();
		}

		/**
		 * A convenience method to release a Segment to the shared
		 * <code>SegmentCache</code>.
		 */
		public static void releaseSharedSegment(Segment segment) {
			getSharedInstance().releaseSegment(segment);
		}

		/**
		 * Creates and returns a SegmentCache.
		 */
		public SegmentCache() {
			segments = new ArrayList(11);
		}

		/**
		 * Returns a <code>Segment</code>. When done, the
		 * <code>Segment</code> should be recycled by invoking
		 * <code>releaseSegment</code>.
		 */
		public Segment getSegment() {
			synchronized (this) {
				int size = segments.size();

				if (size > 0) {
					return (Segment) segments.remove(size - 1);
				}
			}
			return new CachedSegment();
		}

		/**
		 * Releases a Segment. You should not use a Segment after you release
		 * it, and you should NEVER release the same Segment more than once, eg:
		 * 
		 * <pre>
		 * segmentCache.releaseSegment(segment);
		 * segmentCache.releaseSegment(segment);
		 * </pre>
		 * 
		 * Will likely result in very bad things happening!
		 */
		public void releaseSegment(Segment segment) {
			if (segment instanceof CachedSegment) {
				synchronized (this) {
					segment.array = null;
					segment.count = 0;
					segments.add(segment);
				}
			}
		}

		/**
		 * CachedSegment is used as a tagging interface to determine if a
		 * Segment can successfully be shared.
		 */
		private static class CachedSegment extends Segment {
		}
	}

	public class CustomHTMLFactory extends HTMLFactory {
		@Override
		public View create(Element elem) {
			Object o = elem.getAttributes().getAttribute(
					StyleConstants.NameAttribute);
			if (o instanceof HTML.Tag) {
				HTML.Tag kind = (HTML.Tag) o;
				if (kind == HTML.Tag.CONTENT) {
					return new CustomLinebreakInlineView(elem);
				}
			}
			return super.create(elem);
		}
	}

	public class CustomLinebreakInlineView extends InlineView {
		public CustomLinebreakInlineView(Element elem) {
			super(elem);
		}

		@Override
		public View breakView(int axis, int p0, float pos, float len) {
			if (axis == View.X_AXIS) {
				checkPainter();
				int p1 = getGlyphPainter().getBoundedPosition(this, p0, pos,
						len);
				int breakSpot = getBreakSpot(p0, p1);

				if (breakSpot != -1) {
					p1 = breakSpot;
				}
				// else, no break in the region, return a fragment of the
				// bounded region.
				if (p0 == getStartOffset() && p1 == getEndOffset()) {
					return this;
				}
				GlyphView v = (GlyphView) createFragment(p0, p1);
				// v.x = (int) pos;
				return v;
			}
			return this;
		}

		protected int getBreakSpot(int p0, int p1) {
			Segment s = getText(p0, p1);

			for (char ch = s.last(); ch != CharacterIterator.DONE; ch = s.previous()) {
				if (isBreakCharacter(ch)) {
					// found whitespace
					return s.getIndex() - s.getBeginIndex() + 1 + p0;
				}
			}
			return -1;
		}
	}

	protected boolean isBreakCharacter(char c) {
		return Character.isWhitespace(c) || c == '-' || c == '_';
	}

	protected ViewFactory defaultFactory = new CustomHTMLFactory();

	@Override
	public ViewFactory getViewFactory() {
		return defaultFactory;
	}
}
