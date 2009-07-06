package org.bbop.swing;

import java.lang.Character;
import java.lang.String;
import javax.swing.JTextField;
import javax.swing.text.Document;
import javax.swing.text.PlainDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.AttributeSet;

import org.apache.log4j.*;

public class NumericField extends JTextField 
	{

	//initialize logger
	protected final static Logger logger = Logger.getLogger(NumericField.class);

  /**
	 * 
	 */
	private static final long serialVersionUID = -7489721588886177863L;

public NumericField(int cols) 
    {
      super(cols);
    }

  @Override
protected Document createDefaultModel()
    {
      return new NumericDocument();
    }

  static class NumericDocument extends PlainDocument 
  {
    /**
	 * 
	 */
	private static final long serialVersionUID = -6202634288406182851L;

	@Override
	public void insertString(int offs, String str, AttributeSet a) 
      throws BadLocationException
      {
      
	if (str == null)
	  {
	    return;
	  }
	char[] chars = str.toCharArray();
	StringBuffer digits = new StringBuffer();
	for (int i = 0; i < chars.length; i++)
	  {
	    if (Character.isDigit(chars[i]))
	      digits.append (chars[i]);
	  }
	if (digits.length() > 0)
	  {
	    super.insertString(offs, digits.toString(), a);
	  }
      }
  }
}

