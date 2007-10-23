package org.bbop.commandline;

import java.util.List;

public interface ArgumentSignature {

    public int getID();

    //   public void doStore();

    public void init(CommandLineParser p, boolean defaultAttempt);
    public ArgumentSignature copy();
    public void setOnlyAcceptAsLastResort(boolean lastResort);
    public boolean onlyAcceptAsLastResort();
    public void accept(CommandLineParser p) throws FailException;
    public String getShortDocumentation();
    public List getValues() throws UnfullfilledException;
}
