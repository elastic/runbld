package com.example;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;
import java.io.File;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.io.BufferedReader;

/**
 * Unit test for simple App.
 */
public class AppTest 
    extends TestCase
{
    /**
     * Create the test case
     *
     * @param testName name of the test case
     */
    public AppTest( String testName )
    {
        super( testName );
    }

    /**
     * @return the suite of tests being tested
     */
    public static Test suite()
    {
        return new TestSuite( AppTest.class );
    }

    public void testBad()
    {
        assertTrue( false );
    }

    public void testError() throws java.io.IOException
    {
        BufferedReader rdr = Files.newBufferedReader(Paths.get("/some/bad/file"));
        assertTrue(rdr.read() == 1);
    }

    public void testGood()
    {
        assertTrue( true );
    }
}
