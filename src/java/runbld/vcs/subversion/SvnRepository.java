package runbld.vcs.subversion;

/*
 * Hack to wrap SVNKit because the bytecode for SVNRepository.log()
 * doesn't get generated correctly.
 */

import java.util.Collection;
import java.util.Map;
import org.tmatesoft.svn.core.internal.io.dav.DAVRepositoryFactory;
import org.tmatesoft.svn.core.SVNLogEntry;
import org.tmatesoft.svn.core.SVNException;
import org.tmatesoft.svn.core.SVNURL;
import org.tmatesoft.svn.core.io.SVNRepositoryFactory;
import org.tmatesoft.svn.core.auth.ISVNAuthenticationManager;
import org.tmatesoft.svn.core.io.SVNRepository;
import org.tmatesoft.svn.core.wc.SVNWCUtil;

public class SvnRepository {
    private String url;
    private SVNRepository repository;
    
    public SvnRepository(String url) {
        DAVRepositoryFactory.setup();
        this.url = url;

        try {
            this.repository = SVNRepositoryFactory.create(SVNURL.parseURIEncoded(url));
        } catch (SVNException e) {
            System.err
                    .println("error while creating an SVNRepository for the location '"
                            + url + "': " + e.getMessage());
        }

        ISVNAuthenticationManager authManager = SVNWCUtil.createDefaultAuthenticationManager();
        this.repository.setAuthenticationManager(authManager);
    }

    public SVNLogEntry latestLog(long startRevision, long endRevision) {
        Collection logEntries = null;

        try {
            logEntries = repository.log(new String[]{""}, null,
                                        startRevision, endRevision, true, true);

        } catch (SVNException e) {
            System.out.println("error while collecting log information for '"
                    + url + "': " + e.getMessage());
        }

        return (SVNLogEntry) logEntries.toArray()[0];
    }
}
