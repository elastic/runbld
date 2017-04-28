echo ':plugins:jvm-example:integTestCluster#init (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:jvm-example:integTestCluster#init' >&1
echo 'Skipping task '\'':plugins:jvm-example:integTestCluster#init'\'' as it has no actions.' >&1
echo ':plugins:jvm-example:integTestCluster#init (Thread[Daemon worker,5,main]) completed. Took 0.0 secs.' >&1
echo ':plugins:jvm-example:integTestCluster#wait (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:jvm-example:integTestCluster#wait' >&1
echo 'Putting task artifact state for task '\'':plugins:jvm-example:integTestCluster#wait'\'' into context took 0.0 secs.' >&1
echo 'Executing task '\'':plugins:jvm-example:integTestCluster#wait'\'' (up-to-date check took 0.0 secs) due to:' >&1
echo '  Task has not declared any outputs.' >&1
echo '[ant:echo] ==> [Fri Apr 28 09:57:38 UTC 2017] checking health: http://[::1]:45815/_cluster/health?wait_for_nodes=>=1&wait_for_status=yellow' >&1
echo '     [echo] ==> [Fri Apr 28 09:57:38 UTC 2017] checking health: http://[::1]:45815/_cluster/health?wait_for_nodes=>=1&wait_for_status=yellow' >&1
echo '[ant:get] Getting: http://[::1]:45815/_cluster/health?wait_for_nodes=>=1&wait_for_status=yellow' >&1
echo '      [get] Getting: http://[::1]:45815/_cluster/health?wait_for_nodes=>=1&wait_for_status=yellow' >&1
echo '[ant:get] To: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/jvm-example/build/cluster/integTestCluster node0/cwd/wait.success' >&1
echo '      [get] To: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/jvm-example/build/cluster/integTestCluster node0/cwd/wait.success' >&1
echo ':plugins:jvm-example:integTestCluster#wait (Thread[Daemon worker,5,main]) completed. Took 7.133 secs.' >&1
echo ':plugins:jvm-example:integTestRunner (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:jvm-example:integTestRunner' >&1
echo 'Putting task artifact state for task '\'':plugins:jvm-example:integTestRunner'\'' into context took 0.0 secs.' >&1
echo 'Executing task '\'':plugins:jvm-example:integTestRunner'\'' (up-to-date check took 0.0 secs) due to:' >&1
echo '  Task.upToDateWhen is false.' >&1
echo '[ant:junit4] <JUnit4> says Привет! Master seed: 3C85901804BE3487' >&1
echo '==> Test Info: seed=3C85901804BE3487; jvm=1; suites=2' >&1
echo 'Started J0 PID(11627@slave-0928d7475973c422a.build.us-west-2a.elasticnet.co).' >&1
echo 'Suite: org.elasticsearch.plugin.example.JvmExampleClientYamlTestSuiteIT' >&1
echo 'Completed [1/2] in 1.35s, 3 tests' >&1
echo '' >&1
echo 'Suite: org.elasticsearch.plugin.example.ExampleExternalIT' >&1
echo 'Completed [2/2] in 0.03s, 1 test' >&1
echo '' >&1
echo '[ant:junit4] JVM J0: stderr was not empty, see: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/jvm-example/build/testrun/integTestRunner/temp/junit4-J0-20170428_095738_5208867936412548933966.syserr' >&1
echo 'Slow Tests Summary:' >&1
echo '  1.35s | org.elasticsearch.plugin.example.JvmExampleClientYamlTestSuiteIT' >&1
echo '  0.03s | org.elasticsearch.plugin.example.ExampleExternalIT' >&1
echo '' >&1
echo '==> Test Summary: 2 suites, 4 tests' >&1
echo '[ant:junit4] JVM J0:     0.26 ..     3.02 =     2.76s' >&1
echo '[ant:junit4] Execution time total: 3.02 sec.' >&1
echo '[ant:junit4] Tests summary: 2 suites, 4 tests' >&1
echo 'Task :plugins:jvm-example:integTestRunner class loader hash: 9818fdd72fdbe3170a40dcbef630fc4c' >&1
echo 'Task :plugins:jvm-example:integTestRunner actions class loader hash: 93d98fe519353539b253a981d2cc58bc' >&1
echo ':plugins:jvm-example:integTestRunner (Thread[Daemon worker,5,main]) completed. Took 3.029 secs.' >&1
echo ':plugins:jvm-example:integTestCluster#stop (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:jvm-example:integTestCluster#stop' >&1
echo 'Putting task artifact state for task '\'':plugins:jvm-example:integTestCluster#stop'\'' into context took 0.0 secs.' >&1
echo 'Executing task '\'':plugins:jvm-example:integTestCluster#stop'\'' (up-to-date check took 0.0 secs) due to:' >&1
echo '  Task has not declared any outputs.' >&1
echo 'Shutting down external node with pid 11548' >&1
echo 'Starting process '\''command '\''kill'\'''\''. Working directory: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/jvm-example Command: kill -9 11548' >&1
echo 'Successfully started process '\''command '\''kill'\'''\''' >&1
echo ':plugins:jvm-example:integTestCluster#stop (Thread[Daemon worker,5,main]) completed. Took 0.004 secs.' >&1
echo ':plugins:jvm-example:exampleFixture#stop (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:jvm-example:exampleFixture#stop' >&1
echo 'Putting task artifact state for task '\'':plugins:jvm-example:exampleFixture#stop'\'' into context took 0.0 secs.' >&1
echo 'Executing task '\'':plugins:jvm-example:exampleFixture#stop'\'' (up-to-date check took 0.0 secs) due to:' >&1
echo '  Task has not declared any outputs.' >&1
echo 'Shutting down exampleFixture with pid 11495' >&1
echo 'Starting process '\''command '\''kill'\'''\''. Working directory: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/jvm-example Command: kill -9 11495' >&1
echo 'Successfully started process '\''command '\''kill'\'''\''' >&1
echo ':plugins:jvm-example:exampleFixture#stop (Thread[Daemon worker,5,main]) completed. Took 0.005 secs.' >&1
echo ':plugins:jvm-example:integTest (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:jvm-example:integTest' >&1
echo 'Putting task artifact state for task '\'':plugins:jvm-example:integTest'\'' into context took 0.0 secs.' >&1
echo 'Executing task '\'':plugins:jvm-example:integTest'\'' (up-to-date check took 0.0 secs) due to:' >&1
echo '  Task has not declared any outputs.' >&1
echo ':plugins:jvm-example:integTest (Thread[Daemon worker,5,main]) completed. Took 0.001 secs.' >&1
echo ':plugins:lang-javascript:integTestCluster#prepareCluster.cleanShared (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:lang-javascript:integTestCluster#prepareCluster.cleanShared' >&1
echo 'Putting task artifact state for task '\'':plugins:lang-javascript:integTestCluster#prepareCluster.cleanShared'\'' into context took 0.0 secs.' >&1
echo 'Executing task '\'':plugins:lang-javascript:integTestCluster#prepareCluster.cleanShared'\'' (up-to-date check took 0.0 secs) due to:' >&1
echo '  Task has not declared any outputs.' >&1
echo ':plugins:lang-javascript:integTestCluster#prepareCluster.cleanShared (Thread[Daemon worker,5,main]) completed. Took 0.0 secs.' >&1
echo ':plugins:lang-javascript:integTestCluster#clean (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:lang-javascript:integTestCluster#clean' >&1
echo 'Putting task artifact state for task '\'':plugins:lang-javascript:integTestCluster#clean'\'' into context took 0.0 secs.' >&1
echo 'Executing task '\'':plugins:lang-javascript:integTestCluster#clean'\'' (up-to-date check took 0.0 secs) due to:' >&1
echo '  Task has not declared any outputs.' >&1
echo ':plugins:lang-javascript:integTestCluster#clean (Thread[Daemon worker,5,main]) completed. Took 0.0 secs.' >&1
echo ':plugins:lang-javascript:integTestCluster#checkPrevious (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:lang-javascript:integTestCluster#checkPrevious' >&1
echo 'Skipping task '\'':plugins:lang-javascript:integTestCluster#checkPrevious'\'' as task onlyIf is false.' >&1
echo ':plugins:lang-javascript:integTestCluster#checkPrevious SKIPPED' >&1
echo ':plugins:lang-javascript:integTestCluster#checkPrevious (Thread[Daemon worker,5,main]) completed. Took 0.0 secs.' >&1
echo ':plugins:lang-javascript:integTestCluster#stopPrevious (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:lang-javascript:integTestCluster#stopPrevious' >&1
echo 'Skipping task '\'':plugins:lang-javascript:integTestCluster#stopPrevious'\'' as task onlyIf is false.' >&1
echo ':plugins:lang-javascript:integTestCluster#stopPrevious SKIPPED' >&1
echo ':plugins:lang-javascript:integTestCluster#stopPrevious (Thread[Daemon worker,5,main]) completed. Took 0.001 secs.' >&1
echo ':plugins:lang-javascript:integTestCluster#extract (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:lang-javascript:integTestCluster#extract' >&1
echo 'Putting task artifact state for task '\'':plugins:lang-javascript:integTestCluster#extract'\'' into context took 0.0 secs.' >&1
echo 'Task :plugins:lang-javascript:integTestCluster#extract class loader hash: 7064e588920e76a48571db060884e368' >&1
echo 'Task :plugins:lang-javascript:integTestCluster#extract actions class loader hash: e73b7abdd73d9346265974d08e88519c' >&1
echo 'Executing task '\'':plugins:lang-javascript:integTestCluster#extract'\'' (up-to-date check took 0.0 secs) due to:' >&1
echo '  No history is available.' >&1
echo ':plugins:lang-javascript:integTestCluster#extract (Thread[Daemon worker,5,main]) completed. Took 0.391 secs.' >&1
echo ':plugins:lang-javascript:integTestCluster#configure (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:lang-javascript:integTestCluster#configure' >&1
echo 'Putting task artifact state for task '\'':plugins:lang-javascript:integTestCluster#configure'\'' into context took 0.0 secs.' >&1
echo 'Executing task '\'':plugins:lang-javascript:integTestCluster#configure'\'' (up-to-date check took 0.0 secs) due to:' >&1
echo '  Task has not declared any outputs.' >&1
echo 'Configuring /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/cluster/integTestCluster node0/elasticsearch-5.5.0-SNAPSHOT/config/elasticsearch.yml' >&1
echo ':plugins:lang-javascript:integTestCluster#configure (Thread[Daemon worker,5,main]) completed. Took 0.0 secs.' >&1
echo ':plugins:lang-javascript:integTestCluster#copyPlugins (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:lang-javascript:integTestCluster#copyPlugins' >&1
echo 'Putting task artifact state for task '\'':plugins:lang-javascript:integTestCluster#copyPlugins'\'' into context took 0.0 secs.' >&1
echo 'Task :plugins:lang-javascript:integTestCluster#copyPlugins class loader hash: 7064e588920e76a48571db060884e368' >&1
echo 'Task :plugins:lang-javascript:integTestCluster#copyPlugins actions class loader hash: e73b7abdd73d9346265974d08e88519c' >&1
echo 'Executing task '\'':plugins:lang-javascript:integTestCluster#copyPlugins'\'' (up-to-date check took 0.0 secs) due to:' >&1
echo '  No history is available.' >&1
echo ':plugins:lang-javascript:integTestCluster#copyPlugins (Thread[Daemon worker,5,main]) completed. Took 0.007 secs.' >&1
echo ':plugins:lang-javascript:integTestCluster#installLangJavascriptPlugin (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:lang-javascript:integTestCluster#installLangJavascriptPlugin' >&1
echo 'Putting task artifact state for task '\'':plugins:lang-javascript:integTestCluster#installLangJavascriptPlugin'\'' into context took 0.0 secs.' >&1
echo 'Executing task '\'':plugins:lang-javascript:integTestCluster#installLangJavascriptPlugin'\'' (up-to-date check took 0.0 secs) due to:' >&1
echo '  Task has not declared any outputs.' >&1
echo 'Starting process '\''command '\''/var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/cluster/integTestCluster node0/elasticsearch-5.5.0-SNAPSHOT/bin/elasticsearch-plugin'\'''\''. Working directory: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/cluster/integTestCluster node0/cwd Command: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/cluster/integTestCluster node0/elasticsearch-5.5.0-SNAPSHOT/bin/elasticsearch-plugin install file:/var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/cluster/integTestCluster%20node0/plugins%20tmp/lang-javascript-5.5.0-SNAPSHOT.zip' >&1
echo 'Successfully started process '\''command '\''/var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/cluster/integTestCluster node0/elasticsearch-5.5.0-SNAPSHOT/bin/elasticsearch-plugin'\'''\''' >&1
echo 'Warning: Ignoring JAVA_TOOL_OPTIONS=-Dfile.encoding=UTF8' >&1
echo '-> Downloading file:/var/lib/jenkins/workspace/elastic elasticsearch 5.x multijob-intake/plugins/lang-javascript/build/cluster/integTestCluster node0/plugins tmp/lang-javascript-5.5.0-SNAPSHOT.zip' >&1
echo '' >&1
echo '[                                                 ] 1%   ' >&1
echo '[>                                                ] 2%   ' >&1
echo '[>                                                ] 3%   ' >&1
echo '[=>                                               ] 4%   ' >&1
echo '[=>                                               ] 5%   ' >&1
echo '[==>                                              ] 6%   ' >&1
echo '[==>                                              ] 7%   ' >&1
echo '[===>                                             ] 8%   ' >&1
echo '[===>                                             ] 9%   ' >&1
echo '[====>                                            ] 10%   ' >&1
echo '[====>                                            ] 11%   ' >&1
echo '[=====>                                           ] 12%   ' >&1
echo '[=====>                                           ] 13%   ' >&1
echo '[======>                                          ] 14%   ' >&1
echo '[======>                                          ] 15%   ' >&1
echo '[=======>                                         ] 16%   ' >&1
echo '[=======>                                         ] 17%   ' >&1
echo '[========>                                        ] 18%   ' >&1
echo '[========>                                        ] 19%   ' >&1
echo '[=========>                                       ] 20%   ' >&1
echo '[=========>                                       ] 21%   ' >&1
echo '[==========>                                      ] 22%   ' >&1
echo '[==========>                                      ] 23%   ' >&1
echo '[===========>                                     ] 24%   ' >&1
echo '[===========>                                     ] 25%   ' >&1
echo '[============>                                    ] 26%   ' >&1
echo '[============>                                    ] 27%   ' >&1
echo '[=============>                                   ] 28%   ' >&1
echo '[=============>                                   ] 29%   ' >&1
echo '[==============>                                  ] 30%   ' >&1
echo '[==============>                                  ] 31%   ' >&1
echo '[===============>                                 ] 32%   ' >&1
echo '[===============>                                 ] 33%   ' >&1
echo '[================>                                ] 34%   ' >&1
echo '[================>                                ] 35%   ' >&1
echo '[=================>                               ] 36%   ' >&1
echo '[=================>                               ] 37%   ' >&1
echo '[==================>                              ] 38%   ' >&1
echo '[==================>                              ] 39%   ' >&1
echo '[===================>                             ] 40%   ' >&1
echo '[===================>                             ] 41%   ' >&1
echo '[====================>                            ] 42%   ' >&1
echo '[====================>                            ] 43%   ' >&1
echo '[=====================>                           ] 44%   ' >&1
echo '[=====================>                           ] 45%   ' >&1
echo '[======================>                          ] 46%   ' >&1
echo '[======================>                          ] 47%   ' >&1
echo '[=======================>                         ] 48%   ' >&1
echo '[=======================>                         ] 49%   ' >&1
echo '[========================>                        ] 50%   ' >&1
echo '[========================>                        ] 51%   ' >&1
echo '[=========================>                       ] 52%   ' >&1
echo '[=========================>                       ] 53%   ' >&1
echo '[==========================>                      ] 54%   ' >&1
echo '[==========================>                      ] 55%   ' >&1
echo '[===========================>                     ] 56%   ' >&1
echo '[===========================>                     ] 57%   ' >&1
echo '[============================>                    ] 58%   ' >&1
echo '[============================>                    ] 59%   ' >&1
echo '[=============================>                   ] 60%   ' >&1
echo '[=============================>                   ] 61%   ' >&1
echo '[==============================>                  ] 62%   ' >&1
echo '[==============================>                  ] 63%   ' >&1
echo '[===============================>                 ] 64%   ' >&1
echo '[===============================>                 ] 65%   ' >&1
echo '[================================>                ] 66%   ' >&1
echo '[================================>                ] 67%   ' >&1
echo '[=================================>               ] 68%   ' >&1
echo '[=================================>               ] 69%   ' >&1
echo '[==================================>              ] 70%   ' >&1
echo '[==================================>              ] 71%   ' >&1
echo '[===================================>             ] 72%   ' >&1
echo '[===================================>             ] 73%   ' >&1
echo '[====================================>            ] 74%   ' >&1
echo '[====================================>            ] 75%   ' >&1
echo '[=====================================>           ] 76%   ' >&1
echo '[=====================================>           ] 77%   ' >&1
echo '[======================================>          ] 78%   ' >&1
echo '[======================================>          ] 79%   ' >&1
echo '[=======================================>         ] 80%   ' >&1
echo '[=======================================>         ] 81%   ' >&1
echo '[========================================>        ] 82%   ' >&1
echo '[========================================>        ] 83%   ' >&1
echo '[=========================================>       ] 84%   ' >&1
echo '[=========================================>       ] 85%   ' >&1
echo '[==========================================>      ] 86%   ' >&1
echo '[==========================================>      ] 87%   ' >&1
echo '[===========================================>     ] 88%   ' >&1
echo '[===========================================>     ] 89%   ' >&1
echo '[============================================>    ] 90%   ' >&1
echo '[============================================>    ] 91%   ' >&1
echo '[=============================================>   ] 92%   ' >&1
echo '[=============================================>   ] 93%   ' >&1
echo '[==============================================>  ] 94%   ' >&1
echo '[==============================================>  ] 95%   ' >&1
echo '[===============================================> ] 96%   ' >&1
echo '[===============================================> ] 97%   ' >&1
echo '[================================================>] 98%   ' >&1
echo '[================================================>] 99%   ' >&1
echo '[=================================================] 100%   ' >&1
echo '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@' >&1
echo '@     WARNING: plugin requires additional permissions     @' >&1
echo '@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@' >&1
echo '* java.lang.RuntimePermission createClassLoader' >&1
echo '* org.elasticsearch.script.ClassPermission <<STANDARD>>' >&1
echo '* org.elasticsearch.script.ClassPermission org.mozilla.javascript.ContextFactory' >&1
echo '* org.elasticsearch.script.ClassPermission org.mozilla.javascript.Callable' >&1
echo '* org.elasticsearch.script.ClassPermission org.mozilla.javascript.NativeFunction' >&1
echo '* org.elasticsearch.script.ClassPermission org.mozilla.javascript.Script' >&1
echo '* org.elasticsearch.script.ClassPermission org.mozilla.javascript.ScriptRuntime' >&1
echo '* org.elasticsearch.script.ClassPermission org.mozilla.javascript.Undefined' >&1
echo '* org.elasticsearch.script.ClassPermission org.mozilla.javascript.optimizer.OptRuntime' >&1
echo 'See http://docs.oracle.com/javase/8/docs/technotes/guides/security/permissions.html' >&1
echo 'for descriptions of what these permissions allow and the associated risks.' >&1
echo '-> Installed lang-javascript' >&1
echo ':plugins:lang-javascript:integTestCluster#installLangJavascriptPlugin (Thread[Daemon worker,5,main]) completed. Took 0.89 secs.' >&1
echo ':plugins:lang-javascript:integTestCluster#start (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:lang-javascript:integTestCluster#start' >&1
echo 'Putting task artifact state for task '\'':plugins:lang-javascript:integTestCluster#start'\'' into context took 0.0 secs.' >&1
echo 'Executing task '\'':plugins:lang-javascript:integTestCluster#start'\'' (up-to-date check took 0.0 secs) due to:' >&1
echo '  Task has not declared any outputs.' >&1
echo '' >&1
echo 'Node 0 configuration:' >&1
echo '|-----------------------------------------' >&1
echo '|  cwd: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/cluster/integTestCluster node0/cwd' >&1
echo '|  command: sh /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/cluster/integTestCluster node0/cwd/run -E node.portsfile=true -E path.conf=/var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/cluster/integTestCluster node0/elasticsearch-5.5.0-SNAPSHOT/config -E path.data=/var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/cluster/integTestCluster node0/elasticsearch-5.5.0-SNAPSHOT/data' >&1
echo '|  environment:' >&1
echo '|    JAVA_HOME: /usr/lib/jvm/java-1.8.0-openjdk-1.8.0.121-10.b14.fc25.x86_64' >&1
echo '|    ES_JAVA_OPTS:  -Xms512m -Xmx512m  -ea -esa' >&1
echo '|    ES_JVM_OPTIONS: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/cluster/integTestCluster node0/elasticsearch-5.5.0-SNAPSHOT/config/jvm.options' >&1
echo '|' >&1
echo '|  [run]' >&1
echo '    "/var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/cluster/integTestCluster node0/elasticsearch-5.5.0-SNAPSHOT/bin/elasticsearch" "$@" > run.log 2>&1 ; if [ $? != 0 ]; then touch run.failed; fi' >&1
echo '|' >&1
echo '|  [elasticsearch.yml]' >&1
echo '|    cluster.name: plugins_lang-javascript_integTestCluster' >&1
echo '|    pidfile: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/cluster/integTestCluster node0/es.pid' >&1
echo '|    path.repo: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/cluster/shared/repo' >&1
echo '|    path.shared_data: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/cluster/shared/' >&1
echo '|    node.attr.testattr: test' >&1
echo '|    repositories.url.allowed_urls: http://snapshot.test*' >&1
echo '|    node.max_local_storage_nodes: 1' >&1
echo '|    http.port: 0' >&1
echo '|    transport.tcp.port: 0' >&1
echo '|    cluster.routing.allocation.disk.watermark.low: 1b' >&1
echo '|    cluster.routing.allocation.disk.watermark.high: 1b' >&1
echo '|    script.inline: true' >&1
echo '|    script.stored: true' >&1
echo '|    script.max_compilations_per_minute: 1000' >&1
echo '|-----------------------------------------' >&1
echo ':plugins:lang-javascript:integTestCluster#start (Thread[Daemon worker,5,main]) completed. Took 0.011 secs.' >&1
echo ':plugins:lang-javascript:copyRestSpec (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:lang-javascript:copyRestSpec' >&1
echo 'Putting task artifact state for task '\'':plugins:lang-javascript:copyRestSpec'\'' into context took 0.0 secs.' >&1
echo 'Task :plugins:lang-javascript:copyRestSpec class loader hash: 7064e588920e76a48571db060884e368' >&1
echo 'Task :plugins:lang-javascript:copyRestSpec actions class loader hash: e73b7abdd73d9346265974d08e88519c' >&1
echo 'Executing task '\'':plugins:lang-javascript:copyRestSpec'\'' (up-to-date check took 0.0 secs) due to:' >&1
echo '  No history is available.' >&1
echo ':plugins:lang-javascript:copyRestSpec (Thread[Daemon worker,5,main]) completed. Took 0.042 secs.' >&1
echo ':plugins:lang-javascript:test (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:lang-javascript:test' >&1
echo 'Putting task artifact state for task '\'':plugins:lang-javascript:test'\'' into context took 0.001 secs.' >&1
echo 'Executing task '\'':plugins:lang-javascript:test'\'' (up-to-date check took 0.0 secs) due to:' >&1
echo '  Task.upToDateWhen is false.' >&1
echo '[ant:junit4] <JUnit4> says kaixo! Master seed: 81BD3653D0D996DF' >&1
echo '==> Test Info: seed=81BD3653D0D996DF; jvms=3; suites=3' >&1
echo 'Started J2 PID(11736@slave-0928d7475973c422a.build.us-west-2a.elasticnet.co).' >&1
echo 'Started J1 PID(11735@slave-0928d7475973c422a.build.us-west-2a.elasticnet.co).' >&1
echo 'Started J0 PID(11740@slave-0928d7475973c422a.build.us-west-2a.elasticnet.co).' >&1
echo 'Suite: org.elasticsearch.script.javascript.JavaScriptSecurityTests' >&1
echo 'Completed [1/3] on J2 in 4.30s, 3 tests' >&1
echo '' >&1
echo 'Suite: org.elasticsearch.script.javascript.JavaScriptScriptEngineTests' >&1
echo 'Completed [2/3] on J0 in 4.57s, 9 tests' >&1
echo '' >&1
echo '[ant:junit4] JVM J2: stderr was not empty, see: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/testrun/test/temp/junit4-J2-20170428_095742_9102843477036535498614.syserr' >&1
echo 'Suite: org.elasticsearch.script.javascript.JavaScriptScriptMultiThreadedTests' >&1
echo 'Completed [3/3] on J1 in 4.80s, 3 tests' >&1
echo '' >&1
echo '[ant:junit4] JVM J0: stderr was not empty, see: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/testrun/test/temp/junit4-J0-20170428_095742_9055212244773144137131.syserr' >&1
echo '[ant:junit4] JVM J1: stderr was not empty, see: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/testrun/test/temp/junit4-J1-20170428_095742_9053550885361473909085.syserr' >&1
echo 'Slow Tests Summary:' >&1
echo '  4.80s | org.elasticsearch.script.javascript.JavaScriptScriptMultiThreadedTests' >&1
echo '  4.57s | org.elasticsearch.script.javascript.JavaScriptScriptEngineTests' >&1
echo '  4.30s | org.elasticsearch.script.javascript.JavaScriptSecurityTests' >&1
echo '' >&1
echo '==> Test Summary: 3 suites, 15 tests' >&1
echo '[ant:junit4] JVM J0:     0.53 ..     6.30 =     5.77s' >&1
echo '[ant:junit4] JVM J1:     0.53 ..     6.31 =     5.78s' >&1
echo '[ant:junit4] JVM J2:     0.53 ..     6.05 =     5.52s' >&1
echo '[ant:junit4] Execution time total: 6.42 sec.' >&1
echo '[ant:junit4] Tests summary: 3 suites, 15 tests' >&1
echo 'Task :plugins:lang-javascript:test class loader hash: 9818fdd72fdbe3170a40dcbef630fc4c' >&1
echo 'Task :plugins:lang-javascript:test actions class loader hash: 1188279a24c351c6ea1e49ea5477e864' >&1
echo ':plugins:lang-javascript:test (Thread[Daemon worker,5,main]) completed. Took 6.438 secs.' >&1
echo ':plugins:lang-javascript:integTestCluster#init (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:lang-javascript:integTestCluster#init' >&1
echo 'Skipping task '\'':plugins:lang-javascript:integTestCluster#init'\'' as it has no actions.' >&1
echo ':plugins:lang-javascript:integTestCluster#init (Thread[Daemon worker,5,main]) completed. Took 0.0 secs.' >&1
echo ':plugins:lang-javascript:integTestCluster#wait (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:lang-javascript:integTestCluster#wait' >&1
echo 'Putting task artifact state for task '\'':plugins:lang-javascript:integTestCluster#wait'\'' into context took 0.0 secs.' >&1
echo 'Executing task '\'':plugins:lang-javascript:integTestCluster#wait'\'' (up-to-date check took 0.0 secs) due to:' >&1
echo '  Task has not declared any outputs.' >&1
echo '[ant:echo] ==> [Fri Apr 28 09:57:54 UTC 2017] checking health: http://[::1]:41941/_cluster/health?wait_for_nodes=>=1&wait_for_status=yellow' >&1
echo '     [echo] ==> [Fri Apr 28 09:57:54 UTC 2017] checking health: http://[::1]:41941/_cluster/health?wait_for_nodes=>=1&wait_for_status=yellow' >&1
echo '[ant:get] Getting: http://[::1]:41941/_cluster/health?wait_for_nodes=>=1&wait_for_status=yellow' >&1
echo '      [get] Getting: http://[::1]:41941/_cluster/health?wait_for_nodes=>=1&wait_for_status=yellow' >&1
echo '[ant:get] To: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/cluster/integTestCluster node0/cwd/wait.success' >&1
echo '      [get] To: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/cluster/integTestCluster node0/cwd/wait.success' >&1
echo ':plugins:lang-javascript:integTestCluster#wait (Thread[Daemon worker,5,main]) completed. Took 5.13 secs.' >&1
echo ':plugins:lang-javascript:integTestRunner (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:lang-javascript:integTestRunner' >&1
echo 'Putting task artifact state for task '\'':plugins:lang-javascript:integTestRunner'\'' into context took 0.0 secs.' >&1
echo 'Executing task '\'':plugins:lang-javascript:integTestRunner'\'' (up-to-date check took 0.0 secs) due to:' >&1
echo '  Task.upToDateWhen is false.' >&1
echo '[ant:junit4] <JUnit4> says jolly good day! Master seed: F8CECE851C7E60C7' >&1
echo '==> Test Info: seed=F8CECE851C7E60C7; jvm=1; suite=1' >&1
echo 'Started J0 PID(11900@slave-0928d7475973c422a.build.us-west-2a.elasticnet.co).' >&1
echo 'Suite: org.elasticsearch.script.javascript.LangJavascriptClientYamlTestSuiteIT' >&1
echo '  1> [2017-04-28T16:57:56,265][INFO ][o.e.s.j.LangJavascriptClientYamlTestSuiteIT] [test {yaml=lang_javascript/20_search/JavaScript Query}]: before test' >&1
echo '  1> [2017-04-28T16:57:56,286][INFO ][o.e.s.j.LangJavascriptClientYamlTestSuiteIT] initializing REST clients against [http://[::1]:41941]' >&1
echo '  1> [2017-04-28T16:57:57,006][INFO ][o.e.s.j.LangJavascriptClientYamlTestSuiteIT] initializing yaml client, minimum es version: [5.5.0] master version: [5.5.0] hosts: [http://[::1]:41941]' >&1
echo '  1> [2017-04-28T16:57:58,023][WARN ][o.e.c.RestClient         ] request [GET http://[::1]:41941/_search?error_trace=true] returned 1 warnings: [299 Elasticsearch-5.5.0-SNAPSHOT-7380a9d "[javascript] scripts are deprecated, use [painless] scripts instead" "Fri, 28 Apr 2017 09:57:57 GMT"]' >&1
echo '  1> [2017-04-28T16:57:58,197][INFO ][o.e.s.j.LangJavascriptClientYamlTestSuiteIT] [test {yaml=lang_javascript/20_search/JavaScript Query}]: after test' >&1
echo '  1> [2017-04-28T16:57:58,215][INFO ][o.e.s.j.LangJavascriptClientYamlTestSuiteIT] Stash dump on failure [{' >&1
echo '  1>   "stash" : {' >&1
echo '  1>     "body" : {' >&1
echo '  1>       "took" : 143,' >&1
echo '  1>       "timed_out" : false,' >&1
echo '  1>       "_shards" : {' >&1
echo '  1>         "total" : 5,' >&1
echo '  1>         "successful" : 3,' >&1
echo '  1>         "failed" : 2,' >&1
echo '  1>         "failures" : [' >&1
echo '  1>           {' >&1
echo '  1>             "shard" : 2,' >&1
echo '  1>             "index" : "test",' >&1
echo '  1>             "node" : "EnKs9B3dQgO-iSflKgxTkg",' >&1
echo '  1>             "reason" : {' >&1
echo '  1>               "type" : "null_pointer_exception",' >&1
echo '  1>               "reason" : null' >&1
echo '  1>             }' >&1
echo '  1>           }' >&1
echo '  1>         ]' >&1
echo '  1>       },' >&1
echo '  2> REPRODUCE WITH: gradle :plugins:lang-javascript:integTestRunner -Dtests.seed=F8CECE851C7E60C7 -Dtests.class=org.elasticsearch.script.javascript.LangJavascriptClientYamlTestSuiteIT -Dtests.method="test {yaml=lang_javascript/20_search/JavaScript Query}" -Dtests.security.manager=true -Dtests.locale=lt -Dtests.timezone=Asia/Ho_Chi_Minh' >&2
echo '  1>       "hits" : {' >&1
echo '  1>         "total" : 1,' >&1
echo '  1>         "max_score" : null,' >&1
echo '  1>         "hits" : [' >&1
echo '  1>           {' >&1
echo '  1>             "_index" : "test",' >&1
echo '  1>             "_type" : "test",' >&1
echo '  1>             "_id" : "3",' >&1
echo '  1>             "_score" : null,' >&1
echo '  1>             "fields" : {' >&1
echo '  1>               "sNum1" : [' >&1
echo '  1>                 3.0' >&1
echo '  1>               ]' >&1
echo '  1>             },' >&1
echo '  1>             "sort" : [' >&1
echo '  1>               3.0' >&1
echo '  1>             ]' >&1
echo '  1>           }' >&1
echo '  1>         ]' >&1
echo '  1>       }' >&1
echo '  1>     }' >&1
echo '  1>   }' >&1
echo '  1> }]' >&1
echo 'FAILURE 1.98s | LangJavascriptClientYamlTestSuiteIT.test {yaml=lang_javascript/20_search/JavaScript Query} <<< FAILURES!' >&1
echo '   > Throwable #1: java.lang.AssertionError: Failure at [lang_javascript/20_search:48]: hits.total didn'\''t match the expected value:' >&1
echo '   >                     hits.total: expected [2] but was [1]' >&1
echo '   > 	at __randomizedtesting.SeedInfo.seed([F8CECE851C7E60C7:709AF15FB2820D3F]:0)' >&1
echo '   > 	at org.elasticsearch.test.rest.yaml.ESClientYamlSuiteTestCase.executeSection(ESClientYamlSuiteTestCase.java:345)' >&1
echo '   > 	at org.elasticsearch.test.rest.yaml.ESClientYamlSuiteTestCase.test(ESClientYamlSuiteTestCase.java:325)' >&1
echo '   > 	at java.lang.Thread.run(Thread.java:745)' >&1
echo '   > Caused by: java.lang.AssertionError: hits.total didn'\''t match the expected value:' >&1
echo '   >                     hits.total: expected [2] but was [1]' >&1
echo '   > 	at org.elasticsearch.test.rest.yaml.section.MatchAssertion.doAssert(MatchAssertion.java:92)' >&1
echo '   > 	at org.elasticsearch.test.rest.yaml.section.Assertion.execute(Assertion.java:76)' >&1
echo '   > 	at org.elasticsearch.test.rest.yaml.ESClientYamlSuiteTestCase.executeSection(ESClientYamlSuiteTestCase.java:341)' >&1
echo '   > 	... 38 more' >&1
echo '  1> [2017-04-28T16:57:58,228][INFO ][o.e.s.j.LangJavascriptClientYamlTestSuiteIT] [test {yaml=lang_javascript/20_search/JavaScript Use List Length In Scripts}]: before test' >&1
echo '  1> [2017-04-28T16:57:58,442][WARN ][o.e.c.RestClient         ] request [POST http://[::1]:41941/_search?error_trace=true] returned 1 warnings: [299 Elasticsearch-5.5.0-SNAPSHOT-7380a9d "[javascript] scripts are deprecated, use [painless] scripts instead" "Fri, 28 Apr 2017 09:57:58 GMT"]' >&1
echo '  1> [2017-04-28T16:57:58,492][INFO ][o.e.s.j.LangJavascriptClientYamlTestSuiteIT] [test {yaml=lang_javascript/20_search/JavaScript Use List Length In Scripts}]: after test' >&1
echo '  1> [2017-04-28T16:57:58,498][INFO ][o.e.s.j.LangJavascriptClientYamlTestSuiteIT] [test {yaml=lang_javascript/20_search/JavaScript Scores Nested}]: before test' >&1
echo '  1> [2017-04-28T16:57:58,675][WARN ][o.e.c.RestClient         ] request [POST http://[::1]:41941/_search?error_trace=true] returned 1 warnings: [299 Elasticsearch-5.5.0-SNAPSHOT-7380a9d "[javascript] scripts are deprecated, use [painless] scripts instead" "Fri, 28 Apr 2017 09:57:58 GMT"]' >&1
echo '  1> [2017-04-28T16:57:58,711][INFO ][o.e.s.j.LangJavascriptClientYamlTestSuiteIT] [test {yaml=lang_javascript/20_search/JavaScript Scores Nested}]: after test' >&1
echo '  1> [2017-04-28T16:57:58,715][INFO ][o.e.s.j.LangJavascriptClientYamlTestSuiteIT] [test {yaml=lang_javascript/10_basic/Lang JavaScript}]: before test' >&1
echo '  1> [2017-04-28T16:57:58,916][WARN ][o.e.c.RestClient         ] request [POST http://[::1]:41941/_search?error_trace=true] returned 1 warnings: [299 Elasticsearch-5.5.0-SNAPSHOT-7380a9d "[javascript] scripts are deprecated, use [painless] scripts instead" "Fri, 28 Apr 2017 09:57:58 GMT"]' >&1
echo '  1> [2017-04-28T16:57:58,949][INFO ][o.e.s.j.LangJavascriptClientYamlTestSuiteIT] [test {yaml=lang_javascript/10_basic/Lang JavaScript}]: after test' >&1
echo '  1> [2017-04-28T16:57:58,954][INFO ][o.e.s.j.LangJavascriptClientYamlTestSuiteIT] [test {yaml=lang_javascript/20_search/JavaScript Custom Script Boost}]: before test' >&1
echo '  1> [2017-04-28T16:57:59,133][WARN ][o.e.c.RestClient         ] request [POST http://[::1]:41941/_search?error_trace=true] returned 1 warnings: [299 Elasticsearch-5.5.0-SNAPSHOT-7380a9d "[javascript] scripts are deprecated, use [painless] scripts instead" "Fri, 28 Apr 2017 09:57:59 GMT"]' >&1
echo '  2> NOTE: leaving temporary files on disk at: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/testrun/integTestRunner/J0/temp/org.elasticsearch.script.javascript.LangJavascriptClientYamlTestSuiteIT_F8CECE851C7E60C7-001' >&2
echo '  1> [2017-04-28T16:57:59,145][WARN ][o.e.c.RestClient         ] request [GET http://[::1]:41941/_search?error_trace=true] returned 1 warnings: [299 Elasticsearch-5.5.0-SNAPSHOT-7380a9d "[javascript] scripts are deprecated, use [painless] scripts instead" "Fri, 28 Apr 2017 09:57:59 GMT"]' >&1
echo '  2> NOTE: test params are: codec=Asserting(Lucene62): {}, docValues:{}, maxPointsInLeafNode=804, maxMBSortInHeap=5.931759384459007, sim=RandomSimilarity(queryNorm=true,coord=crazy): {}, locale=lt, timezone=Asia/Ho_Chi_Minh' >&2
echo '  2> NOTE: Linux 4.8.6-300.fc25.x86_64 amd64/Oracle Corporation 1.8.0_121 (64-bit)/cpus=4,threads=1,free=424462344,total=514850816' >&2
echo '  2> NOTE: All tests run in this JVM: [LangJavascriptClientYamlTestSuiteIT]' >&2
echo '  1> [2017-04-28T16:57:59,155][WARN ][o.e.c.RestClient         ] request [GET http://[::1]:41941/_search?error_trace=true] returned 1 warnings: [299 Elasticsearch-5.5.0-SNAPSHOT-7380a9d "[javascript] scripts are deprecated, use [painless] scripts instead" "Fri, 28 Apr 2017 09:57:59 GMT"]' >&1
echo '  1> [2017-04-28T16:57:59,175][WARN ][o.e.c.RestClient         ] request [POST http://[::1]:41941/_search?error_trace=true] returned 1 warnings: [299 Elasticsearch-5.5.0-SNAPSHOT-7380a9d "[javascript] scripts are deprecated, use [painless] scripts instead" "Fri, 28 Apr 2017 09:57:59 GMT"]' >&1
echo '  1> [2017-04-28T16:57:59,188][WARN ][o.e.c.RestClient         ] request [POST http://[::1]:41941/_search?error_trace=true] returned 1 warnings: [299 Elasticsearch-5.5.0-SNAPSHOT-7380a9d "[javascript] scripts are deprecated, use [painless] scripts instead" "Fri, 28 Apr 2017 09:57:59 GMT"]' >&1
echo '  1> [2017-04-28T16:57:59,202][WARN ][o.e.c.RestClient         ] request [GET http://[::1]:41941/_search?error_trace=true] returned 1 warnings: [299 Elasticsearch-5.5.0-SNAPSHOT-7380a9d "[javascript] scripts are deprecated, use [painless] scripts instead" "Fri, 28 Apr 2017 09:57:59 GMT"]' >&1
echo '  1> [2017-04-28T16:57:59,254][INFO ][o.e.s.j.LangJavascriptClientYamlTestSuiteIT] [test {yaml=lang_javascript/20_search/JavaScript Custom Script Boost}]: after test' >&1
echo '  1> [2017-04-28T16:57:59,258][INFO ][o.e.s.j.LangJavascriptClientYamlTestSuiteIT] [test {yaml=lang_javascript/20_search/JavaScript Scores With Agg}]: before test' >&1
echo '  1> [2017-04-28T16:57:59,500][WARN ][o.e.c.RestClient         ] request [POST http://[::1]:41941/_search?error_trace=true] returned 1 warnings: [299 Elasticsearch-5.5.0-SNAPSHOT-7380a9d "[javascript] scripts are deprecated, use [painless] scripts instead" "Fri, 28 Apr 2017 09:57:59 GMT"]' >&1
echo '  1> [2017-04-28T16:57:59,554][INFO ][o.e.s.j.LangJavascriptClientYamlTestSuiteIT] [test {yaml=lang_javascript/20_search/JavaScript Scores With Agg}]: after test' >&1
echo '  1> [2017-04-28T16:57:59,559][INFO ][o.e.s.j.LangJavascriptClientYamlTestSuiteIT] [test {yaml=lang_javascript/20_search/JavaScript Script Field Using Source}]: before test' >&1
echo '  1> [2017-04-28T16:57:59,707][WARN ][o.e.c.RestClient         ] request [POST http://[::1]:41941/_search?error_trace=true] returned 1 warnings: [299 Elasticsearch-5.5.0-SNAPSHOT-7380a9d "[javascript] scripts are deprecated, use [painless] scripts instead" "Fri, 28 Apr 2017 09:57:59 GMT"]' >&1
echo '  1> [2017-04-28T16:57:59,741][INFO ][o.e.s.j.LangJavascriptClientYamlTestSuiteIT] [test {yaml=lang_javascript/20_search/JavaScript Script Field Using Source}]: after test' >&1
echo 'Completed [1/1] in 3.90s, 7 tests, 1 failure <<< FAILURES!' >&1
echo '' >&1
echo '[ant:junit4] JVM J0: stderr was not empty, see: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/testrun/integTestRunner/temp/junit4-J0-20170428_095754_4725756721216402836940.syserr' >&1
echo 'Tests with failures:' >&1
echo '  - org.elasticsearch.script.javascript.LangJavascriptClientYamlTestSuiteIT.test {yaml=lang_javascript/20_search/JavaScript Query}' >&1
echo '' >&1
echo 'Slow Tests Summary:' >&1
echo '  3.90s | org.elasticsearch.script.javascript.LangJavascriptClientYamlTestSuiteIT' >&1
echo '' >&1
echo '[ant:junit4] JVM J0:     0.26 ..     5.80 =     5.53s' >&1
echo '[ant:junit4] Execution time total: 5.80 sec.' >&1
echo '[ant:junit4] Tests summary: 1 suite, 7 tests, 1 failure' >&1
echo ':plugins:lang-javascript:integTestRunner FAILED' >&1
echo '' >&1
echo 'Cluster plugins_lang-javascript_integTestCluster - node 0 log excerpt:' >&1
echo '(full log at /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript/build/cluster/integTestCluster node0/elasticsearch-5.5.0-SNAPSHOT/logs/plugins_lang-javascript_integTestCluster.log)' >&1
echo '-----------------------------------------' >&1
echo '[2017-04-28T09:57:46,144][INFO ][o.e.n.Node               ] [] initializing ...' >&1
echo '[2017-04-28T09:57:46,317][INFO ][o.e.e.NodeEnvironment    ] [EnKs9B3] using [1] data paths, mounts [[/ (/dev/xvda1)]], net usable_space [361.1gb], net total_space [492gb], spins? [no], types [ext4]' >&1
echo '[2017-04-28T09:57:46,318][INFO ][o.e.e.NodeEnvironment    ] [EnKs9B3] heap size [494.9mb], compressed ordinary object pointers [true]' >&1
echo '[2017-04-28T09:57:46,320][INFO ][o.e.n.Node               ] node name [EnKs9B3] derived from node ID [EnKs9B3dQgO-iSflKgxTkg]; set [node.name] to override' >&1
echo '[2017-04-28T09:57:46,322][INFO ][o.e.n.Node               ] version[5.5.0-SNAPSHOT], pid[11713], build[7380a9d/2017-04-28T09:15:19.421Z], OS[Linux/4.8.6-300.fc25.x86_64/amd64], JVM[Oracle Corporation/OpenJDK 64-Bit Server VM/1.8.0_121/25.121-b14]' >&1
echo '[2017-04-28T09:57:46,322][WARN ][o.e.n.Node               ] version [5.5.0-SNAPSHOT] is a pre-release version of Elasticsearch and is not suitable for production' >&1
echo '[2017-04-28T09:57:47,297][INFO ][o.e.p.PluginsService     ] [EnKs9B3] loaded module [transport-netty3]' >&1
echo '[2017-04-28T09:57:47,298][INFO ][o.e.p.PluginsService     ] [EnKs9B3] loaded module [transport-netty4]' >&1
echo '[2017-04-28T09:57:47,303][INFO ][o.e.p.PluginsService     ] [EnKs9B3] loaded plugin [lang-javascript]' >&1
echo '[2017-04-28T09:57:50,090][INFO ][o.e.d.DiscoveryModule    ] [EnKs9B3] using discovery type [zen]' >&1
echo '[2017-04-28T09:57:50,564][INFO ][o.e.n.Node               ] initialized' >&1
echo '[2017-04-28T09:57:50,565][INFO ][o.e.n.Node               ] [EnKs9B3] starting ...' >&1
echo '[2017-04-28T09:57:50,744][INFO ][o.e.t.TransportService   ] [EnKs9B3] publish_address {127.0.0.1:38881}, bound_addresses {[::1]:37457}, {127.0.0.1:38881}' >&1
echo '[2017-04-28T09:57:53,828][INFO ][o.e.c.s.ClusterService   ] [EnKs9B3] new_master {EnKs9B3}{EnKs9B3dQgO-iSflKgxTkg}{X6lxUpLnT_KhSIa5TXUxXQ}{127.0.0.1}{127.0.0.1:38881}{testattr=test}, reason: zen-disco-elected-as-master ([0] nodes joined)' >&1
echo '[2017-04-28T09:57:53,848][INFO ][o.e.h.n.Netty4HttpServerTransport] [EnKs9B3] publish_address {127.0.0.1:40127}, bound_addresses {[::1]:41941}, {127.0.0.1:40127}' >&1
echo '[2017-04-28T09:57:53,854][INFO ][o.e.n.Node               ] [EnKs9B3] started' >&1
echo '[2017-04-28T09:57:53,858][INFO ][o.e.g.GatewayService     ] [EnKs9B3] recovered [0] indices into cluster_state' >&1
echo '=========================================' >&1
echo ':plugins:lang-javascript:integTestRunner (Thread[Daemon worker,5,main]) completed. Took 5.83 secs.' >&1
echo ':plugins:lang-javascript:integTestCluster#stop (Thread[Daemon worker,5,main]) started.' >&1
echo ':plugins:lang-javascript:integTestCluster#stop' >&1
echo 'Putting task artifact state for task '\'':plugins:lang-javascript:integTestCluster#stop'\'' into context took 0.0 secs.' >&1
echo 'Executing task '\'':plugins:lang-javascript:integTestCluster#stop'\'' (up-to-date check took 0.0 secs) due to:' >&1
echo '  Task has not declared any outputs.' >&1
echo 'Shutting down external node with pid 11713' >&1
echo 'Starting process '\''command '\''kill'\'''\''. Working directory: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/plugins/lang-javascript Command: kill -9 11713' >&1
echo 'Successfully started process '\''command '\''kill'\'''\''' >&1
echo ':plugins:lang-javascript:integTestCluster#stop (Thread[Daemon worker,5,main]) completed. Took 0.025 secs.' >&1
echo ':docs:integTestCluster#stop (Thread[Daemon worker,5,main]) started.' >&1
echo ':docs:integTestCluster#stop' >&1
echo 'Putting task artifact state for task '\'':docs:integTestCluster#stop'\'' into context took 0.0 secs.' >&1
echo 'Executing task '\'':docs:integTestCluster#stop'\'' (up-to-date check took 0.0 secs) due to:' >&1
echo '  Task has not declared any outputs.' >&1
echo 'Shutting down external node with pid 30442' >&1
echo 'Starting process '\''command '\''kill'\'''\''. Working directory: /var/lib/jenkins/workspace/elastic+elasticsearch+5.x+multijob-intake/docs Command: kill -9 30442' >&1
echo 'Successfully started process '\''command '\''kill'\'''\''' >&1
echo ':docs:integTestCluster#stop (Thread[Daemon worker,5,main]) completed. Took 0.011 secs.' >&1
echo '' >&2
echo 'FAILURE: Build failed with an exception.' >&2
echo '' >&2
echo '* What went wrong:' >&2
echo 'Execution failed for task '\'':plugins:lang-javascript:integTestRunner'\''.' >&2
echo '> There were test failures: 1 suite, 7 tests, 1 failure [seed: F8CECE851C7E60C7]' >&2
echo '' >&2
echo '* Try:' >&2
echo 'Run with --stacktrace option to get the stack trace. Run with --debug option to get more log output.' >&2
echo '' >&1
echo 'BUILD FAILED' >&1
echo '' >&1
echo 'Total time: 43 mins 30.33 secs' >&1
echo 'Stopped 1 worker daemon(s).' >&1
echo 'Received result Failure[value=org.gradle.initialization.ReportedException: org.gradle.internal.exceptions.LocationAwareException: Execution failed for task '\'':plugins:lang-javascript:integTestRunner'\''.] from daemon DaemonInfo{pid=3936, address=[972cf613-f478-438a-91aa-41253220165f port:45459, addresses:[/0:0:0:0:0:0:0:1, /127.0.0.1]], state=Busy, lastBusy=1493370871515, context=DefaultDaemonContext[uid=63446486-490e-4e36-9c4e-1b26e9a09e78,javaHome=/usr/lib/jvm/java-1.8.0-openjdk-1.8.0.121-10.b14.fc25.x86_64,daemonRegistryDir=/var/lib/jenkins/.gradle/daemon,pid=3936,idleTimeout=120000,daemonOpts=-XX:MaxPermSize=256m,-XX:+HeapDumpOnOutOfMemoryError,-Xmx1024m,-Dfile.encoding=UTF8,-Duser.country=US,-Duser.language=en,-Duser.variant]} (build should be done).' >&1
exit /b 1

