Apache Maven 3.2.3 (33f8c3e1027c3ddde99d3cdebad2656a31e8fdf4; 2014-08-11T15:58:10-05:00)
Maven home: C:\Program Files\Apache Software Foundation\apache-maven-3.2.3
Java version: 1.7.0_45, vendor: Oracle Corporation
Java home: C:\Program Files\Java\jdk1.7.0_45\jre
Default locale: en_US, platform encoding: Cp1252
OS name: "windows 7", version: "6.1", arch: "amd64", family: "windows"
[INFO] Error stacktraces are turned on.
[DEBUG] Reading global settings from C:\Program Files\Apache Software Foundation\apache-maven-3.2.3\conf\settings.xml
[DEBUG] Reading user settings from C:\Users\MAveces\.m2\settings.xml
[DEBUG] Using local repository at C:\Users\MAveces\.m2\repository
[DEBUG] Using manager EnhancedLocalRepositoryManager with priority 10.0 for C:\Users\MAveces\.m2\repository
[INFO] Scanning for projects...
[DEBUG] Extension realms for project Jppf-5.0-Test:Jppf-5.0-Test:jar:0.0.1-SNAPSHOT: (none)
[DEBUG] Looking up lifecyle mappings for packaging jar from ClassRealm[plexus.core, parent: null]
[DEBUG] Resolving plugin prefix exec from [org.apache.maven.plugins, org.codehaus.mojo]
[DEBUG] Resolved plugin prefix exec to org.codehaus.mojo:exec-maven-plugin from POM Jppf-5.0-Test:Jppf-5.0-Test:jar:0.0.1-SNAPSHOT
[DEBUG] === REACTOR BUILD PLAN ================================================
[DEBUG] Project: Jppf-5.0-Test:Jppf-5.0-Test:jar:0.0.1-SNAPSHOT
[DEBUG] Tasks:   [exec:java]
[DEBUG] Style:   Regular
[DEBUG] =======================================================================
[INFO]                                                                         
[INFO] ------------------------------------------------------------------------
[INFO] Building jppf-5.0-Test 0.0.1-SNAPSHOT
[INFO] ------------------------------------------------------------------------
[DEBUG] Resolving plugin prefix exec from [org.apache.maven.plugins, org.codehaus.mojo]
[DEBUG] Resolved plugin prefix exec to org.codehaus.mojo:exec-maven-plugin from POM Jppf-5.0-Test:Jppf-5.0-Test:jar:0.0.1-SNAPSHOT
[DEBUG] Lifecycle default -> [validate, initialize, generate-sources, process-sources, generate-resources, process-resources, compile, process-classes, generate-test-sources, process-test-sources, generate-test-resources, process-test-resources, test-compile, process-test-classes, test, prepare-package, package, pre-integration-test, integration-test, post-integration-test, verify, install, deploy]
[DEBUG] Lifecycle clean -> [pre-clean, clean, post-clean]
[DEBUG] Lifecycle site -> [pre-site, site, post-site, site-deploy]
[DEBUG] Lifecycle default -> [validate, initialize, generate-sources, process-sources, generate-resources, process-resources, compile, process-classes, generate-test-sources, process-test-sources, generate-test-resources, process-test-resources, test-compile, process-test-classes, test, prepare-package, package, pre-integration-test, integration-test, post-integration-test, verify, install, deploy]
[DEBUG] Lifecycle clean -> [pre-clean, clean, post-clean]
[DEBUG] Lifecycle site -> [pre-site, site, post-site, site-deploy]
[DEBUG] === PROJECT BUILD PLAN ================================================
[DEBUG] Project:       Jppf-5.0-Test:Jppf-5.0-Test:0.0.1-SNAPSHOT
[DEBUG] Dependencies (collect): []
[DEBUG] Dependencies (resolve): [test]
[DEBUG] Repositories (dependencies): [CentralMaven (http://repo1.maven.org/maven2, releases+snapshots), Central Maven 2 (http://central.maven.org/maven2/, releases+snapshots), maven2-repository.dev.java.net (http://download.java.net/maven/2, releases+snapshots), SpringRepo (http://repo.spring.io/release, releases+snapshots), spring-milestones (http://repo.spring.io/milestone, releases), Jboss 1 (http://repository.jboss.org/nexus/content/groups/public-jboss/, releases+snapshots), Jboss 2 (https://repository.jboss.org/nexus/content/repositories/thirdparty-releases, releases+snapshots), Apache Repository (https://repository.apache.org/content/groups/public/, releases+snapshots), central (https://repo.maven.apache.org/maven2, releases)]
[DEBUG] Repositories (plugins)     : [central (https://repo.maven.apache.org/maven2, releases)]
[DEBUG] --- init fork of Jppf-5.0-Test:Jppf-5.0-Test:0.0.1-SNAPSHOT for org.codehaus.mojo:exec-maven-plugin:1.2.1:java (default-cli) ---
[DEBUG] Dependencies (collect): []
[DEBUG] Dependencies (resolve): []
[DEBUG] --- exit fork of Jppf-5.0-Test:Jppf-5.0-Test:0.0.1-SNAPSHOT for org.codehaus.mojo:exec-maven-plugin:1.2.1:java (default-cli) ---
[DEBUG] -----------------------------------------------------------------------
[DEBUG] Goal:          org.codehaus.mojo:exec-maven-plugin:1.2.1:java (default-cli)
[DEBUG] Style:         Regular
[DEBUG] Configuration: <?xml version="1.0" encoding="UTF-8"?>
<configuration>
  <arguments>
    <argument>-Djppf.config=C:\\Users\\MAveces\\Documents\\NetBeansProjects\\JppfTest\\Jppf-5.0-Test\\configjppf.properties</argument>
    <argument>-Djava.util.logging.config.file=C:\\Users\\MAveces\\Documents\\NetBeansProjects\\JppfTest\\Jppf-5.0-Test\\configlogging.properties</argument>
    <argument>-Xmx64m</argument>${exec.arguments}</arguments>
  <classpathScope default-value="runtime">${exec.classpathScope}</classpathScope>
  <cleanupDaemonThreads>${exec.cleanupDaemonThreads} default-value=</cleanupDaemonThreads>
  <commandlineArgs>${exec.args}</commandlineArgs>
  <daemonThreadJoinTimeout default-value="15000">${exec.daemonThreadJoinTimeout}</daemonThreadJoinTimeout>
  <includePluginDependencies default-value="false">${exec.includePluginDependencies}</includePluginDependencies>
  <includeProjectDependencies default-value="true">${exec.includeProjectDependencies}</includeProjectDependencies>
  <keepAlive default-value="false">${exec.keepAlive}</keepAlive>
  <killAfter default-value="-1">${exec.killAfter}</killAfter>
  <localRepository default-value="${localRepository}"/>
  <mainClass>org.jppf.application.template.TemplateApplicationRunner</mainClass>
  <pluginDependencies default-value="${plugin.artifacts}"/>
  <project default-value="${project}"/>
  <remoteRepositories default-value="${project.remoteArtifactRepositories}"/>
  <skip default-value="false">${skip}</skip>
  <sourceRoot>${sourceRoot}</sourceRoot>
  <stopUnresponsiveDaemonThreads>${exec.stopUnresponsiveDaemonThreads} default-value=</stopUnresponsiveDaemonThreads>
  <testSourceRoot>${testSourceRoot}</testSourceRoot>
</configuration>
[DEBUG] =======================================================================
[INFO] 
[INFO] >>> exec-maven-plugin:1.2.1:java (default-cli) > validate @ Jppf-5.0-Test >>>
[INFO] 
[INFO] <<< exec-maven-plugin:1.2.1:java (default-cli) < validate @ Jppf-5.0-Test <<<
[DEBUG] Dependency collection stats: {ConflictMarker.analyzeTime=0, ConflictMarker.markTime=1, ConflictMarker.nodeCount=27, ConflictIdSorter.graphTime=0, ConflictIdSorter.topsortTime=0, ConflictIdSorter.conflictIdCount=14, ConflictIdSorter.conflictIdCycleCount=0, ConflictResolver.totalTime=3, ConflictResolver.conflictItemCount=22, DefaultDependencyCollector.collectTime=87, DefaultDependencyCollector.transformTime=5}
[DEBUG] Jppf-5.0-Test:Jppf-5.0-Test:jar:0.0.1-SNAPSHOT
[DEBUG]    org.slf4j:slf4j-api:jar:1.6.1:compile
[DEBUG]    org.slf4j:log4j-over-slf4j:jar:1.7.7:compile
[DEBUG]    ch.qos.logback:logback-classic:jar:1.1.2:compile
[DEBUG]    ch.qos.logback:logback-core:jar:1.1.2:compile
[DEBUG]    junit:junit:jar:4.11:compile
[DEBUG]       org.hamcrest:hamcrest-core:jar:1.3:compile
[DEBUG]    org.slf4j:slf4j-log4j12:jar:1.6.1:compile
[DEBUG]       log4j:log4j:jar:1.2.16:compile
[DEBUG]    org.jppf:jppf-common:jar:5.0:compile
[DEBUG]    commons-io:commons-io:jar:2.4:compile
[DEBUG]    org.jppf:jppf-jmxremote_optional:jar:1.1:compile
[DEBUG]    org.jppf:jppf-client:jar:5.0:compile
[INFO] 
[INFO] --- exec-maven-plugin:1.2.1:java (default-cli) @ Jppf-5.0-Test ---
[DEBUG] Created new class realm maven.api
[DEBUG] Importing foreign packages into class realm maven.api
[DEBUG]   Imported: org.apache.maven.wagon.events < plexus.core
[DEBUG]   Imported: org.eclipse.aether.impl < plexus.core
[DEBUG]   Imported: org.apache.maven.exception < plexus.core
[DEBUG]   Imported: org.codehaus.plexus.util.xml.Xpp3Dom < plexus.core
[DEBUG]   Imported: org.eclipse.aether.version < plexus.core
[DEBUG]   Imported: org.eclipse.aether.metadata < plexus.core
[DEBUG]   Imported: javax.enterprise.util.* < plexus.core
[DEBUG]   Imported: org.eclipse.aether.collection < plexus.core
[DEBUG]   Imported: org.apache.maven.monitor < plexus.core
[DEBUG]   Imported: org.apache.maven.wagon.repository < plexus.core
[DEBUG]   Imported: org.apache.maven.repository < plexus.core
[DEBUG]   Imported: org.apache.maven.wagon.resource < plexus.core
[DEBUG]   Imported: org.codehaus.plexus.logging < plexus.core
[DEBUG]   Imported: org.apache.maven.profiles < plexus.core
[DEBUG]   Imported: org.apache.maven.classrealm < plexus.core
[DEBUG]   Imported: org.apache.maven.execution.scope < plexus.core
[DEBUG]   Imported: org.eclipse.aether.artifact < plexus.core
[DEBUG]   Imported: org.apache.maven.execution < plexus.core
[DEBUG]   Imported: org.apache.maven.reporting < plexus.core
[DEBUG]   Imported: org.apache.maven.usability < plexus.core
[DEBUG]   Imported: org.codehaus.plexus.container < plexus.core
[DEBUG]   Imported: org.codehaus.plexus.component < plexus.core
[DEBUG]   Imported: org.eclipse.aether.transfer < plexus.core
[DEBUG]   Imported: org.apache.maven.wagon.authentication < plexus.core
[DEBUG]   Imported: org.codehaus.plexus.util.xml.pull.XmlSerializer < plexus.core
[DEBUG]   Imported: org.apache.maven.lifecycle < plexus.core
[DEBUG]   Imported: org.eclipse.aether.* < plexus.core
[DEBUG]   Imported: org.eclipse.aether.graph < plexus.core
[DEBUG]   Imported: org.codehaus.plexus.classworlds < plexus.core
[DEBUG]   Imported: org.eclipse.aether.internal.impl < plexus.core
[DEBUG]   Imported: org.eclipse.aether.repository < plexus.core
[DEBUG]   Imported: org.eclipse.aether.resolution < plexus.core
[DEBUG]   Imported: javax.inject.* < plexus.core
[DEBUG]   Imported: org.apache.maven.settings < plexus.core
[DEBUG]   Imported: org.codehaus.classworlds < plexus.core
[DEBUG]   Imported: org.apache.maven.wagon.* < plexus.core
[DEBUG]   Imported: org.apache.maven.toolchain < plexus.core
[DEBUG]   Imported: org.eclipse.aether.spi < plexus.core
[DEBUG]   Imported: org.apache.maven.wagon.observers < plexus.core
[DEBUG]   Imported: org.codehaus.plexus.util.xml.pull.XmlPullParserException < plexus.core
[DEBUG]   Imported: org.codehaus.plexus.util.xml.pull.XmlPullParser < plexus.core
[DEBUG]   Imported: org.apache.maven.configuration < plexus.core
[DEBUG]   Imported: org.apache.maven.cli < plexus.core
[DEBUG]   Imported: org.codehaus.plexus.context < plexus.core
[DEBUG]   Imported: org.apache.maven.wagon.authorization < plexus.core
[DEBUG]   Imported: org.apache.maven.project < plexus.core
[DEBUG]   Imported: org.eclipse.aether.installation < plexus.core
[DEBUG]   Imported: org.eclipse.aether.deployment < plexus.core
[DEBUG]   Imported: org.codehaus.plexus.lifecycle < plexus.core
[DEBUG]   Imported: org.apache.maven.rtinfo < plexus.core
[DEBUG]   Imported: org.codehaus.plexus.configuration < plexus.core
[DEBUG]   Imported: org.apache.maven.artifact < plexus.core
[DEBUG]   Imported: org.apache.maven.model < plexus.core
[DEBUG]   Imported: org.slf4j.* < plexus.core
[DEBUG]   Imported: javax.enterprise.inject.* < plexus.core
[DEBUG]   Imported: org.apache.maven.* < plexus.core
[DEBUG]   Imported: org.apache.maven.wagon.proxy < plexus.core
[DEBUG]   Imported: org.codehaus.plexus.* < plexus.core
[DEBUG]   Imported: org.apache.maven.plugin < plexus.core
[DEBUG]   Imported: org.codehaus.plexus.personality < plexus.core
[DEBUG] Populating class realm maven.api
[DEBUG] Dependency collection stats: {ConflictMarker.analyzeTime=0, ConflictMarker.markTime=1, ConflictMarker.nodeCount=68, ConflictIdSorter.graphTime=0, ConflictIdSorter.topsortTime=0, ConflictIdSorter.conflictIdCount=25, ConflictIdSorter.conflictIdCycleCount=0, ConflictResolver.totalTime=1, ConflictResolver.conflictItemCount=65, DefaultDependencyCollector.collectTime=117, DefaultDependencyCollector.transformTime=2}
[DEBUG] org.codehaus.mojo:exec-maven-plugin:jar:1.2.1:
[DEBUG]    org.apache.maven:maven-toolchain:jar:1.0:compile
[DEBUG]    org.apache.maven:maven-project:jar:2.0.6:compile
[DEBUG]       org.apache.maven:maven-settings:jar:2.0.6:compile
[DEBUG]       org.apache.maven:maven-profile:jar:2.0.6:compile
[DEBUG]       org.apache.maven:maven-plugin-registry:jar:2.0.6:compile
[DEBUG]    org.apache.maven:maven-model:jar:2.0.6:compile
[DEBUG]    org.apache.maven:maven-artifact:jar:2.0.6:compile
[DEBUG]    org.apache.maven:maven-artifact-manager:jar:2.0.6:compile
[DEBUG]       org.apache.maven:maven-repository-metadata:jar:2.0.6:compile
[DEBUG]    org.apache.maven:maven-core:jar:2.0.6:compile
[DEBUG]       org.apache.maven:maven-plugin-parameter-documenter:jar:2.0.6:compile
[DEBUG]       org.apache.maven.reporting:maven-reporting-api:jar:2.0.6:compile
[DEBUG]          org.apache.maven.doxia:doxia-sink-api:jar:1.0-alpha-7:compile
[DEBUG]       org.apache.maven:maven-error-diagnostics:jar:2.0.6:compile
[DEBUG]       commons-cli:commons-cli:jar:1.0:compile
[DEBUG]       org.apache.maven:maven-plugin-descriptor:jar:2.0.6:compile
[DEBUG]       org.codehaus.plexus:plexus-interactivity-api:jar:1.0-alpha-4:compile
[DEBUG]       org.apache.maven:maven-monitor:jar:2.0.6:compile
[DEBUG]       classworlds:classworlds:jar:1.1:compile
[DEBUG]    org.apache.maven:maven-plugin-api:jar:2.0.6:compile
[DEBUG]    org.codehaus.plexus:plexus-utils:jar:2.0.5:compile
[DEBUG]    org.codehaus.plexus:plexus-container-default:jar:1.0-alpha-9:compile
[DEBUG]       junit:junit:jar:3.8.2:test
[DEBUG]    org.apache.commons:commons-exec:jar:1.1:compile
[DEBUG] Created new class realm plugin>org.codehaus.mojo:exec-maven-plugin:1.2.1
[DEBUG] Importing foreign packages into class realm plugin>org.codehaus.mojo:exec-maven-plugin:1.2.1
[DEBUG]   Imported:  < maven.api
[DEBUG] Populating class realm plugin>org.codehaus.mojo:exec-maven-plugin:1.2.1
[DEBUG]   Included: org.codehaus.mojo:exec-maven-plugin:jar:1.2.1
[DEBUG]   Included: org.apache.maven.reporting:maven-reporting-api:jar:2.0.6
[DEBUG]   Included: org.apache.maven.doxia:doxia-sink-api:jar:1.0-alpha-7
[DEBUG]   Included: commons-cli:commons-cli:jar:1.0
[DEBUG]   Included: org.codehaus.plexus:plexus-interactivity-api:jar:1.0-alpha-4
[DEBUG]   Included: org.codehaus.plexus:plexus-utils:jar:2.0.5
[DEBUG]   Included: org.apache.commons:commons-exec:jar:1.1
[DEBUG]   Excluded: org.apache.maven:maven-toolchain:jar:1.0
[DEBUG]   Excluded: org.apache.maven:maven-project:jar:2.0.6
[DEBUG]   Excluded: org.apache.maven:maven-settings:jar:2.0.6
[DEBUG]   Excluded: org.apache.maven:maven-profile:jar:2.0.6
[DEBUG]   Excluded: org.apache.maven:maven-plugin-registry:jar:2.0.6
[DEBUG]   Excluded: org.apache.maven:maven-model:jar:2.0.6
[DEBUG]   Excluded: org.apache.maven:maven-artifact:jar:2.0.6
[DEBUG]   Excluded: org.apache.maven:maven-artifact-manager:jar:2.0.6
[DEBUG]   Excluded: org.apache.maven:maven-repository-metadata:jar:2.0.6
[DEBUG]   Excluded: org.apache.maven:maven-core:jar:2.0.6
[DEBUG]   Excluded: org.apache.maven:maven-plugin-parameter-documenter:jar:2.0.6
[DEBUG]   Excluded: org.apache.maven:maven-error-diagnostics:jar:2.0.6
[DEBUG]   Excluded: org.apache.maven:maven-plugin-descriptor:jar:2.0.6
[DEBUG]   Excluded: org.apache.maven:maven-monitor:jar:2.0.6
[DEBUG]   Excluded: classworlds:classworlds:jar:1.1
[DEBUG]   Excluded: org.apache.maven:maven-plugin-api:jar:2.0.6
[DEBUG]   Excluded: org.codehaus.plexus:plexus-container-default:jar:1.0-alpha-9
[DEBUG]   Excluded: junit:junit:jar:3.8.2
[DEBUG] Configuring mojo org.codehaus.mojo:exec-maven-plugin:1.2.1:java from plugin realm ClassRealm[plugin>org.codehaus.mojo:exec-maven-plugin:1.2.1, parent: sun.misc.Launcher$AppClassLoader@1174ec5]
[DEBUG] Configuring mojo 'org.codehaus.mojo:exec-maven-plugin:1.2.1:java' with basic configurator -->
[DEBUG]   (f) arguments = [-Djppf.config=C:\\Users\\MAveces\\Documents\\NetBeansProjects\\JppfTest\\Jppf-5.0-Test\\configjppf.properties, -Djava.util.logging.config.file=C:\\Users\\MAveces\\Documents\\NetBeansProjects\\JppfTest\\Jppf-5.0-Test\\configlogging.properties, -Xmx64m]
[DEBUG]   (f) classpathScope = runtime
[DEBUG]   (f) cleanupDaemonThreads = false
[DEBUG]   (f) daemonThreadJoinTimeout = 15000
[DEBUG]   (f) includePluginDependencies = false
[DEBUG]   (f) includeProjectDependencies = true
[DEBUG]   (f) keepAlive = false
[DEBUG]   (f) killAfter = -1
[DEBUG]   (f) localRepository =       id: local
      url: file:///C:/Users/MAveces/.m2/repository/
   layout: default
snapshots: [enabled => true, update => always]
 releases: [enabled => true, update => always]

[DEBUG]   (f) mainClass = org.jppf.application.template.TemplateApplicationRunner
[DEBUG]   (f) pluginDependencies = [org.codehaus.mojo:exec-maven-plugin:maven-plugin:1.2.1:, org.apache.maven.reporting:maven-reporting-api:jar:2.0.6:compile, org.apache.maven.doxia:doxia-sink-api:jar:1.0-alpha-7:compile, commons-cli:commons-cli:jar:1.0:compile, org.codehaus.plexus:plexus-interactivity-api:jar:1.0-alpha-4:compile, org.codehaus.plexus:plexus-utils:jar:2.0.5:compile, org.apache.commons:commons-exec:jar:1.1:compile]
[DEBUG]   (f) project = MavenProject: Jppf-5.0-Test:Jppf-5.0-Test:0.0.1-SNAPSHOT @ C:\Users\MAveces\Documents\NetBeansProjects\JppfTest\Jppf-5.0-Test\pom.xml
[DEBUG]   (f) remoteRepositories = [      id: CentralMaven
      url: http://repo1.maven.org/maven2
   layout: default
    proxy: procuratio.canal.acp:8080
snapshots: [enabled => true, update => daily]
 releases: [enabled => true, update => daily]
,       id: Central Maven 2
      url: http://central.maven.org/maven2/
   layout: default
    proxy: procuratio.canal.acp:8080
snapshots: [enabled => true, update => daily]
 releases: [enabled => true, update => daily]
,       id: maven2-repository.dev.java.net
      url: http://download.java.net/maven/2
   layout: default
    proxy: procuratio.canal.acp:8080
snapshots: [enabled => true, update => daily]
 releases: [enabled => true, update => daily]
,       id: SpringRepo
      url: http://repo.spring.io/release
   layout: default
    proxy: procuratio.canal.acp:8080
snapshots: [enabled => true, update => daily]
 releases: [enabled => true, update => daily]
,       id: spring-milestones
      url: http://repo.spring.io/milestone
   layout: default
    proxy: procuratio.canal.acp:8080
snapshots: [enabled => false, update => daily]
 releases: [enabled => true, update => daily]
,       id: Jboss 1
      url: http://repository.jboss.org/nexus/content/groups/public-jboss/
   layout: default
    proxy: procuratio.canal.acp:8080
snapshots: [enabled => true, update => daily]
 releases: [enabled => true, update => daily]
,       id: Jboss 2
      url: https://repository.jboss.org/nexus/content/repositories/thirdparty-releases
   layout: default
    proxy: procuratio.canal.acp:8080
snapshots: [enabled => true, update => daily]
 releases: [enabled => true, update => daily]
,       id: Apache Repository
      url: https://repository.apache.org/content/groups/public/
   layout: default
    proxy: procuratio.canal.acp:8080
snapshots: [enabled => true, update => daily]
 releases: [enabled => true, update => daily]
,       id: central
      url: https://repo.maven.apache.org/maven2
   layout: default
    proxy: procuratio.canal.acp:8080
snapshots: [enabled => false, update => daily]
 releases: [enabled => true, update => daily]
]
[DEBUG]   (f) skip = false
[DEBUG]   (f) stopUnresponsiveDaemonThreads = false
[DEBUG] -- end configuration --
[DEBUG] Invoking : org.jppf.application.template.TemplateApplicationRunner.main(-Djppf.config=C:\\Users\\MAveces\\Documents\\NetBeansProjects\\JppfTest\\Jppf-5.0-Test\\configjppf.properties, -Djava.util.logging.config.file=C:\\Users\\MAveces\\Documents\\NetBeansProjects\\JppfTest\\Jppf-5.0-Test\\configlogging.properties, -Xmx64m)
[DEBUG] Plugin Dependencies will be excluded.
[DEBUG] Project Dependencies will be included.
[DEBUG] Collected project artifacts [org.slf4j:slf4j-api:jar:1.6.1:compile, org.slf4j:log4j-over-slf4j:jar:1.7.7:compile, ch.qos.logback:logback-classic:jar:1.1.2:compile, ch.qos.logback:logback-core:jar:1.1.2:compile, junit:junit:jar:4.11:compile, org.hamcrest:hamcrest-core:jar:1.3:compile, org.slf4j:slf4j-log4j12:jar:1.6.1:compile, log4j:log4j:jar:1.2.16:compile, org.jppf:jppf-common:jar:5.0:compile, commons-io:commons-io:jar:2.4:compile, org.jppf:jppf-jmxremote_optional:jar:1.1:compile, org.jppf:jppf-client:jar:5.0:compile]
[DEBUG] Collected project classpath [C:\Users\MAveces\Documents\NetBeansProjects\JppfTest\Jppf-5.0-Test\target\classes]
[DEBUG] Adding to classpath : file:/C:/Users/MAveces/Documents/NetBeansProjects/JppfTest/Jppf-5.0-Test/target/classes/
[DEBUG] Adding project dependency artifact: slf4j-api to classpath
[DEBUG] Adding project dependency artifact: log4j-over-slf4j to classpath
[DEBUG] Adding project dependency artifact: logback-classic to classpath
[DEBUG] Adding project dependency artifact: logback-core to classpath
[DEBUG] Adding project dependency artifact: junit to classpath
[DEBUG] Adding project dependency artifact: hamcrest-core to classpath
[DEBUG] Adding project dependency artifact: slf4j-log4j12 to classpath
[DEBUG] Adding project dependency artifact: log4j to classpath
[DEBUG] Adding project dependency artifact: jppf-common to classpath
[DEBUG] Adding project dependency artifact: commons-io to classpath
[DEBUG] Adding project dependency artifact: jppf-jmxremote_optional to classpath
[DEBUG] Adding project dependency artifact: jppf-client to classpath
[DEBUG] joining on thread Thread[org.jppf.application.template.TemplateApplicationRunner.main(),5,org.jppf.application.template.TemplateApplicationRunner]
[DEBUG] Setting accessibility to true in order to invoke main().
10:07:29.538 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG org.jppf.utils.JPPFConfiguration - reading JPPF configuration file: jppf.properties
10:07:29.543 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG org.jppf.client.AbstractJPPFClient - Instantiating JPPF client with uuid=C5083EB0-7552-F2F1-D293-F1D1286BB611
10:07:29.577 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG org.jppf.utils.NetworkUtils - process name=6624@TIRE-WKS-345494, pid=6624
10:07:29.577 [org.jppf.application.template.TemplateApplicationRunner.main()] INFO  org.jppf.utils.VersionUtils - --------------------------------------------------------------------------------
10:07:29.577 [org.jppf.application.template.TemplateApplicationRunner.main()] INFO  org.jppf.utils.VersionUtils - JPPF Version: 5.0, Build number: 1452, Build date: 2015-03-01 07:02 CET
10:07:29.577 [org.jppf.application.template.TemplateApplicationRunner.main()] INFO  org.jppf.utils.VersionUtils - starting client with PID=6624, UUID=C5083EB0-7552-F2F1-D293-F1D1286BB611
10:07:29.577 [org.jppf.application.template.TemplateApplicationRunner.main()] INFO  org.jppf.utils.VersionUtils - --------------------------------------------------------------------------------
10:07:29.577 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG org.jppf.utils.NetworkUtils - process name=6624@TIRE-WKS-345494, pid=6624
client process id: 6624, uuid: C5083EB0-7552-F2F1-D293-F1D1286BB611
10:07:29.581 [InitPools] DEBUG o.jppf.client.AbstractGenericClient - initializing connections
10:07:29.582 [InitPools] DEBUG o.jppf.client.AbstractGenericClient - initializing connections from discovery with priority = 0 and acceptMultipleInterfaces = false
10:07:29.586 [InitPools] DEBUG o.jppf.client.AbstractGenericClient - found peers in the configuration
10:07:29.586 [InitPools] DEBUG o.jppf.client.AbstractGenericClient - list of drivers: default-driver
10:07:29.588 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG org.jppf.client.JPPFClient - submitting job JPPFJob[name=Template blocking job, uuid=73AA67BB-843E-0147-C52D-75B47B709B66, blocking=true, nbTasks=1, nbResults=0, jobSLA=org.jppf.node.protocol.JPPFJobSLA@743354e1]
10:07:29.595 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.l.b.spi.JPPFBundlerFactory - using default properties: CLIENT{jppf.load.balancing.profile.jppf.size=1000000, jppf.load.balancing.algorithm=manual, jppf.script.default.language=javascript, jppf.load.balancing.profile=jppf}
10:07:29.596 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.l.b.spi.JPPFBundlerFactory - load balancing configuration using algorithm 'manual' with parameters: {size=1000000}
10:07:29.596 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.l.b.spi.JPPFBundlerFactory - oldCL=java.net.URLClassLoader@6536e939, currentCL=java.net.URLClassLoader@6536e939
10:07:29.598 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.l.b.spi.JPPFBundlerFactory - registering new load-balancing algorithm provider 'rl'
10:07:29.598 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.l.b.spi.JPPFBundlerFactory - registering new load-balancing algorithm provider 'proportional'
10:07:29.598 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.l.b.spi.JPPFBundlerFactory - registering new load-balancing algorithm provider 'autotuned'
10:07:29.598 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.l.b.spi.JPPFBundlerFactory - registering new load-balancing algorithm provider 'manual'
10:07:29.598 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.l.b.spi.JPPFBundlerFactory - registering new load-balancing algorithm provider 'nodethreads'
10:07:29.598 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.l.b.spi.JPPFBundlerFactory - found 5 load-balancing algorithms in the classpath
10:07:29.602 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.scheduling.JPPFScheduleHandler - created executor with name=Job Schedule Handler
10:07:29.602 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.scheduling.JPPFScheduleHandler - created executor with name=Job Expiration Handler
10:07:29.607 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.c.balancer.AbstractClientJob - creating ClientJob #1
10:07:29.608 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG org.jppf.client.JPPFResultCollector - job [73AA67BB-843E-0147-C52D-75B47B709B66] status changing from 'SUBMITTED' to 'PENDING'
10:07:29.608 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG org.jppf.client.JPPFResultCollector - job [73AA67BB-843E-0147-C52D-75B47B709B66] fire status changed event for 'PENDING'
10:07:29.608 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.c.b.queue.JPPFPriorityQueue - adding bundle with ClientJob[job=JPPFJob[name=Template blocking job, uuid=73AA67BB-843E-0147-C52D-75B47B709B66, blocking=true, nbTasks=1, nbResults=0, jobSLA=org.jppf.node.protocol.JPPFJobSLA@743354e1], jobStatus=PENDING, broadcastUUID=null, executing=false, nbTasks=1]
10:07:29.609 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.c.b.queue.JPPFPriorityQueue - Maps size information: priorityMap[shallow size=1, total elements=1] - sizeMap[shallow size=1, total elements=1]
10:07:29.638 [Thread-1] DEBUG org.jppf.utils.NetworkUtils - found network interface: name:lo (Software Loopback Interface 1)
10:07:29.638 [Thread-1] DEBUG org.jppf.utils.NetworkUtils - found network interface: name:net4 (Microsoft ISATAP Adapter)
10:07:29.639 [Thread-1] DEBUG org.jppf.utils.NetworkUtils - found network interface: name:eth4 (Intel(R) Ethernet Connection I217-LM #2)
10:07:29.639 [Thread-1] DEBUG org.jppf.utils.NetworkUtils - found network interface: name:eth6 (VirtualBox Host-Only Ethernet Adapter)
10:07:29.639 [Thread-1] DEBUG org.jppf.utils.NetworkUtils - found network interface: name:net6 (Microsoft ISATAP Adapter #2)
10:07:29.690 [Thread-1] DEBUG org.jppf.utils.NetworkUtils - found network interface: name:lo (Software Loopback Interface 1)
10:07:29.690 [Thread-1] DEBUG org.jppf.utils.NetworkUtils - found network interface: name:net4 (Microsoft ISATAP Adapter)
10:07:29.690 [Thread-1] DEBUG org.jppf.utils.NetworkUtils - found network interface: name:eth4 (Intel(R) Ethernet Connection I217-LM #2)
10:07:29.690 [Thread-1] DEBUG org.jppf.utils.NetworkUtils - found network interface: name:eth6 (VirtualBox Host-Only Ethernet Adapter)
10:07:29.690 [Thread-1] DEBUG org.jppf.utils.NetworkUtils - found network interface: name:net6 (Microsoft ISATAP Adapter #2)
10:07:29.690 [Thread-1] DEBUG o.j.c.d.JPPFMulticastReceiver - Found 2 addresses: 10.106.32.93 192.168.56.1
10:07:30.355 [Receiver@10.106.32.93:11111] DEBUG o.j.c.d.JPPFMulticastReceiver - nb connections: 1
10:07:30.356 [Receiver@192.168.56.1:11111] DEBUG o.j.c.d.JPPFMulticastReceiver - nb connections: 1
10:07:30.357 [Thread-1] DEBUG o.j.c.JPPFMulticastReceiverThread - Found connection information: JPPFConnectionInformation[uuid=1DCF19C0-192F-5EF9-F35A-F7D3BC85EAF0, host=10.106.32.93, managementPort=11198, recoveryPort=-1, serverPorts=11111, sslServerPorts=11443]
10:07:30.358 [Thread-1] DEBUG o.jppf.client.AbstractGenericClient - new connection: jppf_discovery-1
10:07:30.369 [JPPF Client-0001] DEBUG o.jppf.client.AbstractGenericClient - '10.106.32.93' was resolved into 'TIRE-WKS-345494.canal.acp'
10:07:30.380 [JPPF Client-0001] DEBUG o.jppf.client.AbstractConnectionPool - adding jppf_discovery-1-1[TIRE-WKS-345494.canal.acp:11111] : CREATED to JPPFConnectionPool[name=jppf_discovery-1, id=1, coreSize=1, maxSize=1, priority=0, sslEnabled=false, client=org.jppf.client.JPPFClient@46cf260b]
10:07:30.392 [JPPF Client-0001] DEBUG o.j.s.JPPFSerialization$Factory - found jppf.object.serialization.class = null
10:07:30.392 [JPPF Client-0001] DEBUG o.j.s.JPPFSerialization$Factory - using DefaultJavaSerialization
10:07:30.393 [JPPF Client-0001] DEBUG o.jppf.client.AbstractConnectionPool - adding JMXDriverConnectionWrapper[url=service:jmx:jmxmp://10.106.32.93:11198, connected=false, local=false, secure=false] to JMXConnectionPool[coreSize=1, maxSize=1, connectionCount=0, coreConnections=0]
10:07:30.394 [JPPF Client-0001] INFO  o.jppf.client.AbstractGenericClient - connection [jppf_discovery-1-1] created
10:07:30.395 [jmx@10.106.32.93:11198] DEBUG o.j.management.JMXConnectionThread - 10.106.32.93:11198 about to perform connection attempts
10:07:30.395 [JPPF Client-0001] DEBUG o.j.c.AbstractJPPFClientConnection - connection 'jppf_discovery-1-1' status changing from CREATED to NEW
10:07:30.397 [JPPF Client-0002] DEBUG o.jppf.client.ConnectionInitializer - initializing driver connection 'jppf_discovery-1-1[TIRE-WKS-345494.canal.acp:11111] : NEW'
10:07:30.398 [JPPF Client-0001] DEBUG o.j.client.balancer.JobManagerClient - adding connection jppf_discovery-1-1[TIRE-WKS-345494.canal.acp:11111] : NEW
10:07:30.403 [JPPF Client-0002] DEBUG o.j.c.ResourceProvider$Factory - jppf.resource.provider.class = org.jppf.classloader.ResourceProviderImpl
10:07:30.403 [JPPF Client-0002] DEBUG o.j.c.AbstractClassServerDelegate - resourceProvider=org.jppf.classloader.ResourceProviderImpl@72ebd7a9
10:07:30.405 [JPPF Client-0002] DEBUG o.j.c.AbstractClientConnectionHandler - connection 'jppf_discovery-1-1 - ClassServer' status changing from NEW to CONNECTING
[client: jppf_discovery-1-1 - ClassServer] Attempting connection to the class server at TIRE-WKS-345494.canal.acp:11111
10:07:30.406 [JPPF Client-0002] INFO  o.j.client.ClassServerDelegateImpl - [client: jppf_discovery-1-1 - ClassServer] Attempting connection to the class server at TIRE-WKS-345494.canal.acp:11111
10:07:30.406 [JPPF Client-0002] DEBUG o.j.c.socket.SocketInitializerImpl - SocketInitializerImpl[TIRE-WKS-345494.canal.acp:11111] about to close socket wrapper
10:07:30.421 [jmx@10.106.32.93:11198] DEBUG o.j.management.JMXConnectionWrapper - 10.106.32.93:11198 JMX connection successfully established
10:07:30.421 [jmx@10.106.32.93:11198] DEBUG o.j.management.JMXConnectionThread - 10.106.32.93:11198 about to suspend connection attempts
10:07:30.453 [JPPF Client-0001] DEBUG org.jppf.utils.NetworkUtils - found network interface: name:lo (Software Loopback Interface 1)
10:07:30.453 [JPPF Client-0001] DEBUG org.jppf.utils.NetworkUtils - found network interface: name:net4 (Microsoft ISATAP Adapter)
10:07:30.453 [JPPF Client-0001] DEBUG org.jppf.utils.NetworkUtils - found network interface: name:eth4 (Intel(R) Ethernet Connection I217-LM #2)
10:07:30.453 [JPPF Client-0001] DEBUG org.jppf.utils.NetworkUtils - found network interface: name:eth6 (VirtualBox Host-Only Ethernet Adapter)
10:07:30.453 [JPPF Client-0001] DEBUG org.jppf.utils.NetworkUtils - found network interface: name:net6 (Microsoft ISATAP Adapter #2)
10:07:30.503 [JPPF Client-0001] DEBUG org.jppf.utils.NetworkUtils - found network interface: name:lo (Software Loopback Interface 1)
10:07:30.503 [JPPF Client-0001] DEBUG org.jppf.utils.NetworkUtils - found network interface: name:net4 (Microsoft ISATAP Adapter)
10:07:30.503 [JPPF Client-0001] DEBUG org.jppf.utils.NetworkUtils - found network interface: name:eth4 (Intel(R) Ethernet Connection I217-LM #2)
10:07:30.503 [JPPF Client-0001] DEBUG org.jppf.utils.NetworkUtils - found network interface: name:eth6 (VirtualBox Host-Only Ethernet Adapter)
10:07:30.503 [JPPF Client-0001] DEBUG org.jppf.utils.NetworkUtils - found network interface: name:net6 (Microsoft ISATAP Adapter #2)
10:07:31.343 [Receiver@192.168.56.1:11111] DEBUG o.j.c.d.JPPFMulticastReceiver - nb connections: 1
10:07:31.343 [Receiver@10.106.32.93:11111] DEBUG o.j.c.d.JPPFMulticastReceiver - nb connections: 1
10:07:31.414 [JPPF Client-0002] DEBUG o.j.c.socket.AbstractSocketWrapper - getReceiveBufferSize() = 32768
[client: jppf_discovery-1-1 - ClassServer] Reconnected to the class server
10:07:31.414 [JPPF Client-0002] INFO  o.j.client.ClassServerDelegateImpl - [client: jppf_discovery-1-1 - ClassServer] Reconnected to the class server
10:07:31.414 [JPPF Client-0002] DEBUG o.j.c.AbstractClientConnectionHandler - connection 'jppf_discovery-1-1 - ClassServer' status changing from CONNECTING to ACTIVE
10:07:31.415 [JPPF Client-0002] DEBUG o.j.c.AbstractClientConnectionHandler - connection 'jppf_discovery-1-1 - TasksServer' status changing from NEW to CONNECTING
10:07:31.415 [JPPF Client-0002] DEBUG o.j.c.AbstractJPPFClientConnection - connection 'jppf_discovery-1-1' status changing from NEW to CONNECTING
10:07:31.415 [jppf_discovery-1-1 - ClassServer] DEBUG o.j.c.AbstractClassServerDelegate - [jppf_discovery-1-1 - ClassServer] : sending channel identifier
10:07:31.416 [jppf_discovery-1-1 - ClassServer] DEBUG o.j.c.AbstractClassServerDelegate - [jppf_discovery-1-1 - ClassServer] : sending initial resource
10:07:31.433 [jppf_discovery-1-1 - ClassServer] DEBUG org.jppf.io.IOHelper - Loaded serializer class class org.jppf.utils.ObjectSerializerImpl
10:07:31.451 [jppf_discovery-1-1 - ClassServer] DEBUG o.j.c.AbstractClassServerDelegate - [jppf_discovery-1-1 - ClassServer] data sent to the server
10:07:31.451 [jppf_discovery-1-1 - ClassServer] DEBUG o.j.c.AbstractClassServerDelegate - [jppf_discovery-1-1 - ClassServer] reading next resource ...
10:07:31.455 [jppf_discovery-1-1 - ClassServer] DEBUG o.j.c.AbstractClassServerDelegate - [jppf_discovery-1-1 - ClassServer] : server handshake done
10:07:31.456 [jppf_discovery-1-1 - ClassServer] DEBUG o.j.c.AbstractClassServerDelegate - [jppf_discovery-1-1 - ClassServer] reading next resource ...
10:07:32.342 [Receiver@10.106.32.93:11111] DEBUG o.j.c.d.JPPFMulticastReceiver - nb connections: 1
10:07:32.343 [Receiver@192.168.56.1:11111] DEBUG o.j.c.d.JPPFMulticastReceiver - nb connections: 1
10:07:33.343 [Receiver@10.106.32.93:11111] DEBUG o.j.c.d.JPPFMulticastReceiver - nb connections: 1
10:07:33.343 [Receiver@192.168.56.1:11111] DEBUG o.j.c.d.JPPFMulticastReceiver - nb connections: 1
10:07:34.342 [Receiver@10.106.32.93:11111] DEBUG o.j.c.d.JPPFMulticastReceiver - nb connections: 1
10:07:34.342 [Receiver@192.168.56.1:11111] DEBUG o.j.c.d.JPPFMulticastReceiver - nb connections: 1
10:07:35.343 [Receiver@10.106.32.93:11111] DEBUG o.j.c.d.JPPFMulticastReceiver - nb connections: 1
10:07:35.343 [Receiver@192.168.56.1:11111] DEBUG o.j.c.d.JPPFMulticastReceiver - nb connections: 1
10:07:36.343 [Receiver@10.106.32.93:11111] DEBUG o.j.c.d.JPPFMulticastReceiver - nb connections: 1
10:07:36.343 [Receiver@192.168.56.1:11111] DEBUG o.j.c.d.JPPFMulticastReceiver - nb connections: 1
10:07:37.274 [JPPF Client-0001] DEBUG o.j.client.balancer.JobManagerClient - end of adding connection jppf_discovery-1-1[TIRE-WKS-345494.canal.acp:11111] : CONNECTING
[client: jppf_discovery-1-1 - TasksServer] Attempting connection to the task server at TIRE-WKS-345494.canal.acp:11111
10:07:37.275 [JPPF Client-0001] DEBUG o.jppf.client.AbstractGenericClient - end of connection [jppf_discovery-1-1] created
10:07:37.275 [JPPF Client-0002] DEBUG o.j.c.TaskServerConnectionHandler - [client: jppf_discovery-1-1 - TasksServer] Attempting connection to the task server at TIRE-WKS-345494.canal.acp:11111
10:07:37.275 [JPPF Client-0002] DEBUG o.j.c.socket.SocketInitializerImpl - SocketInitializerImpl[TIRE-WKS-345494.canal.acp:11111] about to close socket wrapper
10:07:37.343 [Receiver@10.106.32.93:11111] DEBUG o.j.c.d.JPPFMulticastReceiver - nb connections: 1
10:07:37.343 [Receiver@192.168.56.1:11111] DEBUG o.j.c.d.JPPFMulticastReceiver - nb connections: 1
10:07:38.277 [JPPF Client-0002] DEBUG o.j.c.socket.AbstractSocketWrapper - getReceiveBufferSize() = 32768
10:07:38.277 [JPPF Client-0002] DEBUG o.j.c.TaskServerConnectionHandler - sending JPPF identifier
10:07:38.280 [JPPF Client-0002] DEBUG o.j.client.BaseJPPFClientConnection - JPPFClientConnectionImpl[connectionUuid=C5083EB0-7552-F2F1-D293-F1D1286BB611_1, status=CONNECTING] sending handshake job, uuidPath=TraversalList[position=-1, list=[C5083EB0-7552-F2F1-D293-F1D1286BB611]]
10:07:38.330 [JPPF Client-0002] DEBUG o.j.client.BaseJPPFClientConnection - JPPFClientConnectionImpl[connectionUuid=C5083EB0-7552-F2F1-D293-F1D1286BB611_1, status=CONNECTING] : received bundle JPPFTaskBundle[name=handshake job, uuid=handshake job, initialTaskCount=0, taskCount=0, bundleUuid=null, uuidPath=TraversalList[position=0, list=[C5083EB0-7552-F2F1-D293-F1D1286BB611, 1DCF19C0-192F-5EF9-F35A-F7D3BC85EAF0]]], positions=
[client: jppf_discovery-1-1 - TasksServer] Reconnected to the JPPF task server
10:07:38.330 [JPPF Client-0002] DEBUG o.j.c.TaskServerConnectionHandler - [client: jppf_discovery-1-1 - TasksServer] Reconnected to the JPPF task server
10:07:38.330 [JPPF Client-0002] DEBUG o.j.c.AbstractClientConnectionHandler - connection 'jppf_discovery-1-1 - TasksServer' status changing from CONNECTING to ACTIVE
10:07:38.330 [JPPF Client-0002] DEBUG o.j.c.AbstractJPPFClientConnection - connection 'jppf_discovery-1-1' status changing from CONNECTING to ACTIVE
10:07:38.331 [JPPF Client-0002] DEBUG o.j.client.JPPFClientConnectionImpl - connection [jppf_discovery-1-1] status=ACTIVE
10:07:38.331 [TaskQueueChecker] DEBUG o.j.c.b.queue.TaskQueueChecker - 1 channels idle
10:07:38.332 [JPPF Client-0002] DEBUG org.jppf.client.AbstractJPPFClient - adding connection jppf_discovery-1-1[TIRE-WKS-345494.canal.acp:11111] : ACTIVE
10:07:38.332 [TaskQueueChecker] DEBUG o.j.c.b.queue.TaskQueueChecker - found 1 acceptable channels
10:07:38.332 [JPPF Client-0002] DEBUG o.j.client.JPPFClientConnectionImpl - connection [jppf_discovery-1-1] added to the client pool
10:07:38.332 [TaskQueueChecker] DEBUG o.j.c.b.queue.TaskQueueChecker - channel found for bundle: ChannelWrapperRemote[channel=jppf_discovery-1-1[TIRE-WKS-345494.canal.acp:11111] : ACTIVE]
10:07:38.332 [TaskQueueChecker] DEBUG o.j.c.b.queue.TaskQueueChecker - dispatching jobUuid=73AA67BB-843E-0147-C52D-75B47B709B66 to channel ChannelWrapperRemote[channel=jppf_discovery-1-1[TIRE-WKS-345494.canal.acp:11111] : ACTIVE], connectionUuid=
10:07:38.333 [TaskQueueChecker] DEBUG o.j.c.b.queue.JPPFPriorityQueue - requesting bundle with 1000000 tasks, next bundle has 1 tasks
10:07:38.335 [TaskQueueChecker] DEBUG o.j.c.b.queue.JPPFPriorityQueue - removing bundle from queue, jobId=Template blocking job
10:07:38.335 [TaskQueueChecker] DEBUG o.j.c.b.queue.JPPFPriorityQueue - Maps size information: priorityMap[shallow size=0, total elements=0] - sizeMap[shallow size=0, total elements=0]
10:07:38.335 [TaskQueueChecker] DEBUG o.j.c.AbstractClientConnectionHandler - connection 'jppf_discovery-1-1 - TasksServer' status changing from ACTIVE to EXECUTING
10:07:38.335 [TaskQueueChecker] DEBUG o.j.c.AbstractJPPFClientConnection - connection 'jppf_discovery-1-1' status changing from ACTIVE to EXECUTING
10:07:38.337 [TaskQueueChecker] DEBUG org.jppf.client.JPPFJob - firing JOB_START event with 0 tasks for JPPFJob[name=Template blocking job, uuid=73AA67BB-843E-0147-C52D-75B47B709B66, blocking=true, nbTasks=1, nbResults=0, jobSLA=org.jppf.node.protocol.JPPFJobSLA@743354e1]
10:07:38.338 [TaskQueueChecker] DEBUG org.jppf.client.JPPFResultCollector - job [73AA67BB-843E-0147-C52D-75B47B709B66] status changing from 'PENDING' to 'EXECUTING'
10:07:38.338 [TaskQueueChecker] DEBUG org.jppf.client.JPPFResultCollector - job [73AA67BB-843E-0147-C52D-75B47B709B66] fire status changed event for 'EXECUTING'
10:07:38.339 [TaskQueueChecker] DEBUG org.jppf.client.JPPFJob - firing JOB_DISPATCH event with 1 tasks for JPPFJob[name=Template blocking job, uuid=73AA67BB-843E-0147-C52D-75B47B709B66, blocking=true, nbTasks=1, nbResults=0, jobSLA=org.jppf.node.protocol.JPPFJobSLA@743354e1]
10:07:38.339 [RemoteChannelWrapper-jppf_discovery-1-1-0001] DEBUG o.jppf.client.AbstractGenericClient - registered java.net.URLClassLoader@6536e939
10:07:38.343 [Receiver@10.106.32.93:11111] DEBUG o.j.c.d.JPPFMulticastReceiver - nb connections: 1
10:07:38.343 [Receiver@192.168.56.1:11111] DEBUG o.j.c.d.JPPFMulticastReceiver - nb connections: 1
10:07:38.359 [jppf_discovery-1-1 - ClassServer] DEBUG o.j.c.AbstractClassServerDelegate - [jppf_discovery-1-1 - ClassServer] resource requested: JPPFResourceWrapper[dynamic=true, name=org/jppf/application/template/TemplateJPPFTask.class, state=PROVIDER_REQUEST, uuidPath=TraversalList[position=0, list=[C5083EB0-7552-F2F1-D293-F1D1286BB611, 1DCF19C0-192F-5EF9-F35A-F7D3BC85EAF0]], callableID=-1]
10:07:38.359 [jppf_discovery-1-1 - ClassServer] DEBUG o.j.c.AbstractClassServerDelegate - [jppf_discovery-1-1 - ClassServer] using classloaders=[java.net.URLClassLoader@6536e939]
10:07:38.360 [jppf_discovery-1-1 - ClassServer] DEBUG o.j.c.AbstractResourceProvider - resource [org/jppf/application/template/TemplateJPPFTask.class] found
10:07:38.360 [jppf_discovery-1-1 - ClassServer] DEBUG o.j.c.AbstractClassServerDelegate - [jppf_discovery-1-1 - ClassServer] found resource: org/jppf/application/template/TemplateJPPFTask.class (877 bytes)
10:07:38.361 [jppf_discovery-1-1 - ClassServer] DEBUG o.j.c.AbstractClassServerDelegate - [jppf_discovery-1-1 - ClassServer] data sent to the server
10:07:38.361 [jppf_discovery-1-1 - ClassServer] DEBUG o.j.c.AbstractClassServerDelegate - [jppf_discovery-1-1 - ClassServer] reading next resource ...
10:07:38.371 [RemoteChannelWrapper-jppf_discovery-1-1-0001] DEBUG o.j.client.BaseJPPFClientConnection - JPPFClientConnectionImpl[connectionUuid=C5083EB0-7552-F2F1-D293-F1D1286BB611_1, status=EXECUTING] : received bundle JPPFTaskBundle[name=Template blocking job, uuid=73AA67BB-843E-0147-C52D-75B47B709B66, initialTaskCount=1, taskCount=1, bundleUuid=null, uuidPath=TraversalList[position=0, list=[C5083EB0-7552-F2F1-D293-F1D1286BB611, 1DCF19C0-192F-5EF9-F35A-F7D3BC85EAF0]]], positions=0
10:07:38.371 [RemoteChannelWrapper-jppf_discovery-1-1-0001] DEBUG o.j.c.balancer.ChannelWrapperRemote - received 1 tasks from server, first position=0
10:07:38.372 [RemoteChannelWrapper-jppf_discovery-1-1-0001] DEBUG org.jppf.client.balancer.ClientJob - received 1 results for bundle [jobId=Template blocking job, jobUuid=73AA67BB-843E-0147-C52D-75B47B709B66, initialTaskCount=1, taskCount=1, requeue=false, cancelled=false]
10:07:38.372 [RemoteChannelWrapper-jppf_discovery-1-1-0001] DEBUG org.jppf.client.JobResults - adding 1 results
10:07:38.373 [RemoteChannelWrapper-jppf_discovery-1-1-0001] DEBUG org.jppf.client.JPPFResultCollector - Received results for 1 tasks, pendingCount=0, count=1, jobResults=JobResults[size=1, positions=[0]]
10:07:38.373 [RemoteChannelWrapper-jppf_discovery-1-1-0001] DEBUG org.jppf.client.JPPFJob - firing JOB_RETURN event with 1 tasks for JPPFJob[name=Template blocking job, uuid=73AA67BB-843E-0147-C52D-75B47B709B66, blocking=true, nbTasks=1, nbResults=1, jobSLA=org.jppf.node.protocol.JPPFJobSLA@743354e1]
10:07:38.373 [RemoteChannelWrapper-jppf_discovery-1-1-0001] DEBUG org.jppf.client.JPPFJob - firing JOB_END event with 1 tasks for JPPFJob[name=Template blocking job, uuid=73AA67BB-843E-0147-C52D-75B47B709B66, blocking=true, nbTasks=1, nbResults=1, jobSLA=org.jppf.node.protocol.JPPFJobSLA@743354e1]
10:07:38.373 [RemoteChannelWrapper-jppf_discovery-1-1-0001] DEBUG o.jppf.client.AbstractGenericClient - unregistering 73AA67BB-843E-0147-C52D-75B47B709B66
Results for job 'Template blocking job' :
10:07:38.373 [RemoteChannelWrapper-jppf_discovery-1-1-0001] DEBUG org.jppf.client.balancer.ClientJob - bundle=[jobId=Template blocking job, jobUuid=73AA67BB-843E-0147-C52D-75B47B709B66, initialTaskCount=1, taskCount=1, requeue=false, cancelled=false], exception=null for ClientJob[job=JPPFJob[name=Template blocking job, uuid=73AA67BB-843E-0147-C52D-75B47B709B66, blocking=true, nbTasks=1, nbResults=1, jobSLA=org.jppf.node.protocol.JPPFJobSLA@743354e1], jobStatus=EXECUTING, broadcastUUID=null, executing=true, nbTasks=0]
Template blocking job - Template task, execution result: the execution was performed successfully
10:07:38.374 [RemoteChannelWrapper-jppf_discovery-1-1-0001] DEBUG o.j.c.b.queue.JPPFPriorityQueue - removing bundle from queue, jobId=Template blocking job
10:07:38.374 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.jppf.client.AbstractGenericClient - closing JPPF client
10:07:38.374 [RemoteChannelWrapper-jppf_discovery-1-1-0001] DEBUG org.jppf.client.JPPFResultCollector - job [73AA67BB-843E-0147-C52D-75B47B709B66] status changing from 'EXECUTING' to 'COMPLETE'
10:07:38.374 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.jppf.client.AbstractGenericClient - unregistering startup classes
10:07:38.374 [RemoteChannelWrapper-jppf_discovery-1-1-0001] DEBUG org.jppf.client.JPPFResultCollector - job [73AA67BB-843E-0147-C52D-75B47B709B66] fire status changed event for 'COMPLETE'
10:07:38.374 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.jppf.client.AbstractGenericClient - closing job manager
10:07:38.374 [RemoteChannelWrapper-jppf_discovery-1-1-0001] DEBUG o.j.c.AbstractClientConnectionHandler - connection 'jppf_discovery-1-1 - TasksServer' status changing from EXECUTING to ACTIVE
10:07:38.374 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.client.balancer.JobManagerClient - closing org.jppf.client.balancer.JobManagerClient@4643ac23
10:07:38.374 [RemoteChannelWrapper-jppf_discovery-1-1-0001] DEBUG o.j.c.AbstractJPPFClientConnection - connection 'jppf_discovery-1-1' status changing from EXECUTING to ACTIVE
10:07:38.375 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.jppf.client.AbstractGenericClient - closing broadcast receiver
10:07:38.375 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.jppf.client.AbstractGenericClient - closing executor
10:07:38.375 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.jppf.client.AbstractGenericClient - clearing registered class loaders
10:07:38.375 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.c.AbstractJPPFClientConnection - connection 'jppf_discovery-1-1' status changing from ACTIVE to CLOSED
10:07:38.376 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.jppf.client.AbstractGenericClient - Connection [jppf_discovery-1-1] CLOSED
10:07:38.376 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG org.jppf.client.AbstractJPPFClient - removing connection jppf_discovery-1-1[TIRE-WKS-345494.canal.acp:11111] : CLOSED
10:07:38.376 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.jppf.client.AbstractConnectionPool - removing jppf_discovery-1-1[TIRE-WKS-345494.canal.acp:11111] : CLOSED from JPPFConnectionPool[name=jppf_discovery-1, id=1, coreSize=1, maxSize=1, priority=0, sslEnabled=false, client=org.jppf.client.JPPFClient@46cf260b]
10:07:38.376 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.c.AbstractJPPFClientConnection - closing connection JPPFClientConnectionImpl[connectionUuid=C5083EB0-7552-F2F1-D293-F1D1286BB611_1, status=CLOSED]
10:07:38.376 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.client.BaseJPPFClientConnection - JPPFClientConnectionImpl[connectionUuid=C5083EB0-7552-F2F1-D293-F1D1286BB611_1, status=CLOSED] sending close command job, uuidPath=TraversalList[position=-1, list=[C5083EB0-7552-F2F1-D293-F1D1286BB611]]
10:07:38.377 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.c.AbstractJPPFClientConnection - closing task server connection jppf_discovery-1-1[TIRE-WKS-345494.canal.acp:11111] : CLOSED
10:07:38.377 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.c.AbstractClientConnectionHandler - closing jppf_discovery-1-1 - TasksServer
10:07:38.377 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.c.socket.SocketInitializerImpl - SocketInitializerImpl[TIRE-WKS-345494.canal.acp:11111] closing socket initializer
10:07:38.378 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.c.AbstractClientConnectionHandler - jppf_discovery-1-1 - TasksServer closed
10:07:38.378 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.c.AbstractJPPFClientConnection - closing class server connection jppf_discovery-1-1[TIRE-WKS-345494.canal.acp:11111] : CLOSED
10:07:38.378 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.c.AbstractClassServerDelegate - closing jppf_discovery-1-1 - ClassServer
10:07:38.378 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.c.AbstractClientConnectionHandler - closing jppf_discovery-1-1 - ClassServer
10:07:38.378 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.c.socket.SocketInitializerImpl - SocketInitializerImpl[TIRE-WKS-345494.canal.acp:11111] closing socket initializer
10:07:38.378 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.c.AbstractClientConnectionHandler - jppf_discovery-1-1 - ClassServer closed
10:07:38.378 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.c.AbstractClassServerDelegate - jppf_discovery-1-1 - ClassServer closed
10:07:38.378 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.c.AbstractJPPFClientConnection - closing jmx connection jppf_discovery-1-1[TIRE-WKS-345494.canal.acp:11111] : CLOSED
10:07:38.378 [org.jppf.application.template.TemplateApplicationRunner.main()] DEBUG o.j.c.AbstractJPPFClientConnection - connection JPPFClientConnectionImpl[connectionUuid=C5083EB0-7552-F2F1-D293-F1D1286BB611_1, status=CLOSED] closed
[DEBUG] joining on thread Thread[Receiver@10.106.32.93:11111,5,org.jppf.application.template.TemplateApplicationRunner]
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time: 10.506 s
[INFO] Finished at: 2015-10-05T10:07:39-05:00
[INFO] Final Memory: 11M/308M
[INFO] ------------------------------------------------------------------------
