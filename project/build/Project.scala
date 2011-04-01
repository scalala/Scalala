import sbt._
import java.util.jar.Attributes.Name._


class Project(info: ProjectInfo) extends ProguardProject(info) {
  val scalaNlp = "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo"
  val ondexRepo = "ondex" at "http://ondex.rothamsted.bbsrc.ac.uk/nexus/content/groups/public"
  val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"
  // lazy val mavenLocal = "Local Maven Repository" at "file://"+Path.userHome+"/.m2/repository"
  // val publishTo = "Local Nexus" at "http://localhost:8081/nexus/content/repositories/snapshots" 


  val Jline = "jline" % "jline" % "0.9.94"
  val NetlibJava = "netlib" % "netlib-java" % "0.9.2"
  val ArpackCombo = "netlib" % "arpack-combo" % "0.1"
  val Jcommon = "jfree" % "jcommon" % "1.0.16"
  val Jfreechart = "jfree" % "jfreechart" % "1.0.13"
  val XmlgraphicsCommons = "org.apache.xmlgraphics" % "xmlgraphics-commons" % "1.3.1"
  val Itext = "com.lowagie" % "itext" % "2.1.5" intransitive()
  val Scalacheck = "org.scala-tools.testing" %% "scalacheck" % "1.7" % "test"
  val Scalatest = "org.scalatest" % "scalatest" % "1.2-for-scala-2.8.0.RC6-SNAPSHOT" % "test"
  val Junit = "junit" % "junit" % "4.5" % "test"

  override def compileOptions =
    Optimise :: Deprecation ::
    target(Target.Java1_5) ::
    Unchecked :: CompileOption("-no-specialization") ::
    super.compileOptions.toList

  override def packageOptions = ManifestAttributes(
    (IMPLEMENTATION_TITLE, "Scalala"),
    (IMPLEMENTATION_URL, "http://github.com/scalala/"),
    (IMPLEMENTATION_VENDOR, "scala.la"),
    (SEALED, "true")) :: Nil

  override def managedStyle = ManagedStyle.Maven

  override def packageDocsJar = defaultJarPath("-javadoc.jar")

  override def packageSrcJar = defaultJarPath("-sources.jar")

  override def packageTestSrcJar = defaultJarPath("-test-sources.jar")

  lazy val sourceArtifact = Artifact(artifactID, "src", "jar", Some("sources"), Nil, None)

  lazy val docsArtifact = Artifact(artifactID, "docs", "jar", Some("javadoc"), Nil, None)

  override def packageToPublishActions =
    super.packageToPublishActions ++ Seq(packageDocs, packageSrc, packageTestSrc)
  
  override def rtJarPath = {
    import System.getProperty
    val javaHome = Path.fromFile(getProperty("java.home"))
    getProperty("java.vm.vendor") match {
      case "Apple Inc." => Path.fromFile(javaHome.asFile.getParentFile) / "Classes" / "classes.jar"
      case _ => javaHome / "lib" / "rt.jar"
    }
  }
  
  def keepMain(className: String) =
     """-keep public class %s {
     | public static void main(java.lang.String[]);
     |}""".stripMargin format className
      
  override def allDependencyJars = (
    super.allDependencyJars +++ 
    Path.fromFile(buildScalaInstance.compilerJar) +++ 
    Path.fromFile(buildScalaInstance.libraryJar)
  )

  override def proguardOptions = List(
    "-keep class scalala.** { *; }",
    "-keep class org.jfree.** { *; }",
    keepMain("scalala.ScalalaConsole$"),
    keepMain("scala.tools.nsc.MainGenericRunner"),
    "-dontoptimize",
    "-dontobfuscate", 
    proguardKeepLimitedSerializability,
    proguardKeepAllScala,
    "-keep class ch.epfl.** { *; }",
    "-keep interface scala.ScalaObject"
  )
}

