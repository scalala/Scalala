import sbt._
import java.util.jar.Attributes.Name._


class Project(info: ProjectInfo) extends DefaultProject(info) {
  val scalanlpRepo = "Scala NLP" at "http://repo.scalanlp.org/repo/"
  val ondexRepo = "ondex" at "http://ondex.rothamsted.bbsrc.ac.uk/nexus/content/groups/public"
  val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

  // Dependencies are currently picked up from the POM file.

  override def compileOptions = Optimise :: Deprecation :: target(Target.Java1_5) :: Unchecked :: super.compileOptions.toList

  override def packageOptions = ManifestAttributes((IMPLEMENTATION_TITLE, "Scalala"), (IMPLEMENTATION_URL, "http://code.google.com/p/scalala/"), (IMPLEMENTATION_VENDOR, "org.scalanlp"), (SEALED, "true")) :: Nil

  override def managedStyle = ManagedStyle.Maven

  override def packageDocsJar = defaultJarPath("-javadoc.jar")

  override def packageSrcJar = defaultJarPath("-sources.jar")

  override def packageTestSrcJar = defaultJarPath("-test-sources.jar")

  lazy val sourceArtifact = Artifact(artifactID, "src", "jar", Some("sources"), Nil, None)

  lazy val docsArtifact = Artifact(artifactID, "docs", "jar", Some("javadoc"), Nil, None)

  override def packageToPublishActions = super.packageToPublishActions ++ Seq(packageDocs, packageSrc, packageTestSrc)
}
