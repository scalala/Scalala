
// libraryDependencies <+= sbtVersion(v => "com.github.siasia" %% "xsbt-proguard-plugin" % (v+"-0.1.1"))

resolvers += "Proguard plugin repo" at "http://siasia.github.com/maven2"

addSbtPlugin("com.github.siasia" % "xsbt-proguard-plugin" % "0.1-SNAPSHOT")

// resolvers += "retronym-releases" at "http://retronym.github.com/repo/releases"
// resolvers += "retronym-snapshots" at "http://retronym.github.com/repo/snapshots"
// addSbtPlugin("com.github.retronym" % "sbt-onejar" % "0.5")

