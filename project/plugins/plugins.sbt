
//
// add oneJar support
//

libraryDependencies <+= sbtVersion(v => "com.github.siasia" %% "xsbt-proguard-plugin" % (v+"-0.1.1"))

//resolvers += "retronym-releases" at "http://retronym.github.com/repo/releases"
// resolvers += "retronym-snapshots" at "http://retronym.github.com/repo/snapshots"
// addSbtPlugin("com.github.retronym" % "sbt-onejar" % "0.5")

