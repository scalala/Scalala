
resolvers += "Proguard plugin repo" at "http://siasia.github.com/maven2"

libraryDependencies <+= sbtVersion(v => "com.github.siasia" %% "xsbt-proguard-plugin" % ("0.11.2-0.1.1"))

addSbtPlugin("com.github.mpeltonen" % "sbt-idea" % "1.0.0")
