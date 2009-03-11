package scalala;

object ScalalaConsole {
  def main(args : Array[String]) {
    val method = Class.forName("scala.tools.nsc.MainGenericRunner").getMethod("main", classOf[Array[String]]);
    val aurg : Object = (Array[String]("-i","scalala.scala")++args).asInstanceOf[Array[String]];
    method.invoke(null, aurg);
    exit(0);
  }
}
