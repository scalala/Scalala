package scalala;
import scala.tools.nsc.MainGenericRunner

object ScalalaConsole {
  def main(args : Array[String]) {
    MainGenericRunner.main(Array("-i","scalala.scala")++args)
    exit(0)
  }
}
