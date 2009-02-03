package scalala.library

trait IO extends Library {
  
  import java.lang.Double.parseDouble;
  
  def dlmread(file : String) : Matrix = {
    val numCols = scala.io.Source.fromFile(file).getLines.next.trim.split("\\s+").length;
    val numRows = scala.io.Source.fromFile(file).getLines.map(_ => 1).reduceLeft(_+_);
    
    val m = DenseMatrix(numRows, numCols);
    
    var i = 0;
    for (line <- scala.io.Source.fromFile(file).getLines) {
      var j = 0;
      for (entry <- line.trim.split("\\s+").map(parseDouble)) {
        m(i,j) = entry;
        j += 1;
      }
      i += 1;
    }
    
    return m;
  }
}
