import scala.offheap.numeric.jni.LapackJNI;

object main {
  def main (argv: Array[String]): Unit =  {
    LapackJNI.cblas_dgemm(102, 'n', 'n', 2, 2, 2, 1.0, 0, 2, 0, 2, 1.0, 0, 2)
  }
}
