import scala.offheap.numeric.jni.LapackJNI;

public class check {
	int main (String argv[]) {
		LapackJNI.cblas_dgemm(102, 'n', 'n', 2, 2, 2, 1.0, 0, 2, 0, 2, 1.0, 0, 2);
		return 0;
	}
}
