/*
 Copyright 2010 David Hall, Daniel Ramage

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
package scalala;
package tensor;
package dense;

import org.scalatest.junit._;
import org.junit.runner.RunWith;

@RunWith(classOf[JUnitRunner])
class DenseMatrixTest extends ScalalaTest {
 import tensor.dense.DenseMatrix;
 import Scalala._;

 def createM1 = DenseMatrix(2, 2)(1.0, 2.0, 3.0, 4.0)

 test("copy is not shallow") {
   val m1 = createM1
   val m2 = m1.copy
   m2 *= 2.0
   assert(m1 ==(createM1));
 }

 test("svd is not destructive")  {
   val m1 = createM1
   svd(m1)
   assert(m1 ==(createM1))
 }
}
