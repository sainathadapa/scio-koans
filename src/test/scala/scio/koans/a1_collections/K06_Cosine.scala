package scio.koans.a1_collections

import org.openjdk.jmh.annotations._
import scio.koans.shared._

/**
 * Compute the cosine similarity between 2 vectors.
 */
class K06_Cosine extends JmhKoan {
  ImNotDone

  val vec1: Array[Double] = (1 to 100).map(_.toDouble / 100).toArray
  val vec2: Array[Double] = (-100 to -1).map(_.toDouble / 100).toArray

  // cosine(v1, v2) = dot_product(v1, v2) / (magnitude(v1) * magnitude(v2))
  // dot_product(v1, v2) = sum(v1[i] * v2[i] for i in [0, |v1|])
  // magnitude(v) = sqrt(sum(v[i]^2 for i in [0, |v|]))
  @Benchmark def baseline: Double = {
    val dotProd = (vec1 zip vec2).map(t => t._1 * t._2).sum
    val mag1 = math.sqrt(vec1.map(math.pow(_, 2)).sum)
    val mag2 = math.sqrt(vec2.map(math.pow(_, 2)).sum)
    dotProd / (mag1 * mag2)
  }

  @Benchmark def v1: Double = {
    var dp = 0.0
    var m1 = 0.0
    var m2 = 0.0
    var a = 0.0
    var b = 0.0
    var i = 0
    while (i < vec1.length) {
      a = vec1(i)
      b = vec2(i)
      dp += a * b
      m1 += a * a
      m2 += b * b
      i += 1
    }
    dp / (math.sqrt(m1) * math.sqrt(m2))
  }

  verifyResults()
  verifySpeedup(Speedup.Times(100))
}
