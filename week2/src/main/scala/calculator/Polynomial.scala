package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
      Signal(getDelta(a(), b(), c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(getSolutions(a(), b(), c(), getDelta(a(), b(), c())))
  }
  
  private def getDelta(a:Double, b:Double, c:Double):Double = {
    b*b - 4*a*c
  }
  
  private def getSolutions(a:Double, b:Double, c:Double, delta:Double):Set[Double] = delta match {
    case x if x < 0 => Set()
    case _ => getPositiveSolutions(a, b, c, delta)
  }
  
  private def getPositiveSolutions(a:Double, b:Double, c:Double, delta:Double):Set[Double] = {
    var sol1 = (-b + scala.math.sqrt(delta))/(2*a)
    var sol2 = (-b - scala.math.sqrt(delta))/(2*a)
    Set(sol1,sol2)
  }
}
