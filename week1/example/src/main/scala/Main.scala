import example.Lists


object Main extends App {

    def abs(x:Double) = if (x < 0) -x else x

    def sqrtIter(guess: Double, x: Double): Double = 
        if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x), x)
    
    // def isGoodEnough(guess: Double, x: Double) = {println(guess) ; abs(guess * guess - x ) < 0.001 }
    def isGoodEnough(guess: Double, x: Double) = { println(guess) ; abs((guess * guess) / x ) < 1.005 && abs((guess * guess) / x ) > 1}
    
    def improve(guess: Double, x: Double) =(guess + x / guess) / 2
    
    def sqrt(x: Double) = sqrtIter(1.0, x)

    print(sqrt(1.0e50))
}


