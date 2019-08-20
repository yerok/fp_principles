


def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if ( a > b) acc
    else loop(a + 1, f(a) + acc )
  }
  loop(a, 0)
}

println(sum(x => x*x, 1,5))

def product(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else f(a) * product(f)(a+1,b)
}

println(product(x => x*x)(3,4))

def fact(f: Int => Int)(a: Int): Int = {
    if (a == 1) 1
    else f(a) * fact(f)(a-1)
}

println(fact(x => x)(4))

def fact_prod(n: Int) = product(x => x)(1,n)
println(fact_prod(5))


def map_reduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero
    else combine(f(a), map_reduce(f, combine, zero)(a+1,b))
}

def product_red(f: Int => Int)(a: Int, b: Int): Int = map_reduce(f, (x,y) => x*y, 1)(a,b)
def fact_red(n: Int) = product_red(x => x)(1, n)

println(product_red(x => x*x)(3,4))
println(fact_red(5))
