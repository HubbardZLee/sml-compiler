fun g(x:int):int = x
fun f(x: int->int): int = x(0)
fun main(arg:int): int =  f(g)



