
fun f(x: int ref): int = !x
fun main(arg:int): int =  f(ref (arg:int))


end
