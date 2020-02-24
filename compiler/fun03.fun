fun f(x:int ref): int = (!x)+1
fun main(x:int):int = f(ref(x:int))
