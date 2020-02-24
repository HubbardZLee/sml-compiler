fun main(x:int):int = let y = ref(x:int) in y := (!y)+1; !y
