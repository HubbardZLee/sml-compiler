fun main(x:int):int = let y = ref(x:int) in (while (!y) do y := (!y)-111); 0
