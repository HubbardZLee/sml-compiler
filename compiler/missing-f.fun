fun g(x:<int, int, int, int>):<int, int, int, int, int, int> = <#0 x, #1 x, #2 x, #3 x, 5, 6>

fun main(x:int):int =
  let h = if x then g else f in
  let ARG = <x, x, x, x> in
  let v = h(ARG) in #1 v
