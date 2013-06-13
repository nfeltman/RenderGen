structure GeneratedRenderer = struct
open Runtime
fun go () = runExperiment root
and root x =
let val x = (#1 x, splitOneS (#2 x)) in
let val g = #1 x
fun f s = (printCounter (); let val x = (g,s) in
let val x = alpha x in
x
end
end)
val x = mapReduce f reduceS (#2 x) in
x
end
end

and alpha x =
let val x = ((calcBound (#1 x), #1 x), #2 x) in
if passesIsectTest x then
let val x = (#2 (#1 x), #2 x) in
if sizeG (#1 x) >= 2 then
let val x = (splitTwoGP (#1 x), #2 x) in
let val s = #2 x
fun f g = let val x = (g,s) in
let val x = alpha x in
x
end
end
val x = mapReduce f reduceG (#1 x) in
x
end
end
else
let val x = hit x in
x
end
end
else dummy (#2 x)
end

end