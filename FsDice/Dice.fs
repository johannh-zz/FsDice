module FsDice.Dice

module Dist = FsDice.Dist
open FsDice.Prob

/// Add distributions.  Restricts to int.  Make inline?
let add dx dy = Dist.outer (+) dx dy

/// Example of using the prob computation expression.
let addProb dx dy =
    prob { let! x = dx
           let! y = dy
           return x + y }

let rec repeatAndSum d n =
    match n with
    | x when x < 1 -> failwith "n must be >0"
    | 1 -> d
    | 2 -> add d d
    | _ -> let d2 = repeatAndSum d (n / 2)
           if n % 2 = 0 then add d2 d2
           else add d2 (add d2 d)
        
// Add a constant to a distribution.  Restricts to int.  Make inline?
let addInt d i = Dist.fmapInjective ((+) i) d

let printIntType outputType d =
    Dist.printGen 0.0005 (fun p -> sprintf "%5.1f" (p * 100.0)) (sprintf "%5d") outputType d

let printInt d = printIntType Dist.Exactly d

let rec repeatAndSumProb d n =
    match n with
    | x when x < 1 -> failwith "n must be >0"
    | 1 -> d
    | 2 -> addProb d d
    | _ -> let d2 = repeatAndSumProb d (n / 2)
           if n % 2 = 0 then addProb d2 d2
           else addProb d2 (addProb d2 d)

let dN n = Dist.uniform { 1..n }

let d4  = dN 4
let d6  = dN 6
let d8  = dN 8
let d10 = dN 10
let d12 = dN 12

let rec exploder maxTimes explodeOn d =
    if maxTimes <= 0 then d
    else
        d |> Dist.bind (fun di ->
            if di = explodeOn then
                addInt (exploder (maxTimes - 1) explodeOn d) explodeOn
            else Dist.just di)

let ed4  = exploder 5 4  d4
let ed6  = exploder 5 6  d6
let ed8  = exploder 5 8  d8
let ed10 = exploder 5 10 d10
let ed12 = exploder 5 12 d12

let wild x =
    Dist.outer max ed6 x