namespace FsDice

/// Probability distribution.
type Dist<'T> when 'T : comparison = Map<'T, float>

module Dist =
    /// Convert a sequence of (value, probability) pairs into a distribution.
    let ofSeq (s: seq<'T * float>) : Dist<'T> =
        s |> Seq.groupBy fst
          |> Seq.map (fun (x, xps) -> (x, Seq.sumBy snd xps))
          |> Map.ofSeq

    /// Monad return.
    let just x : Dist<'T> = Map.empty.Add(x, 1.0)

    /// Monad bind.
    let bind (f: 'T -> Dist<'U>) (d: Dist<'T>) =
        seq { for KeyValue(x, px) in d   do
              for KeyValue(y, py) in f x do
              yield (y, px * py) }
        |> ofSeq

    /// Map a function across the values.
    let fmap f (d: Dist<'T>) =
        seq { for KeyValue(x, px) in d -> (f x, px) } |> ofSeq

    // If we know the function is injective, we don't need to simplify.
    let fmapInjective f (d: Dist<'T>) : Dist<'U> =
        seq { for KeyValue(x, px) in d -> (f x, px) } |> Map.ofSeq

    /// Outer product of two distributions.  This could be implemented
    /// with bind, but then we'd simplify at each step, rather than just
    /// once at the end.
    let outer f (dx: Dist<'T>) (dy: Dist<'U>) =
        seq { for KeyValue(x, px) in dx do
              for KeyValue(y, py) in dy do
              yield (f x y, px * py) }
        |> ofSeq

    let uniform xs =
        let p = 1.0 / float (Seq.length xs)
        xs |> Seq.map (fun x -> (x, p)) |> ofSeq

    let outcomes (d: Dist<'T>) =
        Map.toSeq d |> Seq.map fst

    let probs (d: Dist<'T>) =
        Map.toSeq d |> Seq.map snd

    let cumprobs (d: Dist<'T>) =
        probs d |> Seq.scan (+) 0.0

    let cumprobSeq (d: Dist<'T>) =
        Seq.zip (outcomes d) (cumprobs d |> Seq.skip 1)

    let choose (r: System.Random) (d: Dist<'T>) =
        let rp = r.NextDouble()
        cumprobSeq d |> Seq.find (fun (x, cp) -> cp >= rp) |> fst

    type OutputType = Exactly | AtLeast | AtMost

    // Generic distribution printer.
    let printGen minProb probFormatter valueFormatter outputType (d: 'a Dist) =
        let probsToDisplay =
            match outputType with
            | Exactly -> probs d
            | AtLeast -> cumprobs d |> Seq.map ((-) 1.)
            | AtMost  -> cumprobs d |> Seq.skip 1
        Seq.zip (outcomes d) probsToDisplay
        |> Seq.iter (fun (v, p) ->
            if p >= minProb then
                printfn "%s: %s" (valueFormatter v) (probFormatter p))

    let print (d: 'a Dist) =
        d |> printGen 0.0005 (fun p -> sprintf "%5.1f" (p * 100.0)) (sprintf "%A") Exactly

// Is there a better place to put this?
module Prob =
    // Computation expression
    type ProbBuilder() =
        member this.Bind(d, f) = Dist.bind f d
        member this.Return(x) = Dist.just x
        member this.ReturnFrom(d) = d

    let prob = ProbBuilder()
