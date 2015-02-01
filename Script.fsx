#r "./packages/FsCheck.1.0.4/lib/net45/FsCheck.dll"
open FsCheck

let rec quickSort = function
| [] -> []
| x::xs -> 
    let lhs = xs |> List.filter (fun a -> a < x)
    let rhs = xs |> List.filter (fun a -> a >= x)
    List.append (quickSort lhs) (x::(quickSort rhs))

let quickSortIsIdempotent (xs : int list) =
    let sorted = quickSort xs
    sorted = quickSort sorted

let quickSortMatchesEnumerableOrderBy (xs : int list) = 
    let quickSorted = quickSort xs
    let systemSorted = 
        System.Linq.Enumerable.OrderBy(xs, fun x -> x) 
        |> List.ofSeq
    quickSorted = systemSorted

Check.Quick quickSortIsIdempotent 
Check.Quick quickSortMatchesEnumerableOrderBy 

let replayConfig = 
    { Config.Verbose with 
        Config.Replay = Some <| (Random.StdGen (1499822617,295966976)) }
Check.One (replayConfig, quickSortMatchesEnumerableOrderBy)