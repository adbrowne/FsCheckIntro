namespace FsCheckIntro

open FsCheck.Xunit

module Tests = 

    let rec quickSort = function
    | [] -> []
    | x::xs -> 
        let lhs = xs |> List.filter (fun a -> a < x)
        let rhs = xs |> List.filter (fun a -> a >= x)
        List.append (quickSort lhs) (x::(quickSort rhs))

    [<Property>]
    let ``quickSort is idempotent`` (xs : int list) =
        let sorted = quickSort xs
        sorted = quickSort sorted

    [<Property>]
    let ``quickSort matches Enumerable.OrderBy`` (xs : int list) = 
        let quickSorted = quickSort xs
        let systemSorted = 
            System.Linq.Enumerable.OrderBy(xs, fun x -> x) 
            |> List.ofSeq
        quickSorted = systemSorted