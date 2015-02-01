namespace FsCheckIntro

open FsCheck
open FsCheck.Xunit

type NegativeNumber = NegativeNumber of int

type ListOfPositiveIntegers = ListOfPositiveIntegers of int list

type MyArbitraryInstances =
    static member NegativeNumber() =
        Arb.from<int>
        |> Arb.filter (fun x -> x < 0)
        |> Arb.convert (NegativeNumber) (fun (NegativeNumber i) -> i)

    static member ListOfPositiveIntegers() =
        let rec nextValue = function
        | 0 -> gen { return [] }
        | x -> gen {
            let! x = Gen.suchThat (fun x -> x > 0) Arb.generate 
            let! rest = (nextValue <| x - 1)
            return x :: rest
        }
        
        Gen.sized nextValue
        |> Arb.fromGen
        

[<Arbitrary(typeof<MyArbitraryInstances>)>]
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

    let abs_x_not_x x = 
        printfn "value: %d" x
        abs x <> x

    [<Property(Verbose = true)>]
    let ``abs of a negative number is not equal to itself 1`` (x : int) =
        (x < 0) ==> abs_x_not_x x

    [<Property>]
    let ``abs of a negative number is not equal to itself 2`` () =
        let negativeNumber =
            Arb.from<int>
            |> Arb.filter (fun x -> x < 0)

        Prop.forAll negativeNumber abs_x_not_x

    [<Property>]
    let ``abs of a negative number is not equal to itself 3`` (NegativeNumber x) =
       abs_x_not_x x

    [<Property>]
    let ``all positive`` (ListOfPositiveIntegers xs) =
        xs |> List.forall (fun x -> x > 0)

    [<Property>]
    let ``all positive categorized`` (ListOfPositiveIntegers xs) =
        xs |> List.forall (fun x -> x > 0)
        |> Prop.classify (xs = []) "Empty list"
        |> Prop.classify (xs <> []) "Non Empty list"
        |> Prop.classify (xs |> List.length > 10) "More than 10 elements"