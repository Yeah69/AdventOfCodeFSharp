type Node = { 
    Position: int*int
    mutable North: Node option 
    mutable South: Node option 
    mutable West:  Node option
    mutable East:  Node option }
let initialNode = { Position = 0, 0; North = None; South = None; West = None; East = None }

let second = { Position = -1, 0; North = None; South = None; West = None; East = None }
initialNode.West <- Some second
second.East <- Some initialNode
let third = { Position = -1, -1; North = None; South = None; West = None; East = None }
second.North <- Some third
third.South <- Some second
let fourth = { Position = -1, 0; North = None; South = None; West = None; East = None }
third.East <- Some fourth
fourth.West <- Some third

let distanceMap = Map.empty |> Map.add initialNode 0
let needVisit = [ initialNode ]

let newDistanceMap =
    (distanceMap, needVisit)
    ||> Seq.fold (fun map node ->
        map |> Map.add node 1)

newDistanceMap |> Map.count

let array =
    seq { yield 0; yield 1 }
    |> Seq.sortBy (fun i -> i)
    |> Seq.append (seq {yield 2})
    |> Seq.pairwise
    |> Seq.map (fun (first, second) -> first + second)
    |> Seq.toArray

printfn "%d" (array |> Array.length)
