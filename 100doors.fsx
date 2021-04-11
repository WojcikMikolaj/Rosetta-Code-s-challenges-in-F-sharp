let openDoors = ([1..100] |> List.map (fun n-> -n), 1)

let closeDoors (doors, i)=
    List.map (fun n-> if n%i=0 then -n else n) doors

let rec processDoors (doors, i)=
    printfn "%d" i
    [for d in doors -> printf "%d " d] |> ignore
    printfn ""
    match i with
    | 100 -> (doors, i) |> closeDoors |> List.filter (fun n -> n>0)
    | _  -> ((doors,i) |> closeDoors ,i+1) |> processDoors

let finalOpenDoors = processDoors openDoors