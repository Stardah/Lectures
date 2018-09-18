// Learn more about F# at http://fsharp.org

open System
(*
let rec ins x = function
    | [] -> [[x]]
    | y::ys as l ->
       *) 
        
let rec perm = function
    | []-> [[]]
    | x::xs ->
        perm xs |> List.collect(ins x)

let rec fold f i = function
    | [] -> i
    | x::xs -> f x (fold f i xs)            

fold (fun x acc -> "f("+string(x)+","+acc+")") "empty" [1;2;3]

let foldL f i L = 
    let rec fold' acc f = function
        | [] -> acc
        | x::xs -> fold' (f x acc) f xs
    fold' i f L

let rev L =
    let rec rev' acc = function 
        | [] -> acc
        | x::xs -> rev' (x::acc) xs
    rev' L
    (*
    let rec gen_line list n acc = 
        if n = 1 then list @ acc
        else gen_line list (n-1) ((list.Head.Item(n) + list.Head.Item(n-1))::acc)
    *)
    //list.Head.Item(p) + list.Head.Item(p-1)
let pascal k =
    let rec gen_pascal i (list:int list list) =
        if i = k then list
        else 
           let P = List.head list
           let L = [for p in 0..(P.Length-2) -> (P.[p] + P.[p+1])]
           let add = [(1::L)@[1]] @ list
           gen_pascal (i + 1) add    
    gen_pascal 1 [[1;1]]
        
pascal 5

printfn "%A" (pascal 3)

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
