module Lecture3

type 't tree =
| Nil
| Node of 't * 't tree * 't tree

let Leaf x = Node(x, Nil, Nil)

let t = Node(0, Leaf 1, Node(2, Leaf 3, Nil))

let ex = Node('+', Leaf('1'), Node('*', Leaf('1'), Leaf('2')))

//type trav_order = Prefix | Infix | Postfix

let Prefix n l r = n(); l(); r()
let Infix n l r = l(); n(); r()
let Postfix n l r = l(); r(); n();

let rec traverse tro f = function
| Nil -> ()
| Node(x, l, r) -> 
    tro (fun() -> f x) (fun() -> traverse tro f l) (fun() -> traverse tro f r)
    
traverse Infix (printf "%A") ex

let rec fold f acc = function
| Nil -> acc
| Node(x, l, r) -> 
        let x1 = fold f acc l
        let x2 = f x x1
        fold f x2 r

fold (fun a b -> string(a)+b) "" ex

let rec insert x = function
| Nil -> Leaf(x)
| Node(z,l,r) as N ->
    if x < z then Node(z, insert x l, r)
    elif x > z then Node(z, l, insert x r)
    else N


let Rnd = new System.Random()

let L = [for x in [1..10] -> Rnd.Next(1, 100)]

let flip f x y = f y x
let curry f x y = f(x,y)
L |> List.fold (flip insert) Nil |> fold (curry List.Cons) []



let rec insert2 x = function
| Nil -> Leaf(x, 1)
| Node((z,n),l,r) as N ->
    if x < z then Node((z,n), insert2 x l, r)
    elif x > z then Node((z,n), l, insert2 x r)
    else Node((x,n+1), l, r)

open System.IO
open FSharp.Charting
open System.Numerics

File.ReadAllLines(@"C:\Users\Admin\source\repos\linguistic-analysis\WebApp1\wwwroot\Content\new_article.txt")
|> Array.map (fun s -> s .ToLower())
|> Array.collect (fun s -> s.Split([|' ';'-';',';'.';';';'?';'!';'(';')';'[';']'|]))
|> Array.filter (fun x -> x.Length > 0)
|> Array.fold (flip insert2) Nil
|> fold (curry List.Cons) []
|> List.sortBy ((~-)<<snd)
|> List.take 5
|> Chart.Bar

let text =
    File.ReadAllLines(@"C:\Users\Admin\source\repos\linguistic-analysis\WebApp1\wwwroot\Content\new_article.txt")
    |> Array.map (fun s -> s .ToLower())
    |> Array.collect (fun s -> s.Split([|' ';'-';',';'.';';';'?';'!';'(';')';'[';']'|]))
    |> Array.filter (fun x -> x.Length > 0)
    |> Array.fold (flip insert2) Nil

let rec size = function
|Nil -> 0
|Node(_,l,r) -> 1+size l+size r

5 |> (+)1 |> (*)2 |> printf "%d"

let plus1 x f = f(x+1)
let times2 x f = f(x*2)

plus1 5 (fun x -> times2 x (printf "%d"))

let rec len f = function
| [] -> f 0
| _::t -> len ((+)1 >> f) t

len id [1..10]

let rec size2 f = function
|Nil -> f 0
|Node(_,l,r) -> 
    l |> size2 (fun x1 ->
        r |> size2(fun x2 -> f (1+x1+x2)))

size2 id text

// Closure

let create_generator f x =
    let mutable c = x
    fun() -> 
        c <- f c; c

let fibgen = create_generator (fun (x,y) -> (x+y,x)) (1,1)

let map f g =
    fun () -> f(g())

let fibs = fibgen |> map fst

let rec filter f g =
    fun () ->
        let x = g()
        if f x then x
        else (filter f g)()
        
let f = fibgen |> map fst |> filter (fun x -> x>1000000)

f()

Seq.unfold (fun (n, acc) -> Some(acc*n,(n+1, n*acc))) (1,1)

let fact = (..) 1 >> Seq.reduce (*)

let fact2 n = Seq.initInfinite(id) |> 
    Seq.map ((+)1) |> 
    Seq.scan (*) 1 |> 
    Seq.item n

// Co-Recursion and Co-Data

let rec ones = seq{
    yield 1
    yield! ones
    }

let rec nat = seq{
    yield 1
    yield! Seq.map ((+)1) nat
    }