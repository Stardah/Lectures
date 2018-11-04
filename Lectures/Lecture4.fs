module Lecture4

open System.Collections.Generic

let x = lazy (3+2)

let y = x.Force()
 // memorization

let bind a f =
    if a = None then None
    else f (a.Value)

    
let (>>=) a f =
    if a = None then None
    else f (a.Value)

let inv x = 
    if x<>0. then Some(1./x)
    else None


type Nondet<'t> = 't list

let (>>>=) mA f = List.collect f mA

[7;8] >>>= (fun x-> [x*2;x*3]) >>>= (fun x -> [x+1])

// Monadic Function Composition

let (>=>) f g = fun x -> (f x >>= g)

//>=>: ('a -> M<'b>) -> ('b -> M<'c>) -> ('a -> M<'c>)

//-------- Определение категорий
// Функтор - отображение двух категорий K1 and K2, 
// функтор сохраняет свойства композиции


type NondetBuilder() =
    member b.Return(x) = [x]
    member b.ReturnFrom(x) = x
    member b.Bind(mA, f) = List.collect f mA

let nondet = new NondetBuilder()

let r = nondet {
        let! vasya = [7;8]
        let! petya = [2*vasya; 3*vasya]
        let lena = petya + 1
        return lena
}


//-------------

let books = Dictionary.GetFiles @"C:\books"
    |> Seq.filter(fun s -> s.EndsWith(".txt"))

