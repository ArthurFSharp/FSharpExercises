let strToInt str =
    if System.Text.RegularExpressions.Regex.IsMatch(str, "^[0-9]+$")
        then Some(int str)
    else
        None

let strAdd str i =
    let num = str |> strToInt
    match num with
    | None -> None
    | Some(x) -> Some(x + i)

let (>>=) m f =
    match m with
    | None -> None
    | Some x ->
        x |> f

type MyBuilder() =
    member this.Bind(m, f) = Option.bind f m
    member this.Return(x) = Some x

let yourWorkflow = new MyBuilder()

let stringAddWorkflow x y z =
    yourWorkflow
        {
            let! a = strToInt x
            let! b = strToInt y
            let! c = strToInt z
            return a + b + c
        }

let good = stringAddWorkflow "12" "3" "2"
let bad = stringAddWorkflow "12" "xyz" "2"

let goodWithBind = strToInt "1" >>= strAdd "2" >>= strAdd "3"
let badWithBind = strToInt "1" >>= strAdd "xyz" >>= strAdd "3"