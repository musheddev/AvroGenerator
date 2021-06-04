// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open Myriad.Plugins
open System


// Define a function to construct a message to print
[<Generator.AVRO("C:\Users\Orlando\Desktop\Projects2021\AvroGenerator\TestProject\schemas")>]
type Dummy9 = unit

[<EntryPoint>]
let main argv =
    let message ="F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code