namespace Myriad.Plugins

open System
open System.Text.Json
open System.Text.Json
open FSharp.Compiler.SyntaxTree
open Fantomas
open FsAst
open Myriad.Core
open System.IO

open Avro.FSharp


module Generator =
    type AVROAttribute(filePath : string) =
        inherit Attribute()
        let mutable _filePath = filePath
        member this.FilePath = filePath
        new () = AVROAttribute(null : string)
        //new (``type``: Type) = ANTLRAttribute(``type``.Name)

    let typeFromSchema (schema : Schema) =
        let rs =
            match schema with
            | Schema.Record(rs) -> rs
            | s -> printfn "%O" s; failwithf "match on schema: %O" s
            
        //SynModuleDecl.CreateType (SynComponentInfoRcd.Create(0L),SynMemberDefns.Cons (SynMemberDefn.) SynMemberDefns.Empty)
        let name = rs.Name.Split('.') |> Array.last
        printfn "EXECUTING SCHEMA %s" name
        let myRecordId = LongIdentWithDots.CreateString name
        let recordCmpInfo = SynComponentInfoRcd.Create(myRecordId.Lid)
        
        let recordDef =
            rs.Fields
            |> Seq.map (fun recordField -> 
                let field =
                    match recordField.Type with
                    | Schema.String -> SynType.String()
                    | Schema.Int -> SynType.Int()
                    | Schema.Boolean -> SynType.Bool()
                    | Schema.Double -> SynType.Float()
                    | Schema.Decimal(ds) -> SynType.Decimal()
                    | Schema.Bytes ->
                         let b = SynType.Create("byte")
                         SynType.Array(0, b, FSharp.Compiler.Range.range.Zero)
                    | Schema.Long -> SynType.Int64()
                    | s -> failwithf "unknown schema %O" s

                SynFieldRcd.Create(Ident.Create(recordField.Name), field))
            
            |> Seq.toList
            |> SynTypeDefnSimpleReprRecordRcd.Create
            |> SynTypeDefnSimpleReprRcd.Record
            
        SynModuleDecl.CreateSimpleType(recordCmpInfo, recordDef)
    
//    let toFormattedCode (cfg: Config) (comment: string) (generatedModule: SynModuleOrNamespaceRcd) = 
//        let parsedInput = 
//            ParsedInput.CreateImplFile(
//                ParsedImplFileInputRcd.CreateFs(cfg.OutputFile).AddModule generatedModule)
//    
//        let cfg = { FormatConfig.FormatConfig.Default with StrictMode = true }
//        let formattedCode = CodeFormatter.FormatASTAsync(parsedInput, "output.fs", [], None, cfg) |> Async.RunSynchronously
//    
//        let formattedCodeWithComment =
//            [   comment
//                formattedCode ]
//            |> String.concat System.Environment.NewLine
//
//        formattedCodeWithComment
    
    
[<MyriadGenerator("AVRO")>]
type AVROGenerator() =

    interface Myriad.Core.IMyriadGenerator with
        member __.ValidInputExtensions = seq { ".fs" }
        member __.Generate(ctx : GeneratorContext) : SynModuleOrNamespaceRcd list =
            let namespace' = "Generated"
            let ast =
                Ast.fromFilename ctx.InputFilename
                |> Async.RunSynchronously
                |> Array.head
                |> fst
            let namespaceAndRecords = Ast.extractTypeDefn ast

            let modules : (SynAttribute*SynTypeDefn) list =
                namespaceAndRecords
                |> List.collect (fun (_, dus) ->
                                    dus
                                    |> List.filter (Ast.hasAttribute<Generator.AVROAttribute>)
                                    |> List.map (fun x -> x |> Ast.getAttribute<Generator.AVROAttribute> |> Option.get, x))

            let types =
                seq {
                    for (attribute,expr) in modules do
                        let folderPath =
                            match attribute.ArgExpr with
                            | SynExpr.Paren(SynExpr.Const(SynConst.String(path,_),_),_,_,_) -> path
                            | s -> failwithf "ArgExpr: %O" attribute.ArgExpr
                        let files =
                            Directory.EnumerateFiles folderPath
                            |> Seq.filter (fun x -> x.EndsWith(".avsc"))
                        for filePath in files do
                            let fileName = (filePath.Split("\\") |> Array.rev).[0].Split(".").[0]
                            printfn "EXECUTING FILE %s" fileName
                            let json = File.ReadAllText(filePath)
                            let jdoc = JsonDocument.Parse(json)
                            let typelist =
                                if jdoc.RootElement.ValueKind = JsonValueKind.Array then
                                    jdoc.RootElement.EnumerateArray()
                                    |> Seq.choose (fun jEl -> 
                                        let raw = jEl.GetRawText()
                                        try 
                                            let schema = Schema.ofString raw
                                            let record = Generator.typeFromSchema schema
                                            Some(record)                                         
                                        with
                                        | e -> printfn "%s || %O" e.Message e; None
                                        )
                                    |> Seq.toList
                                else if jdoc.RootElement.ValueKind = JsonValueKind.Object then
                                    try 
                                        let schema = Schema.ofString (jdoc.RootElement.GetRawText())
                                        let record = Generator.typeFromSchema schema
                                        [record]
                                            
                                    with
                                    | e -> printfn "%s || %O" e.Message e; []
                                else
                                    []
                            yield SynModuleDecl.CreateNestedModule(SynComponentInfoRcd.Create(Ident.CreateLong fileName),typelist)
                            
                } |> Seq.toList
//                modules
//                |> List.collect (fun (attribute,expr) -> 
//                    let folderPath =
//                        match attribute.ArgExpr with
//                        | SynExpr.Paren(SynExpr.Const(SynConst.String(path,_),_),_,_,_) -> path
//                        | s -> failwithf "ArgExpr: %O" attribute.ArgExpr
//                    let files =
//                        Directory.EnumerateFiles folderPath
//                        |> Seq.filter (fun x -> x.EndsWith(".avsc"))
//                    List.collect
//                    let json = File.ReadAllText(filePath)
//                    let jdoc = JsonDocument.Parse(json)
//                    let typelist =
//                        jdoc.RootElement.EnumerateArray()
//                        |> Seq.map (fun jEl -> 
//                            let raw = jEl.GetRawText()
//                            let schema = Schema.ofString raw
//                            let record = Generator.typeFromSchema schema
//                            record)
//                        |> Seq.toList
//                    typelist)

            let namespaceOrModule =
                {SynModuleOrNamespaceRcd.CreateNamespace(Ident.CreateLong namespace')
                    with
                        IsRecursive = true
                        Declarations = types }

            [namespaceOrModule]