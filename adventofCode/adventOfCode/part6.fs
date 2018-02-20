module part6

open System.IO


type Item = int*int
type Message = Item seq

let inputFile = "problem6.txt"

let parseFile (input: string) : Message = 
    File.ReadAllText(input)
        |> fun fileString -> fileString.Split()
        |> Array.toSeq
        |> Seq.map System.Int32.Parse 
        |> Seq.mapi (fun i v -> i, v)
        
//parseFile inputFile

let findMax (fullMap: Message) : Item =
    let folder ((key1, val1): Item) ((key2, val2): Item) : Item =
        if val1 >= val2 then 
            key1, val1 
        else 
            key2, val2
    
    Seq.fold folder (0, 0) fullMap
    
//[(0, 13); (1, 6); (2, 20);] |> findMax

let filterer ((key1, _): Item) ((key2, _): Item) : bool = 
    key1 = key2
        
//filterer (2, 5) (2, 10)
//filterer (2, 5) (3, 5)

let shift (offset: int) (items: Message) : Message = 
    let first = 
        items
            |> Seq.filter (fun (key, _) -> key > offset)
    
    let second = 
        items
            |> Seq.filter (fun (key, _) -> key <= offset)
            
    Seq.append first second
    
//shift (2) (Seq.ofList [(0, 0); (1, 1); (2, 2); (3, 3); ])

let rec pluser (countdown: int) (items: Message) : Message = 
    if countdown = 0 then
        items
    else
        let newHead = items |> Seq.head |> fun (key, value) -> (key, value + 1)
        let newMessage = Seq.append (Seq.tail items) (Seq.singleton newHead)
        pluser (countdown - 1) newMessage
        
//pluser 50 [(0, 0); (1, 1); (2, 2);]

let setToZero (key: int) (message: Message) : Message =
    let zeroed =
        message
            |> Seq.find (fun (x, _) -> x = key)
            |> fun (x, _) -> (x, 0)
            
    let filtered =
        message
            |> Seq.filter (fun (x, _) -> x <> key)
            
    Seq.append (Seq.singleton zeroed) filtered
        |> Seq.sortBy (fun (x, _) -> x)
        
//setToZero 0 [(0, 11); (1, 6); (2, 0);]

let advance (input: Message) : Message =

    let key, value = findMax input
    let zeroed = setToZero key input
    let shifted = shift key zeroed  
    
    shifted
    |> pluser value
    |> Seq.sortBy (fun (x, _) -> x)
    
//[(0, 11); (1, 6); (2, 0);] |> advance |> advance |> advance |> advance |> advance

let messageCheck (mess1: Message) (mess2: Message) : bool = 
    (Set.ofSeq mess1) = (Set.ofSeq mess2)
            
//messageCheck ([(0, 11); (1, 6); (2, 0);] |> Seq.ofList) ([(0, 11); (1, 6); (2, 0);] |> Seq.ofList)

let rec iterate (cache: Message seq) : int =
    
    let nextMessage = cache |> Seq.head |> advance
    
    match Seq.exists (messageCheck nextMessage) cache with
    | true ->
        Seq.length cache
    | false -> 
        cache
            |> Seq.append (Seq.singleton nextMessage)
            |> iterate

let answer = inputFile |> parseFile |> Seq.singleton |> iterate

let adventOfCode6Part1 (sourceString: string) : int =
    sourceString
        |> parseFile 
        |> Seq.singleton
        |> iterate
        
//let answer = adventOfCode6Part1 inputFile

//printf "%A" answer

