module Log

type Command = Cd of string | Ls
type LogLine = 
    | Command of Command
    | Dir of string 
    | File of string * int 

let private parseLogLine (line: string) = 
    match line.Split ' ' with
        | [| "$"; "cd"; dir |] -> Command (Cd dir)
        | [| "$"; "ls" |]      -> Command Ls
        | [| "dir"; name |]    -> Dir name
        | [| fileSize; name |] -> File (name, int fileSize)
        | _ -> failwith "corrupted log"

let parseLog: string seq -> LogLine seq = Seq.map parseLogLine