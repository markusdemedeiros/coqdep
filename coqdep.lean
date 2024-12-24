-- #check IO.FS.Handle.getLine


-- inductive RocqFileType
-- | v
-- | vo
-- | vio
--   deriving Repr, Inhabited

-- instance : ToString RocqFileType where
--   toString f :=
--     match f with
--     | RocqFileType.v => "v"
--     | RocqFileType.vo => "vo"
--     | RocqFileType.vio => "vio"

-- def parseRocqFileType : String -> Option RocqFileType
-- | "v"  => some RocqFileType.v
-- | "vo"  => some RocqFileType.vo
-- | "vio"  => some RocqFileType.vio
-- | _ => none

structure RocqFile where
  path : List String
  name : String
  type : String
  deriving Repr

instance : ToString RocqFile where
  toString f := s!"[{f.path}, {f.name}, {f.type}]"

def parseFile (l : String) : RocqFile :=
  let fext_sep := String.split l (BEq.beq '.')
  let ft := fext_sep[1]!
  let fbody_sep := String.split fext_sep[0]! (BEq.beq '/')
  RocqFile.mk (List.dropLast fbody_sep) (List.getLast! fbody_sep) ft


abbrev depgraph := List (RocqFile × RocqFile)

def parse_graph (l : String) : depgraph :=
  let l_sep := String.split l (BEq.beq ':')
  let l_of := String.split l_sep[0]! (BEq.beq ' ')
  let l_to := List.filter (fun s => s.length > 0) <| String.split l_sep[1]! (BEq.beq ' ')
  let of_parsed := parseFile <$> l_of
  let to_parsed := parseFile <$> l_to
  -- let rs : List (RocqFile × RocqFile) :=
  of_parsed.bind <| fun of => to_parsed.map <| fun to => (to, of)
  -- let x := List.foldl (fun acc s => s!"{acc}\n{s.1} -> {s.2}") ""  rs

instance : ToString depgraph where
  toString _ := "my graph"


def main (args : List String) : IO Unit := do
  -- Read the command line argument
  let path : IO String <-
    match args[0]? with
    | (some p) => pure p
    | none => throw <| IO.userError "please provide the path of _CoqProject as first argument"
  -- let fd : IO.FS.Handle <- IO.FS.Handle.mk path IO.FS.Mode.read
  let g : depgraph := (<- IO.FS.lines path).toList.bind parse_graph
  IO.println s!"{g}"

  -- for l in (<- IO.FS.lines path) do
  --   let g := parse_graph l
  --   -- IO.println s!"{cleanup l}"
  IO.println s!"done!"

  -- let path : String :=



    -- match (args[0]? : Option String) with
    -- | some p => ""
    -- | none => do
    --
    --   return ""



  -- let path <- if let (some path) := (args[0]?) then return path
  --  else
  --    return
