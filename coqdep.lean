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


import Lean.Data.HashMap
import Lean.Data.HashSet

structure RocqFile where
  path : List String
  name : String
  type : String
  deriving Repr, Hashable, BEq

instance : ToString RocqFile where
  toString f := s!"[{f.path}, {f.name}, {f.type}]"

def parseFile (l : String) : RocqFile :=
  let fext_sep := String.split l (BEq.beq '.')
  let fbody_sep := String.split fext_sep[0]! (BEq.beq '/')
  RocqFile.mk (List.dropLast fbody_sep) (List.getLast! fbody_sep) (fext_sep[1]!)

abbrev DepGraph := Lean.HashSet (RocqFile × RocqFile)


def parse_graph (l : String)  :=
  let l_sep := String.split l (BEq.beq ':')
  let l_of := String.split l_sep[0]! (BEq.beq ' ')
  let l_to := List.filter (fun s => s.length > 0) <| String.split l_sep[1]! (BEq.beq ' ')
  let of_parsed := parseFile <$> l_of
  let to_parsed := parseFile <$> l_to
  of_parsed.bind <| fun of => to_parsed.map <| fun to => (to, of)

instance : ToString DepGraph where
  toString x := List.foldl (fun acc s => s!"{acc}\n{s.1} -> {s.2}") "" x.toList



-- def depgraph.nodes (d : DepGraph) : List RocqFile :=
--   (d.filter (fun g => g.1.type == "v" && g.2.type == "v")).map (fun g => g.1)


def dbg_path (x : List String) : String :=
  (List.foldl (fun acc s => s!"{acc}_{s}") "" x).drop 1

def dbg_node (x : RocqFile) : String := s!"{dbg_path x.1}_{x.2}_{x.3}"

--
-- def depgraph.from_vo (d : RocqFile × RocqFile) : Bool := d.1.type == "vo"
-- def depgraph.to_vo (d : RocqFile × RocqFile) : Bool := d.2.type == "vo"

def DepGraph.nodes (d : DepGraph) : Lean.HashSet RocqFile :=
  let all_nodes := d.toList.map (fun x => x.1) ++ d.toList.map (fun x => x.2)
  Lean.HashSet.insertMany Lean.HashSet.empty all_nodes

def all_paths (h : Lean.HashSet RocqFile) : Lean.HashSet (List String) :=
  h.fold (fun acc f => acc.insert f.1) Lean.HashSet.empty


-- Dependency graph (removes self-dependencies)
def DepGraph.points_to (d : DepGraph) : Lean.HashMap RocqFile (Lean.HashSet RocqFile) :=
  d.fold
    (fun acc e =>
      if e.1 == e.2 then acc else
      match Lean.HashMap.find? acc e.1 with
      | some lsf => acc.insert e.1 (Lean.HashSet.insert lsf e.2)
      | none => acc.insert e.1 (Lean.HashSet.insert Lean.HashSet.empty e.2))
    Lean.HashMap.empty

-- The list of all children
def children (m : Lean.HashMap RocqFile (Lean.HashSet RocqFile)) (n : RocqFile) : List RocqFile :=
  match Lean.HashMap.find? m n with
  | some l => l.toList
  | none => []

def is_concrete (n : RocqFile) : Bool := n.type == "v"

def to_concrete (n : RocqFile) : RocqFile := ⟨ n.path, n.type, "v" ⟩

-- -- get all downstream dependencies that are real .v files
-- partial def concrete_descendents (m : Lean.HashMap RocqFile (Lean.HashSet RocqFile)) (n : RocqFile) : List RocqFile :=
--   let v :=
--     (children m n).foldl
--       (fun acc c =>
--         if acc.2.contains c then acc else acc
--         -- if is_concrete c
--         --   then acc ++ [c]
--         --   else acc ++ concrete_descendents m c
--         )
--       ([], [])
--   v.1

-- FIXME: Remove IO
def depgraph.to_graph (g : DepGraph) : IO String := do
  let n := g.nodes
  let p := all_paths n

  let render_subgraph (s : List String) : String :=
    let content : String := n.fold
      (fun acc nod => if (nod.1 == s) then s!"{acc}{dbg_node nod};\n" else acc) ""
    -- s!"subgraph cluster_{dbg_path s} \{\n{dbg_path s}\n{content}}\n"
    s!"subgraph cluster_{dbg_path s} \{\n{content}}\n"
  let subgraphs : String := p.fold (fun acc s => s!"{acc}{render_subgraph s}") ""

  let render_edge (d : (RocqFile × RocqFile)) : String :=
    if (d.1.path == d.2.path)
      then s!"{dbg_node d.1} -> {dbg_node d.2};\n" -- render the within-cluster edges
      else ""
  let edges : String := g.fold (fun acc s => s!"{acc}{render_edge s}") ""

  -- Calculate inter-cluster edges
  let g_ic_edges : Lean.HashSet (List String × List String) :=
    g.fold
      (fun acc s =>
        if s.1 == s.2 then acc else
        acc.insert (s.1.1, s.2.1))
      Lean.HashSet.empty
  let render_ic_edge (d : List String × List String) : String :=
    s!"{dbg_path d.1} -> {dbg_path d.2} [ltail=cluster_{dbg_path d.1}, lhead=cluster_{dbg_path d.2}, weight=0.1] ;\n"
  let ic_edges : String := g_ic_edges.fold (fun acc s => s!"{acc}{render_ic_edge s}") ""

  let overview : String :=
    let content : String := p.fold (fun acc pat => s!"{acc}{dbg_path pat};\n" ) ""
    -- s!"subgraph cluster_{dbg_path s} \{\n{dbg_path s}\n{content}}\n"
    s!"subgraph overview \{\n{content}}\n"

  return s!"digraph \{\ncompound=true;\noverlap=false;\n{overview}\n{ic_edges}\n{subgraphs}\n{edges}\n}\n"


def main (args : List String) : IO Unit := do
  let path : IO String <-
    match args[0]? with
    | (some p) => pure p
    | none => throw <| IO.userError "please provide the path of _CoqProject as first argument"
  let g : DepGraph := Lean.HashSet.insertMany Lean.HashSet.empty <| (<- IO.FS.lines path).toList.bind parse_graph

  -- Simplify g by
  --    - removing self-edges
  --    - collapsing all X.* nodes into X
  --    - if
  let g2 : DepGraph :=
    g.fold (fun acc s =>
      if (s.1.path != s.2.path)
        then acc.insert (⟨ s.1.path, s.1.name, "v" ⟩, ⟨ s.2.path, s.2.name, "v" ⟩)
        else if s.1.name == s.2.name
          then acc -- acc.insert (to_concrete s.1, to_concrete s.2) -- Edge within cluster
          else acc.insert (⟨ s.1.path, s.1.name, "v" ⟩, ⟨ s.2.path, s.2.name, "v" ⟩)
    )
    Lean.HashSet.empty

  -- -- Simplify the graph
  -- let pt := g.points_to
  -- let n := g.nodes


  -- for nod in n do
  --   IO.println s!"{nod}"
    -- IO.println s!"{(concrete_descendents pt nod).length}\n"

  IO.FS.writeFile "./out.dot" (<- depgraph.to_graph g2)
  IO.println "done"





  -- To start, let's just render out the entire graph

  -- for x in g do
  --   -- IO.println s!"{x.2.path}"
  --   -- IO.println s!"./iris/{dbg_path x.1}/{x.2}.{x.3}"
  -- -- IO.println s!"{g.nodes.length}"

  -- IO.println s!"{g.filter (fun d => d.1.type == "v" && d.2.type == "v")}"

  -- There seems to be:
  -- X.v -> X.vo
  -- X.v -> X.glob
  -- X.v -> X.required_vo
  -- X.v -> X.v
  -- for all X

  -- X.v -> X.v are the only .v -> .v dependencies, but not all

  -- IO.println s!"done!"
