(* Charset conversion alone, with only the data it needs embedded:
   CharEncoding reads charmapdir and unimapdir and nothing else, so
   database/ and locales/ are left out. Use camomile-embedded for the
   full API. *)

module Config : CamomileLib.Config.Type = struct
  let datadir = "database"
  let charmapdir = "charmaps"
  let unimapdir = "mappings"
  let localedir = "locales"

  let get path =
    let data =
      match (Filename.dirname path, Filename.basename path) with
        | "charmaps", name -> Charmaps_data.read name
        | "mappings", name -> Unimaps_data.read name
        | _ -> None
    in
    match data with
      | Some data -> Marshal.from_string data 0
      | None -> raise Not_found
end

module CharEncoding = CamomileLib.CharEncoding.Configure (Config)
