module Config : CamomileLib.Config.Type = struct
  let datadir = "database"
  let charmapdir = "charmaps"
  let unimapdir = "mappings"
  let localedir = "locales"

  let get path =
    match Embedded_data.read path with
      | Some data -> Marshal.from_string data 0
      | None -> raise Not_found
end

module Camomile = Camomile.Make (Config)
