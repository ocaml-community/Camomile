[@@@ocaml.deprecated
"this module is deprecated, please update to the most recent camomile API"]

include Camomile
module CharEncoding = CamomileLib.CharEncoding
module UCharInfo = CamomileLib.UCharInfo
module UNF = CamomileLib.UNF
module UCol = CamomileLib.UCol
module CaseMap = CamomileLib.CaseMap
module UReStr = CamomileLib.UReStr
module StringPrep = CamomileLib.StringPrep

module ConfigInt = struct
  module type Type = CamomileLib.Config.Type
end
