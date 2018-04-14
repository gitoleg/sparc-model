open Bap.Std

open Sparc_model


module Lifter = struct

end

module Target : Target = struct
  module CPU = Cpu
    let lift mem insn = failwith ""
end
