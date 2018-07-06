include (Base.Type_equal : module type of Base.Type_equal
         with module Id := Base.Type_equal.Id)

module Id = struct
  include (Base.Type_equal.Id : module type of Base.Type_equal.Id
           with module Uid := Base.Type_equal.Id.Uid)

  module Uid = struct
    module Upstream = Base.Type_equal.Id.Uid
    include Base.Type_equal.Id.Uid
    include Comparable.Extend (Upstream) (struct
        type t = Base.Type_equal.Id.Uid.t [@@deriving sexp]
      end)
    include Hashable.Make (Upstream)
  end
end
