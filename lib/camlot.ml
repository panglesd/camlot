module Text = struct
  module Line = struct
    type t = string
  end

  type t = Line.t list

  let length v = 0 (* TODO *)
end

module Changes = struct
  type 'a change = {
    replaced : int;
    replacement : (int * 'a) option (* None represents everything kept *);
  }

  module ChangeDesc = struct
    type single_change = unit change
    type t = single_change list

    let length v =
      List.fold_left (fun result { replaced; _ } -> result + replaced) 0 v

    let new_length v =
      List.fold_left
        (fun result { replaced; replacement } ->
          match replacement with
          | None -> result + replaced
          | Some (replacement, _) -> result + replacement)
        0 v

    let empty v =
      match v with [] | [ { replacement = None; _ } ] -> true | _ -> false

    let iter_gaps v ~f =
      List.fold_left
        (fun (posA, posB) { replaced; replacement } ->
          f posA posB replaced;
          match replacement with
          | None -> (posA + replaced, posB + replaced)
          | Some (replacement, _) -> (posA + replaced, posB + replacement))
        (0, 0) v
  end

  module ChangeSet = struct
    type t = { section : ChangeDesc.t; inserted : Text.t list }
    type t = { section : ChangeDesc.t; inserted : Text.t list }

    let length v = ChangeDesc.length v.section

    let fold_changes (changes : 'c change list)
        ~(f :
           'acc -> fromA:int -> toA:int -> fromB:int -> toB:int -> 'c -> 'acc)
        acc =
      List.fold_left (fun acc change -> _) acc changes

    let apply v doc =
      if length v <> Text.length doc then failwith "Not good";
      fold_changes v
        (fun doc (fromA, toA, fromB, _toB, text) -> Text.replace text doc)
        doc

    (* Here is how the JSON is represented:
       - It is an Array
       - Each element of the array is
         - A "kept" section if it is a number.
         -
    *)
    let to_JSON v = List.fold_left (fun json -> _)

    let fromJSON json =
      match json with
      | `Array arr ->
          List.fold_left (fun (sections, inserted) part ->
              let sections, inserted =
                match part with
                | `Int replaced ->
                    ( { ChangeDesc.replaced; replacement = None } :: sections,
                      inserted )
                | `Array [ `Int replaced ] ->
                    let section =
                      { ChangeDesc.replaced; replacement = Some 0 }
                    in
                    (section :: sections, inserted)
                | `Array arr ->
                    List.fold_left
                      (fun (sections, inserted) json ->
                        match json with
                        | `Array [ `Int replaced; `String insert ] -> _
                        | _ -> failwith "non appropriate")
                      (sections, inserted) arr
                | _ -> _
              in
              let inserted = _ in
              (sections, inserted))
      | _ -> failwith "should be an array"
  end

  module ChangeSpec = struct
    type t =
      | Change of { from : int; to_ : int option; insert : string list option }
      | List of t list
      | Set of ChangeSet.t
  end
end

module Text = struct
  type node = ..
  and line = ..
  and t = Node of node | Leaf of line list
end

type change = { from : int; to_ : int option; insert : string option }
(** [from] is the beginning of the change. If [to_] is present, everything
    between [from] and [to_] is removed. If [insert] is present, it is added at
    position [from]. *)

type change_set = change list
