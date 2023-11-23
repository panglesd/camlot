module Text = struct
  module Line = struct
    type t = string
  end

  (* type t = Line.t list *)

  type t = string

  let length = String.length
  let append = ( ^ )
  let empty = ""

  let replace t ~from ~to_ ~with_ =
    Format.printf "Calling replace with from:%d to:%d with:%s\n%!" from to_
      with_;
    let initial = String.sub t 0 from in
    let final = String.sub t to_ (String.length t - to_) in
    initial ^ with_ ^ final

  let of_lines l = String.concat "\n" l
  let to_lines l = String.split_on_char '\n' l
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
    type t = Text.t change list

    let length v = ChangeDesc.length v

    (** When `individual` is true, adjacent changes (which are kept separate for
        position mapping) are reported separately. *)
    let fold_changes ?(individual = true) (changes : 'c change list)
        ~(f :
           'acc -> fromA:int -> toA:int -> fromB:int -> toB:int -> 'c -> 'acc)
        acc =
      let rec loop (acc, posA, posB) changes =
        match changes with
        | [] -> acc
        | { replacement = None; replaced } :: q ->
            loop (acc, posA + replaced, posB + replaced) q
        | { replacement = Some (l_r, replacement); replaced } :: q ->
            let toA, toB, q, text =
              if individual then (posA + replaced, posB + l_r, q, replacement)
              else
                let rec collect (toA, toB, text) l =
                  match l with
                  | [] | { replacement = None; _ } :: _ -> (toA, toB, l, text)
                  | { replacement = Some (l_r, replacement); replaced } :: q ->
                      collect
                        (toA + replaced, toB + l_r, Text.append text replacement)
                        q
                in
                collect (posA, posB, Text.empty) changes
            in
            let acc = f acc ~fromA:posA ~toA ~fromB:posB ~toB text in
            loop (acc, toA, toB) q
      in
      loop (acc, 0, 0) changes

    let apply v doc =
      if length v <> Text.length doc then failwith "Not good";
      fold_changes v
        ~f:(fun doc ~fromA ~toA ~fromB ~toB:_ text ->
          Text.replace ~from:fromB ~to_:(fromB + (toA - fromA)) ~with_:text doc)
        doc

    (* Here is how the JSON is represented:
       - It is an Array
       - Each element of the array is
         - A "kept" section if it is a number.
         -
    *)
    let to_JSON v =
      let res =
        List.map
          (function
            | { replaced; replacement = None } -> `Int replaced
            | { replaced; replacement = Some (_, replacement) } ->
                let l = Text.to_lines replacement in
                let l = List.map (fun x -> `String x) l in
                `Array (`Int replaced :: l))
          v
      in
      `Array res

    let fromJSON json =
      match json with
      | `List arr ->
          List.fold_left
            (fun changes -> function
              | `Int replaced -> { replaced; replacement = None } :: changes
              | `List [ `Int replaced ] ->
                  let section = { replaced; replacement = Some (0, "") } in
                  section :: changes
              | `List (`Int replaced :: q) ->
                  let lines =
                    List.map
                      (function
                        | `String s -> s | _ -> failwith "should be a string")
                      q
                  in
                  let text = Text.of_lines lines in
                  let section =
                    { replaced; replacement = Some (Text.length text, text) }
                  in
                  section :: changes
              | _ ->
                  failwith "non appropriate JSON: should be an int or an array")
            [] arr
          |> List.rev
      | _ -> failwith "should be an array"

    let compose csA csB =
      let rec loop acc csA csB =
        match (csA, csB) with
        | [], [] -> acc
        | ({ replaced = 0; _ } as deletion) :: qA, _ ->
            let acc = deletion :: acc in
            loop acc qA csB
        | _, ({ replaced = 0; _ } as deletion) :: qB ->
            let acc = deletion :: acc in
            loop acc csA qB
        | [], _ | _, [] -> failwith "Mismatched change set length"
        | cA :: qA, cB :: qB ->
            let len2 = function Some l -> String.length l | None -> 0 in
            
        | _, _ -> _
      in
      _
  end

  module ChangeSpec = struct
    type t =
      | Change of { from : int; to_ : int option; insert : string list option }
      | List of t list
      | Set of ChangeSet.t
  end
end
