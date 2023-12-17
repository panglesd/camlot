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
  type 'a change = Keep of int | Replace of int * (int * 'a)

  let print l =
    List.iter
      (function
        | Keep i -> Format.printf "Keep %d ;%!" i
        | Replace (i, (_, s)) -> Format.printf "Replace %d by '%s'%!" i s)
      l;
    print_newline ()

  let len (Keep i | Replace (i, _)) = i

  module ChangeDesc = struct
    type single_change = unit change
    type t = single_change list

    let length v = List.fold_left (fun result c -> result + len c) 0 v
    let new_length v = List.fold_left (fun result c -> result + len c) 0 v
    let empty v = match v with [] | [ Keep _ ] -> true | _ -> false

    let iter_gaps v ~f =
      List.fold_left
        (fun (posA, posB) c ->
          f posA posB (len c);
          match c with
          | Keep i -> (posA + i, posB + i)
          | Replace (i, (r, _)) -> (posA + i, posB + r))
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
        | Keep i :: q -> loop (acc, posA + i, posB + i) q
        | Replace (replaced, (l_r, replacement)) :: q ->
            let toA, toB, q, text =
              if individual then (posA + replaced, posB + l_r, q, replacement)
              else
                let rec collect (toA, toB, text) l =
                  match l with
                  | [] | Keep _ :: _ -> (toA, toB, l, text)
                  | Replace (i, (l_r, r)) :: q ->
                      collect (toA + i, toB + l_r, Text.append text r) q
                in
                collect (posA, posB, Text.empty) changes
            in
            let acc = f acc ~fromA:posA ~toA ~fromB:posB ~toB text in
            loop (acc, toA, toB) q
      in
      loop (acc, 0, 0) changes

    let apply v doc =
      if length v <> Text.length doc then (
        Format.printf "Character is: %c %c %c %c%!" doc.[41] doc.[42] doc.[43]
          doc.[44];
        Format.printf "\"%s\"\n%!" doc;
        print v;
        failwith
          (Format.sprintf
             "Length of document and change do not correspond: %d vs %d"
             (length v) (Text.length doc)));
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
            | Keep replaced -> `Int replaced
            | Replace (replaced, (_, replacement)) ->
                let l = Text.to_lines replacement in
                let l = List.map (fun x -> `String x) l in
                `List (`Int replaced :: l))
          v
      in
      `List res

    let fromJSON json =
      match json with
      | `List arr ->
          List.fold_left
            (fun changes -> function
              | `Int replaced -> Keep replaced :: changes
              | `List [ `Int replaced ] ->
                  let section = Replace (replaced, (0, "")) in
                  section :: changes
              | `List (`Int replaced :: q) ->
                  let lines =
                    List.map
                      (function
                        | `String s -> s | _ -> failwith "should be a string")
                      q
                  in
                  let text = Text.of_lines lines in
                  let section = Replace (replaced, (Text.length text, text)) in
                  section :: changes
              | json ->
                  let s =
                    Format.sprintf
                      "non appropriate JSON: should be an int or an array. \
                       Instead, got %s"
                      (Yojson.Safe.to_string json)
                  in
                  failwith s)
            [] arr
          |> List.rev
      | _ -> failwith "should be an array"

    (* let compose csA csB = *)
    (*   let consume ~consumed ~kept = *)
    (*     if consumed = kept then [] else [ Keep (kept - consumed) ] *)
    (*   in *)
    (*   let split c n = *)
    (*     assert (n <= len c); *)
    (*     match c with *)
    (*     | Keep a -> (Keep n, Keep (n - a)) *)
    (*     | Replace (a, (l, s)) -> (Replace (n, (l, s)), Replace (a - n, (0, ""))) *)
    (*   in *)
    (*   let rec loop acc csA csB = *)
    (*     match (csA, csB) with *)
    (*     (\* Finished *\) *)
    (*     | [], [] -> acc *)
    (*     (\* Should reach [] at the same time *\) *)
    (*     | [], _ | _, [] -> failwith "Mismatched change set length" *)
    (*     (\* Skip emptied modification *\) *)
    (*     | (Keep 0 | Replace (0, (0, ""))) :: csA, csB *)
    (*     | csA, (Keep 0 | Replace (0, (0, ""))) :: csB -> *)
    (*         loop acc csA csB *)
    (*     (\* Handle deletion *\) *)
    (*     | (Replace (0, _) as deletion) :: qA, _ -> *)
    (*         let acc = deletion :: acc in *)
    (*         loop acc qA csB *)
    (*     | _, (Replace (0, _) as deletion) :: qB -> *)
    (*         let acc = deletion :: acc in *)
    (*         loop acc csA qB *)
    (*     (\* A is keeping *\) *)
    (*     | (Keep a as ca) :: qA, cb :: qB -> *)
    (*         let len = len cb in *)
    (*         if len <= a then *)
    (*           let _, remaining = split ca len in *)
    (*           loop (cb :: acc) (remaining :: qA) qB *)
    (*         else *)
    (*           let handled, unhandled = split cb a in *)
    (*           loop (handled :: acc) qA (unhandled :: qB) *)
    (*     (\* A is modifying *\) *)
    (*     | Replace (a, (l, r)) :: qA, cb :: qB -> *)
    (*        if l > len cb then *)
    (*          let acc = *)
    (*     | Keep a :: qA, cb :: qB (\* when a < len cb *\) -> *)
    (*         let consumed, remaining = split cb a in *)
    (*         let acc = _ :: acc in *)
    (*         let qB = _ :: qB in *)
    (*         loop acc qA qB *)
    (*     | Keep a :: qA, Keep b :: qB -> *)
    (*         if a < b then loop (Keep a :: acc) qA (Keep (b - a) :: qB) *)
    (*         else if b < a then loop (Keep b :: acc) (Keep (a - b) :: qA) qB *)
    (*         else loop (Keep a :: acc) qA qB *)
    (*     | Keep a :: qA, Replace (b, (len, rep)) :: qB -> *)
    (*         if a > b then *)
    (*           loop (Replace (b, (len, rep)) :: acc) (Keep (a - b) :: qA) qB *)
    (*         else if a < b then loop (Keep b :: acc) (Keep (a - b) :: qA) qB *)
    (*         else loop (Keep a :: acc) qA qB *)
    (*     | cA :: qA, cB :: qB -> *)
    (*         let len2 = function Some l -> String.length l | None -> 0 in *)
    (*         _ *)
    (*     | _, _ -> _ *)
    (*   in *)
    (*   _ *)
  end

  module ChangeSpec = struct
    type t =
      | Change of { from : int; to_ : int option; insert : string list option }
      | List of t list
      | Set of ChangeSet.t
  end
end
