
let () =
  let iface = object
    inherit Calculator.iface
    method ping =
      Printf.printf "server: got ping\n%!";
      print_endline "ping"

    method add i j =
      Printf.printf "server: got add\n%!";
      match i, j with
      | Some i, Some j -> Int32.add i j
      | _ -> failwith "missing arg"

    method calculate _logid op =
      Printf.printf "server: got calculate %ld\n%!"
        (match _logid with None -> 0l| Some i->i);
      match op with
      | None -> failwith "no work provided"
      | Some o ->
        let open Tutorial_types in
        (match o#grab_op with
         | Operation.ADD -> Int32.add o#grab_num1 o#grab_num2
         | Operation.MULTIPLY -> Int32.mul o#grab_num1 o#grab_num2
         | Operation.DIVIDE -> Int32.div o#grab_num1 o#grab_num2
         | Operation.SUBTRACT -> Int32.sub o#grab_num1 o#grab_num2
        )
  end
  in
  let processor = new Calculator.processor iface in
  let server =
    new TThreadedServer.t processor (new TServerSocket.t 2526)
      (object method getTransport t = t end)
      (new TBinaryProtocol.factory)
      (new TBinaryProtocol.factory)
  in
  server#serve
