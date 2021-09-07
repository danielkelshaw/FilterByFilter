open Lwt
open Cohttp_lwt_unix
open Soup

type webpage = 
  | HomePage of Uri.t
  | ProductPage of Uri.t

let get_product_links homepage =
  match homepage with
  | HomePage page ->
    Client.get page >>= fun (_, body) ->
      body |> Cohttp_lwt.Body.to_string >|= fun str ->
        let soup = parse str in
        let coffee_nodes = soup $$ "article[data-show=\"filter\"]" |> to_list in
        let url_strings = List.map (fun node -> node $ "meta[itemprop=\"url\"]" |> attribute "content") coffee_nodes
        in
          List.filter_map (Option.map (fun x -> ProductPage (Uri.of_string x))) url_strings
  | ProductPage _ -> failwith "Must be HomePage"

let () =
  let shop = HomePage (Uri.of_string "https://shop.squaremilecoffee.com") in
  Lwt_main.run (get_product_links shop >|= fun uri_list ->
    List.iter (fun uri -> 
      match uri with
      | HomePage _ -> failwith "Cant process HomePage here..."
      | ProductPage x -> Uri.to_string x |> print_endline
      ) uri_list
  )
