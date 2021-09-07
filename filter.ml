open Lwt
open Cohttp_lwt_unix
open Soup

type webpage = 
  | HomePage of Uri.t
  | ProductPage of Uri.t

type coffee = {
  name          : string;
  description   : string;
  origin        : string list;
  altitude      : string list;
  process       : string list;
  price         : string;
  tasting_notes : string list;
  url           : Uri.t;
}

let sqmile_product_details header nodes =
  List.filter_map (fun node ->
    match node $? "i" with
    | Some i_node -> 
      if leaf_text i_node = Some header then 
        match node $? "b" with
        | Some b_node -> leaf_text b_node
        | None -> None
      else None
    | None -> None
  ) nodes

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

let scrape_product product_page =
  match product_page with
  | HomePage _ -> failwith "Must be ProductPage"
  | ProductPage page ->
    Client.get page >>= fun (_, body) ->
      body |> Cohttp_lwt.Body.to_string >|= fun str ->
      let soup = parse str in

      let name = soup $ "meta[property=\"og:title\"]" |> attribute "content" |> Option.value ~default:"Missing Title" in
      let desc = soup $ "meta[property=\"og:description\"]" |> attribute "content" |> Option.value ~default:"Missing Description" in

      let origin_process_altitude_soups = Soup.flatten (fun s -> s $$ "p") (soup $$ "div[class=\"sqmile-wysiwyg\"]" ) |> to_list in
      let origin = sqmile_product_details "Country" origin_process_altitude_soups in
      let altitude = sqmile_product_details "Altitude" origin_process_altitude_soups in
      let process = sqmile_product_details "Process" origin_process_altitude_soups in

      let price = soup $ "span[itemprop=\"price\"]" |> leaf_text |> require in

      let tasting_note_soup = soup $$ "div[class=\"sqm-product-tasting-notes-pp\"]" |> to_list in
      let tasting_notes = List.filter_map (fun node -> node |> leaf_text) tasting_note_soup

      in

        {
          name=name;
          description=desc;
          origin=origin;
          altitude=altitude;
          process=process;
          price=price;
          tasting_notes=tasting_notes;
          url=page
        }

let () =
  let shop = HomePage (Uri.of_string "https://shop.squaremilecoffee.com") in
  Lwt_main.run (get_product_links shop >>= 
    fun pp_list ->
      Lwt.all (List.map scrape_product pp_list) >|= 
        List.iter (fun x -> print_endline (x.url |> Uri.to_string))
  )
