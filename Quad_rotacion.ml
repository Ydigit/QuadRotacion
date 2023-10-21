(** @file quadtree.ml
    @brief Implementação de uma Quadtree em OCaml.
    
    Este arquivo contém a implementação de uma Quadtree em OCaml, onde uma imagem binária é representada por uma estrutura de dados recursiva. A Quadtree pode ser construída a partir de uma matriz de pixels e oferece funcionalidades como inversão, rotação, contagem de folhas e nós, e conversão de volta para matriz.

    Para utilizar a Quadtree, é necessário fornecer uma matriz de pixels binários. Os pixels pretos são representados por 1 e os pixels brancos são representados por 0.

    Exemplo de uso:

    ```
    let matrix = [|[|1; 1; 0; 0|];
                   [|1; 0; 0; 0|];
                   [|0; 0; 1; 1|];
                   [|0; 0; 1; 0|]|]
    in
    let quadtree = matrix_to_quadtree matrix in
    let inverted_quadtree = invert_tree quadtree in
    let inverted_matrix = tree_to_matrix (make_matrix 4 4 0) inverted_quadtree in
    print_matrix_p1 inverted_matrix
    ```

    Este código cria uma matriz de pixels binários, converte-a em uma Quadtree, inverte a Quadtree e, em seguida, converte a Quadtree invertida de volta para uma matriz. A função `print_matrix_p1` imprime a matriz resultante.

    @note Este código requer as bibliotecas `String`, `Printf` e `Array` para funcionar corretamente.
*)

open String
open Printf
open Array


(** @brief Definição do tipo de cor.
    @details O tipo `color` representa as cores utilizadas na Quadtree. Pode ser preto (B) ou branco (W).
*)
type color = B | W


(** @brief Definição da estrutura da Quadtree.
    @details A Quadtree é uma estrutura de dados recursiva que representa uma imagem binária. Pode ser um pixel único (L) ou um nó interno (N) contendo 4 subárvores.
*)
type quadtree = L of color | N of color * quadtree * quadtree * quadtree * quadtree


(** @brief Função auxiliar para inverter uma cor.
    @param color A cor a ser invertida.
    @return A cor invertida.
*)
let invert_color = function
  | B -> W
  | W -> B


(** @brief Função para converter uma matriz em uma Quadtree.
    @param matrix A matriz de pixels binários.
    @return A Quadtree correspondente à matriz.
*)
let rec matrix_to_quadtree matrix =
  let larg = length matrix.(0) in
  let alt = length matrix in
  let rec matrix_to_quadtree_two x y w h =
    if w = 1 && h = 1 then
      let color = if matrix.(y).(x) = 1 then B else W in
      L color
    else
      let larg_two = w / 2 in
      let alt_two = h / 2 in
      let nw = matrix_to_quadtree_two x y larg_two alt_two in
      let ne = matrix_to_quadtree_two (x + larg_two) y larg_two alt_two in
      let sw = matrix_to_quadtree_two x (y + alt_two) larg_two alt_two in
      let se = matrix_to_quadtree_two (x + larg_two) (y + alt_two) larg_two alt_two in
      match nw, ne, sw, se with
      | L color, L color', L color'', L color''' ->
        if color = color' && color = color'' && color = color''' then
          L color
        else
          N (W, nw, ne, sw, se)
      | _ ->
        N (W, nw, ne, sw, se)
  in
  matrix_to_quadtree_two 0 0 larg alt


(** @brief Função para rotacionar uma Quadtree para a esquerda.
    @param quadtree A Quadtree a ser rotacionada.
    @return A Quadtree rotacionada para a esquerda.
*)
let rec rotate_left = function
  | L color -> L color
  | N (color, nw, ne, sw, se) ->
    let rotated_nw = rotate_left ne in
    let rotated_ne = rotate_left se in
    let rotated_sw = rotate_left nw in
    let rotated_se = rotate_left sw in
    N (color, rotated_nw, rotated_ne, rotated_sw, rotated_se)


(** @brief Função para rotacionar uma Quadtree para a direita.
    @param quadtree A Quadtree a ser rotacionada.
    @return A Quadtree rotacionada para a direita.
*)
let rec rotate_right = function
  | L color -> L color
  | N (color, nw, ne, sw, se) ->
    let rotated_nw = rotate_right se in
    let rotated_ne = rotate_right sw in
    let rotated_sw = rotate_right ne in
    let rotated_se = rotate_right nw in
    N (color, rotated_nw, rotated_ne, rotated_sw, rotated_se)


(** @brief Função para inverter uma Quadtree.
    @param quadtree A Quadtree a ser invertida.
    @return A Quadtree invertida.
*)
let rec invert_tree = function
  | L color -> L (invert_color color)
  | N (color, nw, ne, sw, se) ->
    N (invert_color color, invert_tree nw, invert_tree ne, invert_tree sw, invert_tree se)


(** @brief Função para contar o número de folhas em uma Quadtree.
    @param quadtree A Quadtree a ser contada.
    @return O número de folhas na Quadtree.
*)
let rec count_leaves = function
  | L _ -> 1
  | N (_, nw, ne, sw, se) -> count_leaves nw + count_leaves ne + count_leaves sw + count_leaves se


(** @brief Função para contar o número de nós em uma Quadtree.
    @param quadtree A Quadtree a ser contada.
    @return O número de nós na Quadtree.
*)
let rec count_nodes = function
  | L _ -> 0
  | N (_, nw, ne, sw, se) -> 1 + count_nodes nw + count_nodes ne + count_nodes sw + count_nodes se


(** @brief Função para calcular o comprimento mínimo do ramo em uma Quadtree.
    @param quadtree A Quadtree para calcular o comprimento mínimo do ramo.
    @return O comprimento mínimo do ramo na Quadtree.
*)
let rec min_branch_length = function
  | L _ -> 0
  | N (_, nw, ne, sw, se) ->
    let lengths = [min_branch_length nw; min_branch_length ne; min_branch_length sw; min_branch_length se] in
    1 + List.fold_left min (List.hd lengths) (List.tl lengths)


(** @brief Função para calcular o comprimento máximo do ramo em uma Quadtree.
    @param quadtree A Quadtree para calcular o comprimento máximo do ramo.
    @return O comprimento máximo do ramo na Quadtree.
*)
let rec max_branch_length = function
  | L _ -> 0
  | N (_, nw, ne, sw, se) ->
    let lengths = [max_branch_length nw; max_branch_length ne; max_branch_length sw; max_branch_length se] in
    1 + List.fold_left max (List.hd lengths) (List.tl lengths)


(** @brief Função para converter uma Quadtree em uma matriz.
    @param matrix A matriz para armazenar os pixels da Quadtree.
    @param quadtree A Quadtree a ser convertida.
    @return A matriz resultante da conversão da Quadtree.
*)
let rec tree_to_matrix matrix quadtree =
  let larg = length matrix.(0) in
  let alt = length matrix in
  let rec tree_to_matrix_two x y w h = function
    | L color ->
      for i = y to y + h - 1 do
        for j = x to x + w - 1 do
          matrix.(i).(j) <- if color = B then 1 else 0
        done
      done
    | N (color, nw, ne, sw, se) ->
      let larg_two = w / 2 in
      let alt_two = h / 2 in
      tree_to_matrix_two x y larg_two alt_two nw;
      tree_to_matrix_two (x + larg_two) y larg_two alt_two ne;
      tree_to_matrix_two x (y + alt_two) larg_two alt_two sw;
      tree_to_matrix_two (x + larg_two) (y + alt_two) larg_two alt_two se
  in
  tree_to_matrix_two 0 0 larg alt quadtree;
  matrix


(** @brief Função para imprimir uma matriz no formato P1 (imagem binária).
    @param matrix A matriz de pixels binários.
*)
let print_matrix_p1 matrix =
  let larg = length matrix.(0) in
  let alt = length matrix in
  print_string "P1";
  print_newline ();
  print_int larg;
  print_string " ";
  print_int alt;
  print_newline ();
  for i = 0 to alt - 1 do
    for j = 0 to larg - 1 do
      print_int matrix.(i).(j);
      if j < larg - 1 then print_string " "
    done;
    print_newline ()
  done


(** @brief Função para ler uma matriz de pixels binários a partir da entrada.
    @return A matriz lida da entrada.
    @raise Failure se a entrada não estiver no formato esperado.
*)
let input () =
  let ppm_type = read_line () in
  match ppm_type with
  | "P1" ->
    let ppm_type = read_line () in
    let larg_alt = ppm_type |> split_on_char ' ' |> List.map int_of_string in
    let larg = List.hd larg_alt in
    let alt = List.hd (List.tl larg_alt) in
    let matrix = make_matrix alt larg 0 in
    for i = 0 to alt - 1 do
      let line = read_line () in
      let pixel_values = split_on_char ' ' line |> List.map int_of_string in
      for j = 0 to larg - 1 do
        matrix.(i).(j) <- List.hd pixel_values;
      done;
    done;
    matrix
  | _ -> failwith "Invalid PPM type"


(** @brief Função para escrever uma matriz de pixels binários na saída.
    @param matrix A matriz de pixels binários.
*)
let output matrix =
  print_string "P1";
  print_newline ();
  print_int (length matrix.(0));
  print_string " ";
  print_int (length matrix);
  print_newline ();
  for i = 0 to length matrix - 1 do
    for j = 0 to length matrix.(0) - 1 do
      print_int matrix.(i).(j);
      print_string " "
    done;
    print_newline ()
  done
