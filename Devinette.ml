open Random

let () =
  Random.self_init ();
  let nombre_a_deviner = 1 + Random.int 100 in
  let rec devine_nombre tentative =
    print_string "Devinez le nombre entre 1 et 100 : ";
    flush stdout;
    try
      let guess = read_int () in
      if guess < 1 || guess > 100 then
        begin
          print_endline "Veuillez entrer un nombre entre 1 et 100.";
          devine_nombre tentative
        end
      else if guess < nombre_a_deviner then
        begin
          print_endline "Plus grand !";
          devine_nombre (tentative + 1)
        end
      else if guess > nombre_a_deviner then
        begin
          print_endline "Plus petit !";
          devine_nombre (tentative + 1)
        end
      else
        Printf.printf "Bravo, vous avez deviné le nombre en %d tentatives !\n" (tentative + 1)
    with
    | Failure _ ->
        print_endline "Veuillez entrer un nombre valide.";
        devine_nombre tentative
  in
  print_endline "Bienvenue dans le jeu de devinette !";
  devine_nombre 0
