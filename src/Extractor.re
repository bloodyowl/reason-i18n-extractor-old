module Parser = {
  open Reason_toolchain.RE;
  open Migrate_parsetree;
  open Ast_404;
  open Ast_mapper;
  open Asttypes;
  open Parsetree;
  open Longident;

  let parse = fileContents => {
    let translations = ref(Belt.Set.String.empty);
    let mapper = {
      ...default_mapper,
      expr: (mapper, item) =>
        switch (item) {
        | {
            pexp_desc:
              Pexp_apply(
                {pexp_desc: Pexp_ident({txt: Ldot(Lident("T"), "__")})},
                [
                  (
                    Nolabel,
                    {
                      pexp_desc:
                        Pexp_constant(Pconst_string(translation, None)),
                    },
                  ),
                ],
              ),
          } as x =>
          translations := (translations^)->Belt.Set.String.add(translation);
          x;
        | anythingElse => default_mapper.expr(mapper, anythingElse)
        },
    };
    let astAndComments = fileContents->implementation_with_comments;
    let (ast, _) = astAndComments;
    mapper.structure(mapper, ast)->ignore;
    print_implementation_with_comments(Format.str_formatter, astAndComments);
    translations^;
  };
};

let read = () => {
  let set = ref(Belt.Set.String.empty);
  let rec read = () =>
    try (
      {
        set := (set^)->Belt.Set.String.add(stdin->input_line);
        read();
      }
    ) {
    | End_of_file => ()
    };
  read();
  set^;
};

let main = () =>
  switch (Sys.argv) {
  | [|_, "--help"|] =>
    print_endline("Reason i18n extractor");
    print_endline(
      "Finds occurences of `T.__(translation)` in the list of files you feed in stdin",
    );
    print_endline(
      "Usage: find src/**/*.re | write-translations path/to/translations.json",
    );
  | [|_, destinationFile|] =>
    let oldTranslations =
      try (
        switch (Yojson.Basic.from_file(destinationFile)) {
        | `Assoc(list) =>
          let rec aggregate = (translations, l) =>
            switch (l) {
            | [] => translations
            | [(key, `String(value)), ...tl] =>
              aggregate(translations->Belt.Map.String.set(key, value), tl)
            | _ => failwith("Malformed destination file")
            };
          aggregate(Belt.Map.String.empty, list);
        | _ => failwith("Malformed destination file")
        }
      ) {
      | _ =>
        print_endline("Destination file does not exit");
        Belt.Map.String.empty;
      };
    let translations =
      read()
      ->Belt.Set.String.reduce(
          Belt.Set.String.empty,
          (set, file) => {
            let ic = open_in_bin(file);
            print_endline("Parsing " ++ file);
            let lexbuf = Lexing.from_channel(ic);
            let found = Parser.parse(lexbuf);
            close_in(ic);
            set->Belt.Set.String.union(found);
          },
        );

    let json =
      `Assoc(
        translations
        ->Belt.Set.String.toList
        ->Belt.List.map(key => {
            let value =
              oldTranslations
              ->Belt.Map.String.get(key)
              ->Belt.Option.getWithDefault(key);
            (key, `String(value));
          }),
      );

    let oc = open_out(destinationFile);
    oc->output_string(Yojson.pretty_to_string(json));
    close_out(oc);
  | _ => failwith("No destination file provided")
  };

main();
