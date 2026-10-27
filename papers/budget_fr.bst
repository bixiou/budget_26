%% budget_fr.bst -- natbib-compatible author-year style following the
%% journal's "Bibliography presentation": "Name F.G. [year], Title, Journal,
%% vol(num), pages." Used by budget.tex.

ENTRY
  { address author booktitle editor howpublished institution journal key
    note number pages publisher school title url volume year }
  {}
  { label sort.label }

STRINGS { s t }
INTEGERS { nameptr namesleft numnames }

FUNCTION {not} { { #0 } { #1 } if$ }
FUNCTION {and} { 'skip$ { pop$ #0 } if$ }
FUNCTION {or} { { pop$ #1 } 'skip$ if$ }

FUNCTION {field.or.null} { duplicate$ empty$ { pop$ "" } 'skip$ if$ }

FUNCTION {emphasize} { duplicate$ empty$ { pop$ "" } { "\emph{" swap$ * "}" * } if$ }

% Appends ", <field>" to the string below it when <field> is not empty
FUNCTION {add.comma} { duplicate$ empty$ { pop$ } { ", " swap$ * * } if$ }

FUNCTION {format.names}
{ 's :=
  #1 'nameptr :=
  s num.names$ 'numnames :=
  numnames 'namesleft :=
    { namesleft #0 > }
    { s nameptr "{vv~}{ll}{ f{.}.}{, jj}" format.name$ 't :=
      nameptr #1 >
        { t "others" = { " et al." * } { ", " * t * } if$ }
        { t }
      if$
      nameptr #1 + 'nameptr :=
      namesleft #1 - 'namesleft :=
    }
  while$
}

FUNCTION {format.authors}
{ author empty$
    { editor empty$ { key field.or.null } { editor format.names " (ed.)" * } if$ }
    { author format.names }
  if$
}

FUNCTION {format.lab.names}
{ 's :=
  s num.names$ 'numnames :=
  s #1 "{vv~}{ll}" format.name$
  numnames #2 >
    { " et~al." * }
    { numnames #2 =
        { s #2 "{ff }{vv }{ll}{ jj}" format.name$ "others" =
            { " et~al." * }
            { " et " * s #2 "{vv~}{ll}" format.name$ * }
          if$ }
        'skip$
      if$ }
  if$
}

FUNCTION {author.key.label}
{ author empty$
    { editor empty$ { key empty$ { cite$ #1 #3 substring$ } 'key if$ } { editor format.lab.names } if$ }
    { author format.lab.names }
  if$
}

FUNCTION {format.url} { url empty$ { "" } { "\url{" url * "}" * } if$ }

FUNCTION {output.bibitem}
{ "\bibitem[{" label * "(" * year field.or.null * ")}]{" * cite$ * "}" * write$ newline$
  format.authors " [" * year field.or.null * "]" *
}

FUNCTION {fin.entry} { "." * write$ newline$ newline$ }

FUNCTION {format.vol.num.pages}
{ volume field.or.null
  number empty$ 'skip$ { "(" number * ")" * * } if$
  pages empty$
    'skip$
    { duplicate$ empty$ { pop$ pages } { ", " * pages * } if$ }
  if$
}

FUNCTION {article}
{ output.bibitem
  title "t" change.case$ add.comma
  journal emphasize add.comma
  format.vol.num.pages add.comma
  fin.entry
}

FUNCTION {book}
{ output.bibitem
  title "t" change.case$ emphasize add.comma
  address add.comma
  publisher add.comma
  fin.entry
}

FUNCTION {incollection}
{ output.bibitem
  title "t" change.case$ add.comma
  editor empty$
    { booktitle emphasize empty$ 'skip$ { ", in: " * booktitle emphasize * } if$ }
    { ", in: " * editor format.names *
      editor num.names$ #1 > { " (eds.)" } { " (ed.)" } if$ *
      booktitle emphasize add.comma }
  if$
  address add.comma
  publisher add.comma
  pages add.comma
  fin.entry
}

FUNCTION {techreport}
{ output.bibitem
  title "t" change.case$ add.comma
  institution add.comma
  number add.comma
  format.url add.comma
  fin.entry
}

FUNCTION {misc}
{ output.bibitem
  title "t" change.case$ add.comma
  howpublished empty$ { publisher } { howpublished } if$ add.comma
  institution add.comma
  school add.comma
  note add.comma
  format.url add.comma
  fin.entry
}

FUNCTION {inbook} { incollection }
FUNCTION {inproceedings} { incollection }
FUNCTION {conference} { incollection }
FUNCTION {proceedings} { book }
FUNCTION {booklet} { misc }
FUNCTION {manual} { misc }
FUNCTION {mastersthesis} { misc }
FUNCTION {phdthesis} { misc }
FUNCTION {unpublished} { misc }
FUNCTION {default.type} { misc }

READ

FUNCTION {sortify} { purify$ "l" change.case$ }

FUNCTION {presort}
{ author.key.label 'label :=
  author empty$ { key field.or.null } { author format.names } if$ sortify
  "    " * year field.or.null * "    " * title field.or.null sortify *
  #1 entry.max$ substring$ 'sort.key$ :=
}

ITERATE {presort}

SORT

FUNCTION {begin.bib}
{
  preamble$ empty$ 'skip$ { preamble$ write$ newline$ } if$
  "\providecommand{\url}[1]{\texttt{#1}}" write$ newline$
  "\begin{thebibliography}{99}" write$ newline$
}

EXECUTE {begin.bib}

ITERATE {call.type$}

FUNCTION {end.bib} { newline$ "\end{thebibliography}" write$ newline$ }

EXECUTE {end.bib}
