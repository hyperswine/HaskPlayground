\version "2.24.3"
\score {
 \new PianoStaff <<
  \new Staff { \clef treble \time 4/4 \key c \major
    \tempo 4 = 100
    << { c'4_\markup \small "0" d'4_\markup \small "2" e'4_\markup \small "4" f'4_\markup \small "5" | g'4_\markup \small "7" a'4_\markup \small "9" b'4_\markup \small "11" c''4_\markup \small "12" |  } \\ { c''2_\markup \small "12" b'2_\markup \small "11" | a'2_\markup \small "9" g'2_\markup \small "7" |  } >>
  }
  \new Staff { \clef bass \time 4/4 \key c \major
    \tempo 4 = 100
    << { r1 | r1 |  } >>
  }
 >>
 \header { piece = \markup \small "Numbers under the notes are Neo steps: 0 = C4" }
 \layout { }
}
