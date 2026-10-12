\version "2.24.3"
\score {
 \new PianoStaff <<
  \new Staff { \clef treble \time 4/4 \key c \major
    \tempo 4 = 120
    << { c'8_\markup \small "0" d'8_\markup \small "2" e'8_\markup \small "4" f'8_\markup \small "5" g'8_\markup \small "7" a'8_\markup \small "9" b'8_\markup \small "11" c''8_\markup \small "12" |  } >>
  }
  \new Staff { \clef bass \time 4/4 \key c \major
    \tempo 4 = 120
    << { r1 |  } >>
  }
 >>
 \header { piece = \markup \small "Numbers under the notes are Neo steps: 0 = C4" }
 \layout { }
}
