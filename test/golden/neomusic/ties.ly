\version "2.24.3"
\score {
 \new PianoStaff <<
  \new Staff { \clef treble \time 4/4 \key c \major
    \tempo 4 = 90
    << { r2. <c' e' g'>4_\markup \small \override #'(baseline-skip . 1.7) \center-column { "7" "4" "0" }~ | <c' e' g'>2 d'2_\markup \small "2" |  } >>
  }
  \new Staff { \clef bass \time 4/4 \key c \major
    \tempo 4 = 90
    << { r1 | r1 |  } >>
  }
 >>
 \header { piece = \markup \small "Numbers under the notes are Neo steps: 0 = C4" }
 \layout { }
}
