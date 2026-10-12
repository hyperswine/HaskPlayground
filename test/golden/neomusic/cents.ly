\version "2.24.3"
\score {
 \new PianoStaff <<
  \new Staff { \clef treble \time 4/4 \key c \major
    \tempo 4 = 72
    << { <c' e' g'>1_\markup \small \override #'(baseline-skip . 1.7) \center-column { "7" "4" "0" } | c'1_\markup \small "0" |  } \\ { s1 | e'1^\markup \small "-14¢"_\markup \small "4" |  } \\ { s1 | g'1^\markup \small "+2¢"_\markup \small "7" |  } >>
  }
  \new Staff { \clef bass \time 4/4 \key c \major
    \tempo 4 = 72
    << { c1_\markup \small "-12" | c1_\markup \small "-12" |  } >>
  }
 >>
 \header { piece = \markup \small "Numbers under the notes are Neo steps: 0 = C4" }
 \layout { }
}
