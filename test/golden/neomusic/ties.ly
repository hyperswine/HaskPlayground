\version "2.24.3"
\score {
 \new PianoStaff <<
  \new Staff { \clef treble \time 4/4 \key c \major
    \tempo 4 = 90
    << { r2. <c' e' g'>4~ | <c' e' g'>2 d'2 |  } >>
  }
  \new Staff { \clef bass \time 4/4 \key c \major
    \tempo 4 = 90
    << { r1 | r1 |  } >>
  }
 >>
 \layout { }
}
